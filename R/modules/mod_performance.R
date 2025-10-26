performance_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "performance",
    h2("2. Model Performance Evaluation 📊"),

    # --- New User Guide Box ---
    box(
      title = "Instructions",
      status = NULL,
      solidHeader = FALSE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      p(
        strong("Prerequisite:"), " A model must be trained in the 'Model Specification' tab before computing performance.",
        br(),
        strong("Metrics:"), " Performance metrics are calculated on the training data. Classification uses AUC (via ROC Curve), Regression uses RMSE and R-squared (via Observed vs Predicted plot), and Survival uses the C-index and a Risk Score plot.",
        br(),
        strong("Survival Plots:"), " For Survival models, you can switch between viewing the raw **Linear Predictor** (Log-Hazard proxy) or the exponential **Hazard Ratio** (Risk Score)."
      )
    ),
    # -------------------------
    fluidRow(
      box(
        title = "Performance Summary",
        status = "primary",
        solidHeader = TRUE,
        width = 12,

        # --- Always visible: Compute button ---
        actionButton(ns("compute_perf"), "Compute Performance", icon = icon("calculator"), class = "btn-primary"),
        br(), br(),

        # --- Conditional Output Container ---
        # The content inside this UI will only be generated and displayed
        # after the 'compute_perf' button is clicked and the server runs renderUI.
        uiOutput(ns("performance_results_ui"))
      )
    )
  )
}

performance_server <- function(id, app_data_rv) {
  moduleServer(id, function(input, output, session) {

    metrics_rv <- reactiveVal(NULL)
    plots_rv <- reactiveVal(list(ggplots = NULL, preds = NULL, c_index = NULL))

    observeEvent(input$compute_perf, {
      req(app_data_rv$fit, app_data_rv$data, app_data_rv$outcome_name, app_data_rv$task)

      showNotification("Evaluating model performance...", type = "message", duration = 2)

      data <- app_data_rv$data
      outcome <- app_data_rv$outcome_name
      task <- app_data_rv$task
      fit_obj <- app_data_rv$fit
      preds <- NULL
      truth <- NULL
      c_index_val <- NA

      # --- 1. CONSOLIDATED MODEL ENGINE IDENTIFICATION ---
      model_engine <- "other"

      if (inherits(fit_obj, "workflow")) {
        model_engine <- fit_obj$fit$actions$model$spec$engine
      } else if (inherits(fit_obj, "model_fit")) {
        model_engine <- fit_obj$spec$engine
      } else if (inherits(fit_obj, "ranger")) {
        model_engine <- "ranger_base" # Distinguish base ranger from parsnip ranger
      } else if (inherits(fit_obj, "aorsf")) {
        model_engine <- "aorsf"
      }
      model_engine <- as.character(model_engine)

      # --- 2. PREDICTION LOGIC ---

      # --- Base RANGER Models ---
      if (model_engine == "ranger_base") {
        if (task == "Survival") {
          ranger_preds <- predict(fit_obj, data = data)
          preds <- -log(ranger_preds$survival[, 1])
          truth <- survival::Surv(data[[outcome]], data[["status"]])
        } else {
          ranger_preds <- predict(fit_obj, data = data)
          preds <- ranger_preds$predictions
          truth <- data[[outcome]]
        }
        # --- TIDYMODELS MODELS (Main Path) ---
      } else {
        # Prepare data for prediction
        data_for_pred <- data %>%
          dplyr::select(-dplyr::any_of(app_data_rv$outcome_name), -dplyr::any_of("status"))

        preds_tbl <- tryCatch({
          if (task == "Survival") {
            if (model_engine %in% c("ranger", "aorsf")) {
              predict(fit_obj, data_for_pred, type = "survival", eval_time = 1)
            } else {
              predict(fit_obj, data_for_pred, type = "linear_pred")
            }
          } else {
            predict(fit_obj, data_for_pred)
          }
        }, error = function(e) { showNotification(paste("Prediction failed:", e$message), type = "error"); NULL })

        req(preds_tbl)
        truth <- data[[outcome]]

        # --- 3. EXTRACTION LOGIC (Survival) ---
        if (task == "Classification (Binary)") {
          preds <- as.numeric(preds_tbl[[grep("^\\.pred_", names(preds_tbl), value = TRUE)[1]]])
        } else if (task == "Regression (Continuous)") {
          preds <- preds_tbl$.pred
        } else if (task == "Survival") {
          if (model_engine %in% c("ranger", "aorsf")) {

            if (".pred" %in% names(preds_tbl) && !is.null(preds_tbl$.pred[[1]])) {

              # Extract the .pred_survival column from the .pred list column
              survival_vals <- sapply(preds_tbl$.pred, function(x) x$.pred_survival)

              if (is.numeric(survival_vals) && length(survival_vals) == nrow(data)) {
                preds <- -log(survival_vals)
              } else {
                showNotification("Survival column extraction failed within .pred list. Inner column missing.", type = "error")
                return(NULL)
              }
            } else {
              showNotification("Survival prediction column (.pred) is missing or malformed.", type = "error")
              return(NULL)
            }
          } else {
            preds <- preds_tbl$.pred_linear_pred
            if (is.null(preds)) { preds <- preds_tbl$.pred }
          }
          truth <- survival::Surv(data[[outcome]], data[["status"]])
        }
      }

      req(preds)

      # --- METRICS AND PLOTS ---
      metric_tbl <- tibble::tibble()
      ggplots_list <- list()

      if (task == "Classification (Binary)") {
        df <- tibble(truth = factor(truth), pred = as.numeric(preds))
        auc_val <- yardstick::roc_auc(df, truth, pred)$.estimate
        metric_tbl <- tibble(Metric = "AUC", Value = round(auc_val, 4))
        roc_obj <- yardstick::roc_curve(df, truth, pred)
        p <- ggplot(roc_obj, aes(x = 1 - specificity, y = sensitivity)) +
          geom_path(color = "steelblue", linewidth = 1.1) +
          geom_abline(linetype = "dashed") +
          coord_equal() +
          theme_minimal(base_size = 14) +
          labs(title = "ROC Curve", subtitle = paste0("AUC = ", round(auc_val, 3)))
        ggplots_list$main_plot <- p
      } else if (task == "Regression (Continuous)") {
        df <- tibble(truth = truth, pred = preds)
        rmse_val <- yardstick::rmse(df, truth = truth, estimate = pred)[[3]]
        r2_val <- yardstick::rsq(df, truth = truth, estimate = pred)[[3]]
        metric_tbl <- tibble(Metric = c("RMSE", "R²"), Value = c(round(rmse_val, 4), round(r2_val, 4)))
        p <- ggplot(df, aes(x = truth, y = pred)) +
          geom_point(alpha = 0.6, color = "steelblue") +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
          theme_minimal(base_size = 14) +
          labs(
            title = "Observed vs Predicted",
            subtitle = paste0("RMSE = ", round(rmse_val, 3), " | R² = ", round(r2_val, 3)),
            x = "Observed", y = "Predicted"
          )
        ggplots_list$main_plot <- p
      } else if (task == "Survival") {

        # C-index calculation (uses the derived linear predictor proxy: 'preds')
        perf_data <- tibble::tibble(
          Surv_object = survival::Surv(data[[outcome]], data[["status"]]),
          preds = preds
        )
        c_index_val <- tryCatch({
          survival::concordance(Surv_object ~ preds, data = perf_data)$concordance
        }, error = function(e) { showNotification(paste("C-index calculation failed:", e$message), type = "error"); NA })
        metric_tbl <- tibble(Metric = "C-index", Value = round(c_index_val, 4))

        # --- Survival Plots ---
        plot_data_raw <- tibble(Index = seq_along(preds),
                                Linear_Pred = preds,
                                Hazard_Ratio = exp(preds))

        plot_data <- plot_data_raw %>%
          dplyr::filter(!is.na(Linear_Pred))

        # 2. Linear Predictor Plot (Log-Hazard Proxy)
        p_lp <- ggplot(plot_data, aes(x = Index, y = Linear_Pred, group = 1,
                                      text = paste("Index:", Index, "<br>Log-Hazard:", round(Linear_Pred, 4)))) +
          geom_line(color = "steelblue", linewidth = 1) +
          geom_point(color = "steelblue", size = 1) +
          theme_minimal(base_size = 14) +
          labs(
            title = "Predicted Linear Predictor (Log-Hazard Ratio Proxy)",
            subtitle = paste0("C-index = ", round(c_index_val, 3)),
            x = "Observation Index",
            y = "Linear Predictor proxy (-log(S(t=1)))"
          )

        # 3. Hazard Ratio Plot (Transformed Risk Score)
        p_hr <- ggplot(plot_data, aes(x = Index, y = Hazard_Ratio, group = 1,
                                      text = paste("Index:", Index, "<br>Hazard Ratio:", round(Hazard_Ratio, 4)))) +
          geom_line(color = "firebrick", linewidth = 1) +
          geom_point(color = "firebrick", size = 1) +
          theme_minimal(base_size = 14) +
          labs(
            title = "Predicted Hazard Ratio (Risk Score)",
            subtitle = paste0("C-index = ", round(c_index_val, 3)),
            x = "Observation Index",
            y = "Hazard Ratio (Exp(Linear Predictor))"
          )

        # Store both raw ggplot objects
        ggplots_list$linear_pred <- p_lp
        ggplots_list$hazard_ratio <- p_hr
      }

      metrics_rv(metric_tbl)
      plots_rv(list(ggplots = ggplots_list, preds = preds, c_index = c_index_val))
    })

    output$performance_results_ui <- renderUI({

      req(plots_rv()$ggplots)

      tagList(
        # --- Plot Selection (Survival Only) ---
        checkboxGroupInput(
          session$ns("plot_selection"),
          label = "Select Plot View (Survival Only)",
          choices = c("Linear Predictor (Log-Hazard)" = "linear_pred",
                      "Hazard Ratio (Risk Score)" = "hazard_ratio"),
          selected = "linear_pred",
          inline = TRUE
        ),

        # --- Dynamic Plot Output ---
        withSpinner(uiOutput(session$ns("perf_plots_output"))),

        br(),
        fluidRow(
          column(3, downloadButton(session$ns("download_plot"), "Download Plot (PNG)"))
        ),
        br(),
        DTOutput(session$ns("metrics_table")),
        br(),
        fluidRow(
          column(3, downloadButton(session$ns("download_metrics"), "Download Metrics (CSV)"))
        )
      )
    })

    # --- DYNAMIC UI RENDERING (Survival) ---
    output$perf_plots_output <- renderUI({
      req(plots_rv()$ggplots)
      task <- app_data_rv$task

      if (task == "Survival") {
        plot_outputs <- list()

        if ("linear_pred" %in% input$plot_selection) {
          plot_outputs <- append(plot_outputs, list(
            plotly::plotlyOutput(session$ns("plot_linear_pred"), height = "400px"),
            br()
          ))
        }
        if ("hazard_ratio" %in% input$plot_selection) {
          plot_outputs <- append(plot_outputs, list(
            plotly::plotlyOutput(session$ns("plot_hazard_ratio"), height = "400px"),
            br()
          ))
        }
        return(tagList(plot_outputs))
      } else {
        return(plotly::plotlyOutput(session$ns("main_plot"), height = "400px"))
      }
    })

    # --- PLOTLY RENDERERS ---

    output$main_plot <- plotly::renderPlotly({
      req(plots_rv()$ggplots$main_plot)
      plotly::ggplotly(plots_rv()$ggplots$main_plot, tooltip = "all")
    })

    output$plot_linear_pred <- plotly::renderPlotly({
      req(plots_rv()$ggplots$linear_pred)
      plotly::ggplotly(plots_rv()$ggplots$linear_pred, tooltip = "text")
    })

    output$plot_hazard_ratio <- plotly::renderPlotly({
      req(plots_rv()$ggplots$hazard_ratio)
      plotly::ggplotly(plots_rv()$ggplots$hazard_ratio, tooltip = "text")
    })

    # ... Metrics Table and Download handlers ...

    output$metrics_table <- renderDT({
      req(metrics_rv())
      DT::datatable(metrics_rv(), options = list(dom = 't', paging = FALSE))
    })

    output$download_metrics <- downloadHandler(
      filename = function() paste0("model_metrics_", Sys.Date(), ".csv"),
      content = function(file) readr::write_csv(metrics_rv(), file)
    )

    output$download_plot <- downloadHandler(
      filename = function() paste0("performance_plot_", Sys.Date(), ".png"),
      content = function(file) {
        req(plots_rv()$ggplots)
        task <- app_data_rv$task
        current_plot_name <- NULL

        if (task == "Survival") {
          if ("hazard_ratio" %in% input$plot_selection) {
            current_plot_name <- "hazard_ratio"
          } else if ("linear_pred" %in% input$plot_selection) {
            current_plot_name <- "linear_pred"
          } else {
            showNotification("Select a survival plot type to download.", type = "warning")
            return(NULL)
          }
        } else {
          current_plot_name <- "main_plot"
        }

        current_plot <- plots_rv()$ggplots[[current_plot_name]]

        if (!is.null(current_plot)) {
          ggplot2::ggsave(file, plot = current_plot, width = 7, height = 5, dpi = 300)
        }
      }
    )
  })
}
