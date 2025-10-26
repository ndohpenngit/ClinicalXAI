xai_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "xai",
    h2("3. Explainable AI (XAI) 🧠: Model Explanation with SHAP Values"),
    # --- New User Guide Box ---
    box(
      title = "Instructions",
      status = NULL,
      solidHeader = FALSE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      p(
        strong("Prerequisite:"), " A model must be trained in the 'Model Specification' tab.",
        br(),
        strong("SHAP Computation:"), " Click ", strong("Compute SHAP / Importance"), " to generate feature attribution values for every observation.",
        br(),
        strong("Modes:"),
        tags$ul(
          tags$li(strong("Global Importance:"), " Shows the average magnitude of SHAP values (Mean |SHAP|) for all features. This is the main summary of overall feature impact."),
          tags$li(strong("Local Explanation:"), " (Force Plot) Allows you to select a single observation and view how each feature's value pushes the prediction above or below the baseline prediction."),
          tags$li(strong("Compare Train vs Validation:"), " (If validation data is uploaded) Compares the Mean |SHAP| between training and validation sets to check for feature importance stability.")
        )
      )
    ),
    # -------------------------
    # --- Control panel ---
    fluidRow(
      box(
        title = "Model Explanation Tools",
        status = "primary", solidHeader = TRUE, width = 12,
        fileInput(ns("val_data_upload"), "Upload Validation Data (optional)", accept = ".csv"),
        radioButtons(
          ns("shap_mode"),
          "SHAP Mode:",
          choices = c("Single Dataset" = "single", "Compare Train vs Validation" = "compare"),
          selected = "single",
          inline = TRUE
        ),
        actionButton(ns("compute_shap"), "Compute SHAP / Importance", icon = icon("brain"), class = "btn-primary"),
        br(), br(),
        selectInput(
          ns("xai_mode"),
          "Select Explanation Type:",
          choices = c("Global Importance", "Local Explanation (Force Plot)"),
          selected = "Global Importance",
          width = "50%"
        ),
        uiOutput(ns("obs_selector_ui")),
        uiOutput(ns("data_source_ui"))
      )
    ),

    # --- Conditional Output Container ---
    uiOutput(ns("xai_outputs")),

    # --- Local explanation ---
    fluidRow(
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Local Explanation (Force Plot)'", ns("xai_mode")),
        box(
          title = "Local Explanation Details",
          status = NULL, solidHeader = TRUE, width = 12,
          withSpinner(DTOutput(ns("local_table")))
        )
      )
    ),

    # --- Correlation heatmap ---
    fluidRow(
      conditionalPanel(
        condition = sprintf("input['%s'] == 'compare'", ns("shap_mode")),
        box(
          title = "Feature Importance Stability: Train vs Validation (Correlation Heatmap)",
          status = NULL, solidHeader = TRUE, width = 12,
          withSpinner(plotlyOutput(ns("shap_corr_plot"), height = "500px"))
        )
      )
    ),

    # --- Stability summary ---
    fluidRow(
      conditionalPanel(
        condition = sprintf("input['%s'] == 'compare'", ns("shap_mode")),
        box(
          title = "Top 5 Most Unstable Features (Δ Mean |SHAP| Train vs Validation)",
          status = NULL, solidHeader = TRUE, width = 12,
          withSpinner(DTOutput(ns("shap_stability_table")))
        )
      )
    )
  )
}


xai_server <- function(id, app_data_rv) {
  moduleServer(id, function(input, output, session) {

    shap_rv <- reactiveVal(NULL)
    shap_val_rv <- reactiveVal(NULL)
    shap_explainer <- reactiveVal(NULL)
    local_table_rv <- reactiveVal(NULL)
    val_data_rv <- reactiveVal(NULL)

    # --- Validation upload ---
    observeEvent(input$val_data_upload, {
      req(input$val_data_upload)
      val_data_rv(readr::read_csv(input$val_data_upload$datapath))
      showNotification("Validation data loaded successfully.", type = "message")
    })

    output$data_source_ui <- renderUI({
      if (!is.null(val_data_rv())) strong("Validation dataset available.") else strong("Using training dataset only.")
    })

    # --- SHAP computation ---
    observeEvent(input$compute_shap, {
      req(app_data_rv$fit, app_data_rv$outcome_name)
      showModal(modalDialog("Computing SHAP values...", footer = NULL))

      fit_obj <- app_data_rv$fit
      outcome <- app_data_rv$outcome_name
      task <- app_data_rv$task
      train_data <- app_data_rv$data
      val_data <- val_data_rv()

      # --- Determine Model Engine (Crucial for RSF prediction in pred_fun) ---
      model_engine <- if(inherits(fit_obj, "workflow")) {
        fit_obj$fit$actions$model$spec$engine
      } else if (inherits(fit_obj, "model_fit")) {
        fit_obj$spec$engine
      } else {
        "other"
      }

      # *** pred_fun for RSF/Survival ***
      pred_fun <- function(model, newdata) {
        if (task == "Classification (Binary)") {
          preds <- predict(model, newdata)
          prob_col <- grep("^\\.pred_", names(preds), value = TRUE)[1]
          as.numeric(preds[[prob_col]])
        } else if (task == "Survival") {
          # For Survival, we predict the linear predictor (Log-Hazard Ratio Proxy)
          if (model_engine %in% c("ranger", "aorsf")) {
            # Use type="survival"
            preds_tbl <- predict(model, newdata, type = "survival", eval_time = 1)

            # ROBUST FIX: Use sapply to iterate through the .pred list and extract the inner .pred_survival column.
            if (".pred" %in% names(preds_tbl) && !is.null(preds_tbl$.pred[[1]])) {

              survival_vals <- sapply(preds_tbl$.pred, function(x) x$.pred_survival)

              if (is.numeric(survival_vals) && length(survival_vals) == nrow(newdata)) {
                # Risk score proxy = -log(S(t=1))
                preds <- -log(survival_vals)
                return(as.numeric(preds))
              } else {
                return(rep(NA, nrow(newdata)))
              }
            } else {
              return(rep(NA, nrow(newdata)))
            }
          } else {
            # Standard Cox/PH model prediction
            preds_tbl <- predict(model, newdata, type = "linear_pred")
            as.numeric(preds_tbl$.pred_linear_pred)
          }
        } else { # Regression (Continuous)
          as.numeric(predict(model, newdata)$.pred)
        }
      }
      # ************************

      compute_shap_df <- function(data) {
        predictors <- data %>% dplyr::select(-all_of(outcome))
        # For survival, the response must be the survival object (time and status).
        if (task == "Survival") {
          response <- survival::Surv(data[[outcome]], data[["status"]])
        } else {
          response <- data[[outcome]]
        }


        explainer <- tryCatch({
          DALEXtra::explain_tidymodels(
            model = fit_obj,
            data = predictors,
            y = response,
            label = paste(task, "Model"),
            type = if (task == "Classification (Binary)") "probability" else "response",
            predict_function = pred_fun
          )
        }, error = function(e) { showNotification(paste("Explainer failed:", e$message), type = "error"); NULL })
        req(explainer)

        shap_values <- tryCatch({
          predict_parts(explainer, new_observation = predictors, type = "shap")
        }, error = function(e) { showNotification(paste("SHAP computation failed:", e$message), type = "error"); NULL })
        req(shap_values)

        shap_df <- as.data.frame(shap_values)
        shap_df <- shap_df %>%
          dplyr::rename(
            Feature = dplyr::any_of("variable"),
            SHAP_Value = dplyr::any_of("contribution")
          )
        if (!"value" %in% names(shap_df)) shap_df$value <- NA
        if (!"observation" %in% names(shap_df)) shap_df$observation <- seq_len(nrow(shap_df))
        shap_df <- shap_df %>% mutate(Observation = as.integer(observation)) %>%
          select(Feature, value, SHAP_Value, Observation)
        list(df = shap_df, explainer = explainer)
      }

      train_result <- compute_shap_df(train_data)
      shap_rv(train_result$df)
      shap_explainer(train_result$explainer)

      if (!is.null(val_data) && input$shap_mode == "compare") {
        val_result <- compute_shap_df(val_data)
        shap_val_rv(val_result$df)
      } else shap_val_rv(NULL)

      removeModal()
      showNotification("SHAP computation complete.", type = "message")
    })

    # --- Conditional UI Renderer for SHAP Outputs ---
    output$xai_outputs <- renderUI({
      req(shap_rv())

      tagList(
        # --- Importance plots ---
        fluidRow(
          box(
            title = "Feature Importance / SHAP Plot",
            status = NULL, solidHeader = TRUE, width = 12,
            withSpinner(plotlyOutput(session$ns("shap_plot"), height = "500px")),
            br(),
            downloadButton(session$ns("download_shap_plot"), "Download Plot (PNG)")
          )
        ),

        # --- Raw SHAP table ---
        fluidRow(
          box(
            title = "Feature-Level SHAP Summary",
            status = NULL, solidHeader = TRUE, width = 12,
            withSpinner(DTOutput(session$ns("shap_table")))
          )
        )
      )
    })

    # --- Local observation selector ---
    output$obs_selector_ui <- renderUI({
      req(input$xai_mode)
      if (input$xai_mode == "Local Explanation (Force Plot)" && !is.null(shap_rv())) {
        obs_ids <- sort(unique(shap_rv()$Observation))
        selectInput(session$ns("obs_id"), "Select Observation:", choices = obs_ids,
                    selected = obs_ids[1], width = "50%")
      }
    })

    # --- Global & local plots ---
    output$shap_plot <- renderPlotly({
      req(shap_rv())
      if (input$xai_mode == "Global Importance") {
        shap_summary <- shap_rv() %>%
          group_by(Feature) %>%
          summarise(TrainMeanAbs = mean(abs(SHAP_Value)), .groups = "drop")

        if (input$shap_mode == "compare" && !is.null(shap_val_rv())) {
          val_summary <- shap_val_rv() %>%
            group_by(Feature) %>%
            summarise(ValMeanAbs = mean(abs(SHAP_Value)), .groups = "drop")

          merged <- full_join(shap_summary, val_summary, by = "Feature") %>%
            replace(is.na(.), 0) %>%
            mutate(
              Delta = abs(TrainMeanAbs - ValMeanAbs),
              Dataset = ifelse(TrainMeanAbs >= ValMeanAbs, "Training > Validation", "Validation > Training")
            )

          p <- ggplot(merged, aes(x = reorder(Feature, Delta), y = Delta,
                                  fill = Dataset,
                                  text = paste0("Feature: ", Feature,
                                                "<br>|Δ Mean |SHAP|| = ", round(Delta,4)))) +
            geom_col() + coord_flip() +
            scale_fill_manual(values = c("Training > Validation"="steelblue","Validation > Training"="orange")) +
            theme_minimal(base_size = 14) +
            labs(title = "Feature Stability (|Δ Mean |SHAP|| Train–Val)",
                 x = "Feature", y = "|Δ Mean |SHAP||")
        } else {
          p <- ggplot(shap_summary, aes(x = reorder(Feature, TrainMeanAbs), y = TrainMeanAbs,
                                        text = paste0("Feature: ", Feature,
                                                      "<br>Mean |SHAP| = ", round(TrainMeanAbs,4)))) +
            geom_col(fill = "steelblue") + coord_flip() +
            theme_minimal(base_size = 14) +
            labs(title = "Mean |SHAP| Feature Importance (Training)",
                 x = "Feature", y = "Mean(|SHAP|)")
        }
        ggplotly(p, tooltip = "text")

      } else {
        req(input$obs_id, shap_explainer())
        obs_idx <- as.integer(input$obs_id)
        single_obs <- app_data_rv$data[obs_idx, , drop = FALSE]
        breakdown <- predict_parts_break_down(shap_explainer(), new_observation = single_obs)
        breakdown_df <- as.data.frame(breakdown) %>%
          select(variable, contribution, variable_value) %>%
          rename(Feature = variable, Contribution = contribution, Value = variable_value)
        baseline <- breakdown_df$Contribution[breakdown_df$Feature == "(Intercept)"]
        breakdown_df <- breakdown_df %>% filter(Feature != "(Intercept)")
        pred_value <- sum(breakdown_df$Contribution) + baseline
        breakdown_df <- breakdown_df %>% mutate(Contribution = round(Contribution,4))
        local_table_rv(list(df = breakdown_df, baseline = baseline, pred = pred_value))
        plotly::ggplotly(plot(breakdown) + theme_minimal(base_size = 14))
      }
    })

    # --- Local explanation table ---
    output$local_table <- renderDT({
      req(local_table_rv())
      lt <- local_table_rv()

      baseline_val <- if (is.numeric(lt$baseline) && length(lt$baseline) == 1) round(lt$baseline, 4) else NA
      pred_val <- if (is.numeric(lt$pred) && length(lt$pred) == 1) round(lt$pred, 4) else NA

      req(lt$df)

      # 1. Prepare the main data frame for binding (ensure 'Value' is character/string).
      df_to_bind <- lt$df %>%
        mutate(Value = as.character(Value)) # Ensure consistency with footer's NA value

      # 2. Create the footer
      footer <- tibble::tibble(
        Feature = as.character(c("Baseline", "Prediction")),
        Value = as.character(c(NA, NA)),
        Contribution = as.numeric(c(baseline_val, pred_val))
      )

      # 3. Bind the data frames
      final_table <- bind_rows(
        df_to_bind %>% select(Feature, Value, Contribution),
        footer
      )

      datatable(final_table,
                options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })

    # --- Full SHAP data table ---
    output$shap_table <- renderDT({
      req(shap_rv())
      datatable(shap_rv(), options = list(pageLength = 10, scrollX = TRUE))
    })

    # --- Correlation heatmap ---
    output$shap_corr_plot <- renderPlotly({
      req(input$shap_mode == "compare", shap_rv(), shap_val_rv())
      train_summary <- shap_rv() %>% group_by(Feature) %>%
        summarise(TrainMeanAbs = mean(abs(SHAP_Value)), .groups="drop")
      val_summary <- shap_val_rv() %>% group_by(Feature) %>%
        summarise(ValMeanAbs = mean(abs(SHAP_Value)), .groups="drop")
      merged <- full_join(train_summary, val_summary, by="Feature") %>% replace(is.na(.),0)
      merged <- merged %>% mutate(Delta = abs(TrainMeanAbs - ValMeanAbs))
      corr_val <- cor(merged$TrainMeanAbs, merged$ValMeanAbs, method="spearman")
      p <- ggplot(merged, aes(x=TrainMeanAbs, y=ValMeanAbs, color=Delta,
                              text=paste0("Feature: ",Feature,
                                          "<br>Train |SHAP|: ",round(TrainMeanAbs,4),
                                          "<br>Val |SHAP|: ",round(ValMeanAbs,4),
                                          "<br>|Δ|: ",round(Delta,4)))) +
        geom_point(size=3, alpha=0.8) +
        geom_smooth(method="lm", se=FALSE, color="gray40", linewidth=1) +
        scale_color_viridis_c(option="C") +
        theme_minimal(base_size=14) +
        labs(title=paste0("Train vs Validation Mean |SHAP| (ρ = ",round(corr_val,3),")"),
             x="Train Mean |SHAP|", y="Validation Mean |SHAP|", color="|Δ| (Instability)")
      ggplotly(p, tooltip="text")
    })

    # --- Stability table ---
    output$shap_stability_table <- renderDT({
      req(input$shap_mode=="compare", shap_rv(), shap_val_rv())
      train_summary <- shap_rv() %>% group_by(Feature) %>%
        summarise(TrainMeanAbs=mean(abs(SHAP_Value)), .groups="drop")
      val_summary <- shap_val_rv() %>% group_by(Feature) %>%
        summarise(ValMeanAbs=mean(abs(SHAP_Value)), .groups="drop")
      merged <- full_join(train_summary,val_summary,by="Feature") %>%
        replace(is.na(.),0) %>%
        mutate(Delta=abs(TrainMeanAbs-ValMeanAbs),
               Delta_Pct=100*Delta/pmax(TrainMeanAbs,ValMeanAbs,1e-9)) %>%
        arrange(desc(Delta)) %>% slice_head(n=5)
      datatable(
        merged %>%
          mutate(across(c(TrainMeanAbs,ValMeanAbs,Delta),round,4),
                 Delta_Pct=paste0(round(Delta_Pct,1),"%")) %>%
          rename(`Train Mean |SHAP|`=TrainMeanAbs,
                 `Validation Mean |SHAP|`=ValMeanAbs,
                 `Δ Mean |SHAP|`=Delta, `% Change`=Delta_Pct),
        options=list(pageLength=5, searching=FALSE, dom='t'), rownames=FALSE)
    })

    # --- Download plot ---
    output$download_shap_plot <- downloadHandler(
      filename=function() paste0("xai_plot_",Sys.Date(),".png"),
      content=function(file){ ggsave(file, plot=last_plot(), width=7, height=5, dpi=300) }
    )
  })
}
