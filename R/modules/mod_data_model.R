data_model_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "build_model",
    h2("1. Interactive Model Specification and Training"),
    # --- User Guide Box ---
    box(
      title = "Instructions & Overview",
      status = NULL,
      solidHeader = FALSE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      p(
        strong("Data Source:"), " Either upload a CSV file or use the controls to simulate a dataset (e.g., Clinical Trial data). Simulated data automatically includes common covariates (age, sex).",
        br(),
        strong("Model Setup:"), " Select your desired Outcome Variable and the Prediction Task (Classification, Regression, or Survival). The Algorithm choices will automatically update based on the task type. Click 'TRAIN MODEL' to fit the model."
      ),
      p(
        em("Note: For Survival tasks, select the Time variable as the Outcome. The 'status' variable must be present for event/censor information.")
      )
    ),

    fluidRow(
      box(
        title = "Data Upload / Simulation", status = "primary", solidHeader = TRUE, width = 4,
        fileInput(ns("file_upload"), "Upload CSV", accept = ".csv"),
        selectInput(ns("sim_design"), "Trial Design", choices = c("parallel", "paired", "one-sample")),
        selectInput(ns("sim_outcome"), "Outcome Type", choices = c("continuous", "binary", "survival")),
        numericInput(ns("sim_n"), "Sample Size", value = 100, min = 2),
        numericInput(ns("sim_effect"), "Effect Size", value = 0.5, step = 0.1),
        uiOutput(ns("sd_input")),
        uiOutput(ns("p_baseline_input")),
        uiOutput(ns("lambda_input")),
        uiOutput(ns("rho_input")),
        uiOutput(ns("censor_input")),
        checkboxInput(ns("sim_covariates"), "Add covariates (age, sex)", TRUE),
        numericInput(ns("sim_train"), "Train proportion (0-1, optional)", value = 1, min = 0, max = 1, step = 0.05),
        actionButton(ns("simulate_button"), "SIMULATE DATA", icon = icon("random")),
        uiOutput(ns("data_status"))
      ),
      box(
        title = "Model Specification", status = "primary", solidHeader = TRUE, width = 8,
        fluidRow(
          column(4, uiOutput(ns("outcome_select_ui"))),
          column(4, radioButtons(ns("task_type"), "Prediction Task:",
                                 choices = c("Classification (Binary)", "Regression (Continuous)", "Survival"))),
          column(4, uiOutput(ns("model_algo_ui"))),
          actionButton(ns("train_button"), "TRAIN MODEL", icon = icon("cogs"), class = "btn-primary", style = "margin-top:5px;")
        )
      )
    ),
    fluidRow(
      conditionalPanel(
        condition = sprintf("output['%s'] == '1'", ns("show_results_flag")),
        box(status = NULL, width = 12,
            textOutput(ns("training_status_text"))
        ),
        box(title = "Data Preview", status = NULL, solidHeader = TRUE, width = 12, collapsible = TRUE,
            withSpinner(DT::DTOutput(ns("data_preview_table"))),

            tags$div(style = "margin-top: 10px;",
                     downloadButton(ns("download_data"), "Download Data", icon = icon("download"))
            )
        )
      )
    )
  )
}

data_model_server <- function(id, app_data_rv) {
  moduleServer(id, function(input, output, session) {

    `%||%` <- function(a, b) if (!is.null(a)) a else b

    # Flag for conditional panel
    output$show_results_flag <- renderText({ if (is.null(app_data_rv$data)) "0" else "1" })
    outputOptions(output, "show_results_flag", suspendWhenHidden = FALSE)

    # --- Conditional UI for simulation ---
    output$sd_input <- renderUI({ req(input$sim_outcome); if(input$sim_outcome=="continuous") numericInput(session$ns("sim_sd"), "SD", 1, 0.1) })
    output$p_baseline_input <- renderUI({ req(input$sim_outcome); if(input$sim_outcome=="binary") numericInput(session$ns("sim_p"), "Baseline prob", 0.3, 0, 1, 0.01) })
    output$lambda_input <- renderUI({ req(input$sim_outcome); if(input$sim_outcome=="survival") numericInput(session$ns("sim_lambda"), "Baseline hazard", 0.05, 0.001, 1, 0.01) })
    output$rho_input <- renderUI({ req(input$sim_design,input$sim_outcome); if(input$sim_design=="paired" && input$sim_outcome!="survival") numericInput(session$ns("sim_rho"), "Correlation", 0.3, -1, 1, 0.05) })
    output$censor_input <- renderUI({ req(input$sim_outcome); if(input$sim_outcome=="survival") numericInput(session$ns("sim_censor"), "Censor rate", 0.2, 0, 1, 0.05) })

    # --- CSV Upload ---
    observeEvent(input$file_upload, {
      req(input$file_upload)
      app_data_rv$data <- read_csv(input$file_upload$datapath)
      app_data_rv$fit <- NULL
      output$training_status_text <- renderText("Data loaded! Specify model.")
    })

    # --- Simulation ---
    observeEvent(input$simulate_button, {
      req(input$sim_design, input$sim_outcome, input$sim_n)
      showModal(modalDialog("Simulating data...", footer = NULL))

      sim_args <- list(
        design = input$sim_design,
        outcome = input$sim_outcome,
        n_total = input$sim_n,
        effect_size = input$sim_effect,
        add_covariates = input$sim_covariates,
        train_prop = if(input$sim_train==1) NULL else input$sim_train,
        seed = 123
      )
      if(input$sim_outcome=="continuous") sim_args$sd <- input$sim_sd %||% 1
      if(input$sim_outcome=="binary") sim_args$p_baseline <- input$sim_p %||% 0.3
      if(input$sim_outcome=="survival") { sim_args$lambda_baseline <- input$sim_lambda %||% 0.05; sim_args$censor_rate <- input$sim_censor %||% 0.2 }
      if(input$sim_design=="paired" && input$sim_outcome!="survival") sim_args$rho <- input$sim_rho %||% 0.3
      app_data_rv$data <- do.call(simulate_data, sim_args)
      if(is.list(app_data_rv$data) && !is.null(app_data_rv$data$train)) app_data_rv$data <- app_data_rv$data$train

      removeModal()
      showNotification("Simulation complete.", type = "message")
      output$training_status_text <- renderText(paste0("Simulated ", input$sim_outcome, " data (n =", nrow(app_data_rv$data), ")"))
    })

    # --- Outcome selection ---
    output$outcome_select_ui <- renderUI({
      req(app_data_rv$data)

      # Filter available choices based on the selected task type
      choices <- names(app_data_rv$data)
      if (input$task_type == "Survival") {
        # In survival, the outcome is the time variable.
        # The 'status' variable is assumed to exist alongside the time variable.
        # We restrict choices to common time-like variable names and exclude 'id' and 'status'.
        choices <- choices[grepl("time|y", choices, ignore.case = TRUE) & !choices %in% c("id", "status")]
      } else {
        # For Classification/Regression, exclude ID and Status (if present)
        choices <- choices[!choices %in% c("id", "status")]
      }

      # Ensure the default outcome variable for simulation ('y' or 'time') is the default selection
      default_selection <- if (input$task_type == "Survival") "time" else "y"

      selectInput(session$ns("outcome_var"), "Outcome (Y)",
                  choices = choices,
                  selected = if(default_selection %in% choices) default_selection else choices[1])
    })

    # --- Algorithm selection ---
    output$model_algo_ui <- renderUI({
      req(input$task_type)
      choices <- switch(input$task_type,
                        "Classification (Binary)" = c("XGBoost","Random Forest","Logistic Regression"),
                        "Regression (Continuous)" = c("XGBoost","Random Forest","Linear Regression"),
                        "Survival" = c("Cox Regression","Random Survival Forest"))
      selectInput(session$ns("model_algo"), "Algorithm", choices = choices)
    })

    # --- Data preview ---
    output$data_preview_table <- DT::renderDT({ req(app_data_rv$data); app_data_rv$data })

    # --- Train model ---
    observeEvent(input$train_button, {
      req(app_data_rv$data, input$outcome_var, input$task_type, input$model_algo)
      data_to_use <- app_data_rv$data
      outcome_var_name <- input$outcome_var

      rec <- if(input$task_type=="Survival") {
        # 1. Define recipe with the original outcome variables (e.g., "time" + "status")
        recipe(as.formula(paste(outcome_var_name, "+ status ~ .")), data_to_use) %>%
          step_rm(id) %>%
          # 2. Standard preprocessing steps (all_numeric_predictors() safely excludes time/status)
          step_dummy(all_nominal_predictors()) %>%
          step_impute_mean(all_numeric_predictors()) %>%
          step_normalize(all_numeric_predictors()) %>%
          # 3. Mutate the two outcome variables into a single Surv object.
          step_mutate(
            !!sym(outcome_var_name) := Surv(!!sym(outcome_var_name), status),
            status = NULL,
            skip = TRUE # Skip this step during prediction
          )
      } else {
        # Existing Classification/Regression logic
        recipe(as.formula(paste(outcome_var_name, "~ .")), data_to_use) %>%
          step_rm(id) %>%
          step_dummy(all_nominal_predictors()) %>%
          step_impute_mean(all_numeric_predictors()) %>%
          step_normalize(all_numeric_predictors())
      }


      # MODEL SPECIFICATION
      model_spec <- switch(input$model_algo,
                           "XGBoost" = boost_tree(trees = 500) %>%
                             set_engine("xgboost") %>%
                             set_mode(ifelse(input$task_type=="Classification (Binary)","classification","regression")),
                           "Random Forest" = rand_forest(trees = 500) %>%
                             set_engine("ranger") %>%
                             set_mode(ifelse(input$task_type=="Classification (Binary)","classification","regression")),
                           "Logistic Regression" = logistic_reg() %>%
                             set_engine("glm") %>%
                             set_mode("classification"),
                           "Linear Regression" = linear_reg() %>%
                             set_engine("lm") %>%
                             set_mode("regression"),
                           "Cox Regression" = proportional_hazards() %>%
                             set_engine("survival") %>% set_mode("censored regression"),
                           "Random Survival Forest" = rand_forest(trees = 500) %>%
                             set_engine("aorsf", importance = "permute") %>% set_mode("censored regression")
      )

      # WORKFLOW CONSTRUCTION
      wf <- workflow() %>%
        add_recipe(rec) %>%
        add_model(model_spec)


      fit_model <- tryCatch({
        withProgress(message = "Training model...", value = 0, {
          incProgress(0.2, detail = "Preprocessing data")
          Sys.sleep(0.3)
          incProgress(0.5, detail = "Fitting model")
          model_fit <- fit(wf, data = data_to_use)
          incProgress(0.8, detail = "Finalizing")
          Sys.sleep(0.2)
          incProgress(1, detail = "Done!")
          model_fit
        })
      }, error = function(e) {
        showNotification(e$message, type = "error")
        NULL
      })

      app_data_rv$fit <- fit_model
      app_data_rv$task <- input$task_type
      app_data_rv$outcome_name <- outcome_var_name
      app_data_rv$fit_source <- ifelse(grepl("ranger|RandomForest", input$model_algo), "ranger", "tidymodels")

      output$training_status_text <- renderText({ if(is.null(fit_model)) "Training failed." else paste("Model trained:", input$model_algo) })
    })

    # Download Handler for Rendered Data
    output$download_data <- downloadHandler(
      filename = function() {
        data_source <- if (is.null(input$file_upload)) "Simulated" else "Uploaded"
        paste0(data_source, "_Data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(app_data_rv$data)

        readr::write_csv(app_data_rv$data, file)
      }
    )
  })
}
