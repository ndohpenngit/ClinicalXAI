# ======================================================
# ClinicalXAI: Explainable Predictive Modeling
# ======================================================

# --- Core Shiny Framework ---
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)

# --- Data Manipulation & Plotting ---
library(dplyr)
library(tibble)
library(ggplot2)
library(readr)
library(plotly)
library(orca)

# --- Tidymodels Framework (Modeling Pipeline) ---
library(tidymodels)
library(recipes)
library(workflows)
library(parsnip)
library(rsample)
library(yardstick)   # For metrics

# --- ML Engines ---
library(xgboost)
library(survival)
library(ranger) # Required for Random Forest
library(aorsf)  # Required for Random Survival Forest engine (or use ranger)
library(censored)

# --- Explainable AI / SHAP Tools ---
library(DALEX)
library(DALEXtra)
library(shapley)
library(shapviz)

# --- Utility ---
library(broom)
library(rlang)

# --- Source module/functions ---
source("R/modules/mod_data_model.R")
source("R/modules/mod_performance.R")
source("R/modules/mod_xai.R")
source("R/simulate_data.R")

# --- Shared reactive values ---
app_data_rv <- reactiveValues(
  data = NULL,        # Raw/Processed data (training set)
  fit = NULL,         # Fitted tidymodels workflow
  recipe = NULL,      # Preprocessing recipe
  task = NULL,        # "Classification (Binary)", "Regression (Continuous)", or "Survival"
  outcome_name = NULL,# Outcome variable
  fit_source = NULL   # Optional: source of model (ranger/xgboost/tidymodels)
)

# --- UI ---
ui <- dashboardPage(
  dashboardHeader(title = "ClinicalXAI"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Build Model", tabName = "build_model", icon = icon("cogs")),
      menuItem("2. Performance", tabName = "performance", icon = icon("chart-line")),
      menuItem("3. Explainable AI", tabName = "xai", icon = icon("brain")),
      hr(),
      div(
          style = "padding: 10px;",
          actionButton("show_guide", "Help & Guide", icon = icon("info-circle"),
                       width = "55%",
                       class = "btn-info")
      )
    )
  ),
  dashboardBody(
    tabItems(
      data_model_ui("data_model_1"),
      performance_ui("performance_1"),
      xai_ui("xai_1")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  # Modules
  data_model_server("data_model_1", app_data_rv)
  performance_server("performance_1", app_data_rv)
  xai_server("xai_1", app_data_rv)

  # Help & Guide Modal
  observeEvent(input$show_guide, {
    guide_path <- "www/user_guide.html"
    guide_content <- HTML(readLines(guide_path, warn = FALSE))

    showModal(modalDialog(
      guide_content,

      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}

# --- Run App ---
shinyApp(ui, server)
