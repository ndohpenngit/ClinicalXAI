library(shiny)
library(shinydashboard)
library(tidymodels)
library(xgboost)
library(ranger)
library(readr)
library(DT)
library(rlang)
library(survival) # Necessary for the Surv() function
library(dplyr) # Required for data manipulation

# --- Custom Data Simulation Function (Enhanced Version from User) ---

#' Simulate Clinical Trial Data (Enhanced)
#'
#' @description
#' Generates simulated clinical trial data for continuous, binary, or survival outcomes
#' under various designs (parallel, paired, one-sample). Optionally adds covariates
#' and returns train/test datasets for modeling workflows.
#'
#' @details
#' - Continuous: normal outcome
#' - Binary: Bernoulli/logit model
#' - Survival: exponential survival times with optional censoring
#' - Supports parallel, paired, and one-sample designs
#' - Adds baseline covariates (age, sex, biomarker)
#' - Optionally splits into train/test sets
#'
#' @param design One of "parallel", "paired", "one-sample"
#' @param outcome One of "continuous", "binary", "survival"
#' @param n_total Total sample size
#' @param effect_size Treatment effect:
#'   - mean diff (continuous)
#'   - log-odds ratio (binary)
#'   - log-hazard ratio (survival)
#' @param sd Standard deviation (continuous only)
#' @param p_baseline Baseline probability (binary)
#' @param lambda_baseline Baseline hazard (survival)
#' @param rho Correlation (paired, continuous)
#' @param censor_rate Proportion censored (survival)
#' @param add_covariates Logical; whether to simulate covariates
#' @param train_prop Proportion for training set (0–1, optional)
#' @param seed Random seed
#'
#' @return
#' A data.frame (or list with train/test) with:
#' - id, arm, y, status (if survival)
#' - Covariates (if requested)
#' - Attributes: outcome, design, effect_size, parameters
#'
#' @examples
#' \dontrun{
# Continuous, with covariates
# dat <- simulate_data("parallel", "continuous", n_total = 100, effect_size = 0.5)
#
# # Binary, with train/test split
# dat2 <- simulate_data("parallel", "binary", n_total = 120, effect_size = log(2), train_prop = 0.7)
# str(dat2$train)
#
# # Survival, one-sample
# dat3 <- simulate_data("one-sample", "survival", n_total = 80, effect_size = log(0.8))
# head(dat3)
#' }
#'
#' @export
simulate_data <- function(design = c("parallel", "paired", "one-sample"),
                          outcome = c("continuous", "binary", "survival"),
                          n_total = 100,
                          effect_size = 0.5,
                          sd = 1,
                          p_baseline = 0.3,
                          lambda_baseline = 0.05,
                          rho = 0.3,
                          censor_rate = 0.2,
                          add_covariates = TRUE,
                          train_prop = NULL,
                          seed = NULL) {

  design <- match.arg(design)
  outcome <- match.arg(outcome)
  if (!is.null(seed)) set.seed(seed)
  if (n_total <= 1) stop("Sample size must be greater than 1.")

  id <- seq_len(n_total)
  if (design == "parallel") {
    if (n_total %% 2 != 0) {
      warning("n_total is odd. Adjusting to nearest even number for parallel design.")
      n_total <- n_total - 1
      id <- seq_len(n_total)
    }
    arm <- rep(c("control", "treatment"), each = n_total / 2)
    n_per <- n_total / 2
  } else {
    arm <- rep("single", n_total)
    n_per <- n_total
  }

  if (outcome == "continuous") {
    if (design == "parallel") { y <- c(stats::rnorm(n_per, mean = 0, sd = sd), stats::rnorm(n_per, mean = effect_size, sd = sd))
    } else if (design == "paired") { y0 <- stats::rnorm(n_per, mean = 0, sd = sd); y1 <- y0 * rho + sqrt(1 - rho^2) * stats::rnorm(n_per, mean = effect_size, sd = sd); y <- y1 - y0
    } else { y <- stats::rnorm(n_total, mean = effect_size, sd = sd) }
  } else if (outcome == "binary") {
    if (design == "parallel") { p_ctrl <- p_baseline; p_trt <- stats::plogis(stats::qlogis(p_ctrl) + effect_size); y <- factor(c(stats::rbinom(n_per, 1, p_ctrl), stats::rbinom(n_per, 1, p_trt)), levels = c(0, 1), labels = c("No", "Yes"))
    } else if (design == "paired") { p_ctrl <- p_baseline; p_trt <- stats::plogis(stats::qlogis(p_ctrl) + effect_size); y0 <- stats::rbinom(n_per, 1, p_ctrl); y1_prob <- stats::plogis(stats::qlogis(p_trt) + 0.5 * y0 * effect_size); y1 <- stats::rbinom(n_per, 1, y1_prob); y <- factor(y1, levels = c(0, 1), labels = c("No", "Yes"))
    } else { p_trt <- stats::plogis(stats::qlogis(p_baseline) + effect_size); y <- factor(stats::rbinom(n_total, 1, p_trt), levels = c(0, 1), labels = c("No", "Yes")) }
  } else if (outcome == "survival") {
    if (design == "parallel") { lambda_ctrl <- lambda_baseline; lambda_trt <- lambda_baseline * exp(-effect_size); t_ctrl <- stats::rexp(n_per, rate = lambda_ctrl); t_trt <- stats::rexp(n_per, rate = lambda_trt); time <- c(t_ctrl, t_trt)
    } else { lambda <- lambda_baseline * exp(-effect_size); time <- stats::rexp(n_total, rate = lambda) }
    cens_time <- stats::rexp(n_total, rate = lambda_baseline * censor_rate); status <- as.numeric(time <= cens_time); time <- pmin(time, cens_time); y <- time
  }

  data <- data.frame(id = id, arm = factor(arm), y = y)
  if (outcome == "survival") {
    data <- data %>% dplyr::rename(time = y) %>% dplyr::mutate(status = status)
    if (design == "parallel") data$arm <- factor(rep(c("control", "treatment"), each = n_per))
  }

  if (outcome == "continuous" && design == "paired") { data <- data %>% dplyr::rename(y_diff = y) }
  if (add_covariates) { data$age <- round(stats::rnorm(n_total, 60, 10)); data$sex <- factor(sample(c("Male", "Female"), n_total, TRUE)); data$biomarker <- stats::rnorm(n_total, mean = ifelse(data$arm == "treatment", 1, 0), sd = 1) }
  if (design %in% c("paired", "one-sample") && outcome != "survival") { data <- dplyr::select(data, -arm) }

  if (!is.null(train_prop)) {
    train_idx <- sample(seq_len(n_total), size = floor(train_prop * n_total))
    result <- list(train = data[train_idx, ], test = data[-train_idx, ])
  } else { result <- data }

  attr(result, "outcome") <- outcome
  attr(result, "design") <- design
  attr(result, "parameters") <- list(n_total = n_total, effect_size = effect_size, sd = sd, p_baseline = p_baseline, lambda_baseline = lambda_baseline, rho = rho, censor_rate = censor_rate)

  return(result)
}
