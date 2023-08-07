#' PERR method with Cox proportional hazards regression
#'
#' This function performs the PERR method using Cox proportional hazards regression to adjust the treatment effect estimate.
#'
#' @param data A data frame containing the data.
#' @param treatment_var The name of the treatment variable in the data.
#' @param outcome_var The name of the time-to-event outcome variable in the data.
#' @param covariates A character vector of covariate names. Default is NULL.
#' @param interactions A character vector of interaction term names. Default is NULL.
#' @param nonlinear_terms A character vector of nonlinear term names. Default is NULL.
#'
#' @return A list containing the original and adjusted hazard ratios (HR) with their 95% confidence intervals.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(treatment = c(0,1,0,1), outcome = c(5,3,2,4))
#' perr_cox(data, "treatment", "outcome")
#' }
#' @export
perr_cox <- function(data, treatment_var, outcome_var, covariates = NULL, interactions = NULL, nonlinear_terms = NULL) {
  # ... function body ...
}
library(dplyr)
library(survival)
# Cox比例风险回归
perr_cox <- function(data, treatment_var, time_var, event_var, covariates = NULL, interactions = NULL, nonlinear_terms = NULL) {
  
  # 计算先验事件率
  prior_event_rate <- data %>%
    group_by(!!sym(treatment_var)) %>%
    summarise(prior_event_rate = mean(!!sym(event_var)), .groups = 'drop')
  
  # 检查分母是否为0
  if (prior_event_rate$prior_event_rate[1] == 0) {
    stop("Error: Denominator of PERR is zero.")
  }
  
  # 计算先验事件率比（PERR）
  perr <- prior_event_rate$prior_event_rate[2] / prior_event_rate$prior_event_rate[1]
  
  # 创建公式
  formula <- paste("Surv(", time_var, ",", event_var, ") ~", treatment_var, 
                   if (!is.null(covariates)) paste("+", paste(covariates, collapse = " + ")), 
                   if (!is.null(interactions)) paste("+", paste(interactions, collapse = " + ")), 
                   if (!is.null(nonlinear_terms)) paste("+", paste(nonlinear_terms, collapse = " + ")))
  
  # 使用PERR调整治疗效果估计
  model <- coxph(as.formula(formula), data = data)
  model_adjusted <- coxph(as.formula(paste("I(", event_var, "/", perr, ") ~", treatment_var, 
                                           if (!is.null(covariates)) paste("+", paste(covariates, collapse = " + ")), 
                                           if (!is.null(interactions)) paste("+", paste(interactions, collapse = " + ")), 
                                           if (!is.null(nonlinear_terms)) paste("+", paste(nonlinear_terms, collapse = " + ")))), data = data)
  
  # 获取HR和95%CI
  coef_original <- coef(summary(model))
  HR_original <- exp(coef_original[, "coef"])
  CI_low_original <- exp(coef_original[, "coef"] - 1.96 * coef_original[, "se(coef)"])
  CI_high_original <- exp(coef_original[, "coef"] + 1.96 * coef_original[, "se(coef)"])
  
  coef_adjusted <- coef(summary(model_adjusted))
  HR_adjusted <- exp(coef_adjusted[, "coef"])
  CI_low_adjusted <- exp(coef_adjusted[, "coef"] - 1.96 * coef_adjusted[, "se(coef)"])
  CI_high_adjusted <- exp(coef_adjusted[, "coef"] + 1.96 * coef_adjusted[, "se(coef)"])
  
  return(list(original = data.frame(HR = HR_original, CI_low = CI_low_original, CI_high = CI_high_original),
              adjusted = data.frame(HR = HR_adjusted, CI_low = CI_low_adjusted, CI_high = CI_high_adjusted)))
}
