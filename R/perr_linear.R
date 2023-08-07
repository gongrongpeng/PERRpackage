#' PERR method with linear regression
#'
#' This function performs the PERR method using linear regression to adjust the treatment effect estimate.
#'
#' @param data A data frame containing the data.
#' @param treatment_var The name of the treatment variable in the data.
#' @param outcome_var The name of the continuous outcome variable in the data.
#' @param covariates A character vector of covariate names. Default is NULL.
#' @param interactions A character vector of interaction term names. Default is NULL.
#' @param nonlinear_terms A character vector of nonlinear term names. Default is NULL.
#'
#' @return A list containing the original and adjusted beta coefficients with their 95% confidence intervals.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(treatment = c(0,1,0,1), outcome = c(5.2,3.4,2.1,4.5))
#' perr_linear(data, "treatment", "outcome")
#' }
#' @export
perr_linear <- function(data, treatment_var, outcome_var, covariates = NULL, interactions = NULL, nonlinear_terms = NULL) {
  # ... function body ...
}
library(dplyr)
# 线性回归
perr_linear <- function(data, treatment_var, outcome_var, covariates = NULL, interactions = NULL, nonlinear_terms = NULL) {
  
  # 计算先验事件率
  prior_event_rate <- data %>%
    group_by(!!sym(treatment_var)) %>%
    summarise(prior_event_rate = mean(!!sym(outcome_var)), .groups = 'drop')
  
  # 检查分母是否为0
  if (prior_event_rate$prior_event_rate[1] == 0) {
    stop("Error: Denominator of PERR is zero.")
  }
  
  # 计算先验事件率比（PERR）
  perr <- prior_event_rate$prior_event_rate[2] / prior_event_rate$prior_event_rate[1]
  
  # 创建公式
  formula <- paste(outcome_var, "~", treatment_var, 
                   if (!is.null(covariates)) paste("+", paste(covariates, collapse = " + ")), 
                   if (!is.null(interactions)) paste("+", paste(interactions, collapse = " + ")), 
                   if (!is.null(nonlinear_terms)) paste("+", paste(nonlinear_terms, collapse = " + ")))
  
  # 使用PERR调整治疗效果估计
  model <- lm(as.formula(formula), data = data)
  model_adjusted <- lm(as.formula(paste("I(", outcome_var, "/", perr, ") ~", treatment_var, 
                                        if (!is.null(covariates)) paste("+", paste(covariates, collapse = " + ")), 
                                        if (!is.null(interactions)) paste("+", paste(interactions, collapse = " + ")), 
                                        if (!is.null(nonlinear_terms)) paste("+", paste(nonlinear_terms, collapse = " + ")))), data = data)
  
  # 获取β系数和95%CI
  coef_original <- coef(summary(model))
  beta_original <- coef_original[, "Estimate"]
  CI_low_original <- beta_original - 1.96 * coef_original[, "Std. Error"]
  CI_high_original <- beta_original + 1.96 * coef_original[, "Std. Error"]
  
  coef_adjusted <- coef(summary(model_adjusted))
  beta_adjusted <- coef_adjusted[, "Estimate"]
  CI_low_adjusted <- beta_adjusted - 1.96 * coef_adjusted[, "Std. Error"]
  CI_high_adjusted <- beta_adjusted + 1.96 * coef_adjusted[, "Std. Error"]
  
  return(list(original = data.frame(beta = beta_original, CI_low = CI_low_original, CI_high = CI_high_original),
              adjusted = data.frame(beta = beta_adjusted, CI_low = CI_low_adjusted, CI_high = CI_high_adjusted)))
}