#' Estimating estimands for ordinal endpoints
#'
#' @description A function that estimates the treatment effect wrt an ordinal outcome via a targeted maximum likelihood estimator (using the estimator function \code{drord} in package \code{drord}).
#'
#' @param data A data frame containing the observed data at a given time.
#' @param estimand A character string indicating the estimand of interest (for an ordinal endpoint); difference in (weighted) means ("weighted_mean"), log odds ratio ("log_odds") and Mann-Whitney ("mann_whitney").
#' @param outcome_column A character string representing the column name of the outcome variable.
#' @param treatment_column A character string representing the column name of the treatment variable.
#' @param covariate_columns A vector of character string representing the column names of the covariates to include in the working proportional odds model (see function \code{drord} in apckage \code{drord}).
#' @param ... Further arguments for the estimator function \code{drord} in package \code{drord}.
#'
#' @import drord
#'
#' @return An estimated value for the (chosen) treatment effect based on the observed dataset and estimation method.
#'
#' @examples
#' ctn03_sim$arm_num = 1*(ctn03_sim$arm == "7-day")
#' ctn03_sim$cows_category_eot_num = as.numeric(ctn03_sim$cows_category_eot)
#' drordinal(data = ctn03_sim,
#' estimand = c("weighted_mean"),
#' outcome_column = "cows_category_eot_num",
#' treatment_column = "arm_num",
#' covariate_columns = c("stability_dose",
#'                      "arsw_score_bl", "cows_total_score_bl",
#'                      "vas_crave_opiates_bl",
#'                      "vas_current_withdrawal_bl",
#'                      "vas_study_tx_help_bl",
#'                      "uds_opioids_bl", "uds_oxycodone_bl",
#'                      "uds_any_positive_bl"))
#'
#' @export
drordinal <- function(data,
                      estimand = c("weighted_mean", "log_odds", "mann_whitney")[1],
                      outcome_column,
                      treatment_column,
                      covariate_columns,
                      ...) {

  if(!(estimand %in% c("weighted_mean", "log_odds", "mann_whitney"))){
    stop("Estimand for the function drordinal must be one of the following: weighted_mean,
                            log_odds, or mann_whitney")
  }

  ellipsis_args = as.list(substitute(list(...)))[-1L]

  all_args = c(list(out = data[,outcome_column], treat = data[,treatment_column],
                    covar = data.frame(data[,covariate_columns])), ellipsis_args)

  fit = do.call(what = drord,
                args = all_args)

  if(estimand == "weighted_mean"){
    ate <- fit$weighted_mean$est$est[3]
  } else if (estimand == "log_odds"){
    ate <- fit$log_odds$est[3]
  } else if (estimand == "mann_whitney"){
    ate <- fit$mann_whitney$est
  }

  return(ate)
}
