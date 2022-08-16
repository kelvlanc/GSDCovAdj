#' Estimating time-to-event estimands
#'
#' @description A function that estimates the treatment effect wrt a time-to-event outcome via a targeted maximum likelihood estimator.
#'
#' @param data A data frame containing the observed data at a given time.
#' @param estimand A character string indicating the estimand of interest (for a time-to-event endpoint); difference in restricted mean survival time ("rmst") or difference in survival probabilities ("survprob").
#' @param outcome.formula See \code{survrct} in package \code{adjrct}.
#' @param trt.formula See \code{survrct} in package \code{adjrct}.
#' @param coarsen See \code{survrct} in package \code{adjrct}.
#' @param algo See \code{survrct} in package \code{adjrct}.
#' @param crossfit See \code{survrct} in package \code{adjrct}.
#' @param horizon See \code{rmst} and \code{survprob} in package \code{adjrct}.
#'
#' @import simul
#' @import adjrct
#'
#' @return An estimated value for the treatment effect based on the observed dataset and estimation method.
#'
#' @examples
#' colon_cancer$trt = ifelse(colon_cancer$arm=="Obs", 1, 0)
#' survrct_diff(data=colon_cancer,
#' estimand = "survprob",
#' outcome.formula=Surv(months_to_death, event_death) ~ trt + age,
#' trt.formula=trt ~ 1,
#' horizon=14)
#'
#' @export
survrct_diff <- function(data,
                         estimand = c("rmst", "survprob")[1],
                         outcome.formula,
                         trt.formula,
                         coarsen = 1,
                         algo = c("glm", "lasso", "rf", "xgboost"),
                         crossfit = TRUE,
                         horizon) {

  surv = survrct(outcome.formula,
                 trt.formula,
                 data,
                 coarsen,
                 algo,
                 crossfit)

  if(estimand == "rmst"){
    return( rmst(surv, horizon)[[3]][[1]]$theta )
  }else{
    if(estimand == "survprob"){
      return( survprob(surv, horizon)[[3]][[1]]$theta )
    }
  }
}
