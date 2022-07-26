#' Estimating the (statistical) information for a given dataset and estimation method
#'
#' @description A function estimating the (statistical) information based on a data frame and estimation function.
#'
#' @param data A data frame containing the observed data at a given time. This data frame should have the same structure as the data frame outputted by \code{data_at_time_t}.
#' @param totalInformation A numeric value indicating the total/maximum information.
#' @param analysisNumber A numeric value representing the number of the current analysis.
#' @param previousDatasets A list of data frames with same structure as in output of \code{data_at_time_t}. They represent the observed datasets up to the previous analysis time.
#' @param estimationMethod A function naming the function to be called to estimate the treatment effect. Included in this package: \code{standardization} (binary and continuous outcomes), \code{rctmle} (binary and continuous outcomes), \code{survrct_diff} (time-to-event outcomes) and \code{drordinal} (ordinal outcomes).
#' @param estimand A character string indicating the estimand of interest; treatment effect on "difference" scale (continuous or binary outcome), "ratio" scale (binary outcome), odds ratio ("oddsratio") scale (binary outcome), difference in (weighted) means ("weighted_mean" for ordinal outcomes), log odds ratio ("log_odds" for ordinal outcomes), Mann-Whitney ("mann_whitney" for ordinal outcomes), difference in restricted mean survival time ("rmst" for time-to-event outcome) or difference in survival probabilities ("survprob" for time-to-event outcome).
#' @param update A character string indicating whether information needs to be calculated based on the updated estimate ("yes") or based on the original estimate ("no").
#' @param previousEstimatesOriginal A vector of numeric values containing the (original) estimates up to the previous analysis.
#' @param previousCovMatrixOriginal A matrix of numeric values respresenting the covariance matrix of the (original) estimates up to the previous analysis.
#' @param parametersPreviousEstimators A list of a list with the parameters used for the previous estimates. One can also change the estimation method by adding \code{estimationMethod} to the different lists corresponding with the different analyses.
#' @param correction Should a small sample correction be included?
#' @param ... Further arguments for the estimator function or calculation of the variance (ie, number of bootstraps).
#'
#' @return An object of the class \code{interimInformation}.
#' \describe{
#'   \item{information}{The (estimated) information available in the data frame \code{data} for the treatment effect of interest.}
#'   \item{informationTime}{The information time, that is, the (estimated) information available relative to the total information.}}
#'
#' @examples
#' \dontrun{
#' colon_cancer_enr = colon_cancer
#' colon_cancer_enr$trt = ifelse(colon_cancer_enr$arm=="Obs", 1, 0)
#' colon_cancer_enr$enrollmentTime = c(rep(1:100, 9), 1:29)
#' colon_cancer_enr$deathTime = colon_cancer_enr$months_to_death +
#' colon_cancer_enr$enrollmentTime
#' analysis_dataset = data_at_time_t(
#' data = colon_cancer_enr,
#' id_column = "id",
#' analysis_time = 50,
#' enrollment_time = "enrollmentTime",
#' treatment_column = "trt",
#' covariate_columns = c("age" , "sex"),
#' outcome_columns = c("event_death"),
#' outcome_times =  c("deathTime"),
#' time_to_event = TRUE
#' )
#' interimInformation(data = analysis_dataset,
#' totalInformation = 5000,
#' analysisNumber=1,
#' estimationMethod = survrct_diff,
#' estimand = "survprob",
#' update="no",
#' outcome.formula=Surv(.time_to_event_1, event_death) ~ trt + age,
#' trt.formula=trt ~ 1,
#' horizon = 14,
#' bootstraps=10)
#' }
#'
#' @export
interimInformation = function(data,
                              totalInformation,
                              analysisNumber,
                              previousDatasets=list(),
                              estimationMethod,
                              estimand = "difference",
                              update,
                              previousEstimatesOriginal=c(),
                              previousCovMatrixOriginal=c(),
                              parametersPreviousEstimators = NULL,
                              correction="no",
                              ...){

  if(!(estimand %in% c("difference", "ratio", "oddsratio", "weighted_mean",
                       "log_odds", "mann_whitney", "rmst", "survprob"))){
    stop("Estimand must be one of the following: difference, ratio, oddsratio, weighted_mean,
                            log_odds, mann_whitney, rmst, or survprob")
  }

  if(length(parametersPreviousEstimators)!=0 &
     length(parametersPreviousEstimators)!=length(previousEstimatesOriginal)){
    stop("The parameters corresponding with previous estimators (i.e., parametersPreviousEstimators) should be a list with same length of previousEstimatesOriginal.")
  }


  ellipsis_args = as.list(substitute(list(...)))[-1L]

  # Estimate (original) treatment effect and corresponding variance
  all_args = c(list(data=data, estimationMethod=estimationMethod, estimand=estimand), ellipsis_args)
  estimate = do.call(what = calculateEstimate,
                     args = all_args)

  if(correction=="yes"){
    correctionTerm = do.call(what = calculateCorrectionTerm,
                             args = all_args[intersect(x=names(all_args),
                                                       y= formalArgs(calculateCorrectionTerm))])

  }else{
    correctionTerm = 1
  }

  if(update == "no" | analysisNumber==1){

    variance = do.call(what = calculateVariance,
                       args = all_args)

    variance = variance*correctionTerm

    # Calculate (original) information and information time
    information = 1/variance
    informationTime = information/totalInformation

  }else{

    all_args_cov = c(all_args,
                     list(previousDatasets=previousDatasets))
    all_args_cov = c(all_args_cov,
                     list(parametersPreviousEstimators = parametersPreviousEstimators))

    covariance = do.call(what = calculateCovariance,
                         args = all_args_cov)

    # Update covariance matrix of original estimates
    covMatrix = cbind(rbind(previousCovMatrixOriginal, covariance[-analysisNumber]), covariance)

    # Update/orthogonalize the original estimate at the (interim) analysis
    # based on the original covariance matrix and
    # the original estimates
    updated = updateEstimate(
      covMatrixOriginal=covMatrix,
      estimatesOriginal=c(previousEstimatesOriginal, estimate))

    information = 1/(updated[[2]]*correctionTerm)
    informationTime = information/totalInformation

  }

  out <- list(information = information,
              informationTime = informationTime)

  class(out) <- "interimInformation"
  return(out)

}
