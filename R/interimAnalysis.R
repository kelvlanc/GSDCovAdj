#' Performing an (interim) analysis based on a given dataset
#'
#' @description A function performing an (interim) analysis based on a given data frame for a chosen estimator.
#'
#' @param data A data frame containing the observed data at a given time. This data frame should have the same structure as the data frame outputted by \code{data_at_time_t}.
#' @param totalInformation A numeric value indicating the total/maximum information.
#' @param estimationMethod A function naming the function to be called to estimate the treatment effect. Included in this package: \code{standardization} and \code{tmle}.
#' @param estimand A character string indicating the estimand of interest; treatment effect on "difference" scale, "ratio" scale or odds ratio ("oddsratio") scale.
#' @param previousEstimatesOriginal A vector of numeric values containing the (original) estimates up to the previous analysis.
#' @param previousCovMatrixOriginal A matrix of numeric varlues respresenting the covariance matrix of the (original) estimates up to the previous analysis.
#' @param previousInformationTimesOriginal A vector or numeric values representing the information times of the original estimates up to the previous analysis.
#' @param previousInformationTimesUpdated A vector or numeric values representing the information times of the updated estimates up to the previous analysis.
#' @param previousDatasets A list of data frames with same structure as in output of \code{data_at_time_t}. They represent the observed datasets up to the previous analysis time.
#' @param null.value A numeric value indicating the value of the treatment effect under the null hypothesis.
#' @param alpha The (total) siginificance level alpha.
#' @param beta Type II error rate.
#' @param alternative A character string specifying the alternative hypothesis. The alternative is one of the following: two-sided (\code{two.sided}; default), one-sided less (\code{less}) or one-sided more (\code{more}).
#' @param typeOfDesign The type of design (see \code{getDesignGroupSequential} in \code{rpact} package).
#' @param typeBetaSpending The type of beta spending (see \code{getDesignGroupSequential} in \code{rpact} package).
#' @param futilityStopping \code{futilityStopping=TRUE} if futility stopping is allowed. Default is \code{FALSE}. Note that futility stopping is only possible for one-sided testing.
#' @param plannedAnalyses A numeric value specifying the number of planned analyses in the group sequential design.
#' @param plannedInformationTimes A vector of numeric values representing the information times at which analyses (interim and final) are planned. This only needs to be specified if \code{futilityStopping=TRUE}.
#' @param parametersPreviousEstimators A list of a list with the parameters used for the previous estimates. One can also change the estimation method by adding \code{estimationMethod} to the different lists corresponding with the different analyses.
#' @param correction Should a small sample correction be included?
#' @param ... Further arguments for the estimator function,calculation of variance/covariance (ie, number of bootstraps) or arguments for \code{getDesignGroupSequential} in \code{rpact} package.
#'
#'
#'
#' @return An object of the class \code{interimAnalysis}.
#' \describe{
#'   \item{estimateOriginal}{The original estimate at the current analysis.}
#'   \item{standardErrorOriginal}{The estimated standard error of original estimate at the current analysis.}
#'   \item{testStatisticOriginal}{The test statistic corresponding with the original estimate at the current analysis.}
#'   \item{decisionOriginal}{The decision corresponding with the original estimate at the current analysis.}
#'   \item{informationTimeOriginal}{The information time corresponding with the original estimate at the current analysis.}
#'   \item{estimateUpdated}{The updated/orthogonalized estimate at the current analysis.}
#'   \item{standardErrorUpdated}{The estimated standard error of the updated/orthogonalized estimate at the current analysis.}
#'   \item{testStatisticUpdated}{The test statistic corresponding with the updated estimate at the current analysis.}
#'   \item{decisionUpdated}{The decision corresponding with the updated estimate at the current analysis: A string value indicating whether one can stop for \code{"Futility"}, \code{"Efficacy"} or whether one can not stop (\code{"No"}).}
#'   \item{informationTimeUpdated}{The information time corresponding with the updated estimate at the current analysis.}
#'   \item{covMatrixOriginal}{A matrix of numeric values representing the covariance matrix of the original estimates up to the current analysis.}}
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
#' interim1=interimAnalysis(data = analysis_dataset,
#' totalInformation=5000,
#' estimationMethod= survrct_diff,
#' estimand = "survprob",
#' previousEstimatesOriginal=c(),
#' previousCovMatrixOriginal=c(),
#' previousInformationTimesOriginal=c(),
#' previousInformationTimesUpdated=c(),
#' previousDatasets = list(),
#' null.value = 0,
#' alpha = 0.05,
#' beta = 0.2,
#' alternative = "two.sided",
#' typeOfDesign = "asP",
#' typeBetaSpending = "none",
#' futilityStopping = FALSE,
#' plannedAnalyses=2,
#' plannedInformationTimes = NULL,
#' parametersPreviousEstimators = NULL,
#' correction="no",
#' bootstraps =100,
#' outcome.formula=Surv(.time_to_event_1, event_death) ~ trt + age,
#' trt.formula=trt ~ 1,
#' horizon = 14)
#' interim1
#' final = interimAnalysis(data = colon_cancer,
#'                         totalInformation=5000,
#'                         estimationMethod= survrct_diff,
#'                         estimand = "survprob",
#'                         previousEstimatesOriginal=as.numeric(interim1[[1]]),
#'                         previousCovMatrixOriginal=interim1[[11]],
#'                         previousInformationTimesOriginal=as.numeric(interim1[[5]]),
#'                         previousInformationTimesUpdated=as.numeric(interim1[[10]]),
#'                         previousDatasets = list(analysis_dataset),
#'                         null.value = 0,
#'                         alpha = 0.05,
#'                         beta = 0.2,
#'                         alternative = "two.sided",
#'                         typeOfDesign = "asP",
#'                         typeBetaSpending = "none",
#'                         futilityStopping = FALSE,
#'                         plannedAnalyses=2,
#'                         bootstraps=100,
#'                         plannedInformationTimes = NULL,
#'                         parametersPreviousEstimators =
#'                         list(list(outcome.formula=Surv(.time_to_event_1, event_death) ~ trt + age,
#'                         trt.formula=trt ~ 1,
#'                         horizon = 14)),
#'                         correction="no",
#'                         outcome.formula=Surv(months_to_death, event_death) ~ trt + age,
#'                         trt.formula=trt ~ 1,
#'                         horizon = 14)
#' final
#' }
#'
#' @export
interimAnalysis = function(data,
                           totalInformation,
                           estimationMethod,
                           estimand = "difference",
                           previousEstimatesOriginal=c(),
                           previousCovMatrixOriginal=c(),
                           previousInformationTimesOriginal=c(),
                           previousInformationTimesUpdated=c(),
                           previousDatasets=list(),
                           null.value = 0,
                           alpha = 0.05,
                           beta = 0.20,
                           alternative = "two.sided",
                           typeOfDesign = "asOF",
                           typeBetaSpending = "none",
                           futilityStopping = FALSE,
                           plannedAnalyses,
                           plannedInformationTimes = NULL,
                           parametersPreviousEstimators = NULL,
                           correction="no",
                           ...){

  stopifnot(alpha>0 & alpha<1)
  stopifnot(beta>0 & beta<1)
  stopifnot(length(plannedInformationTimes) == plannedAnalyses,
            length(previousEstimatesOriginal)==
              length(previousInformationTimesOriginal),
            length(previousEstimatesOriginal)==
              length(previousInformationTimesUpdated),
            length(previousEstimatesOriginal)==
              length(previousDatasets))

  if(!(estimand %in% c("difference", "ratio", "oddsratio", "weighted_mean",
                            "log_odds", "mann_whitney"))){
    stop("Estimand must be one of the following: difference, ratio, oddsratio, weighted_mean,
                            log_odds, or mann_whitney")
  }

  if(length(parametersPreviousEstimators)!=0 &
     length(parametersPreviousEstimators)!=length(previousInformationTimesOriginal)){
    stop("The parameters corresponding with previous estimators (i.e., parametersPreviousEstimators) should be a list with same length of previousInformationTimesOriginal.")
  }

  # Number of current analysis
  analysisNumber = length(previousInformationTimesOriginal)+1

  ellipsis_args = as.list(substitute(list(...)))[-1L]

  # Estimate (original) treatment effect and corresponding variance
  # based on the estimator in Appendix C
  all_args = c(list(data=data, estimationMethod=estimationMethod, estimand=estimand), ellipsis_args)
  estimateOriginal = do.call(what = calculateEstimate,
                             args = all_args)


  # Update covariance matrix of original estimates

  if(analysisNumber==1){
    variance = do.call(what = calculateVariance,
                       args = all_args)
    covMatrixOriginal = variance

    standardErrorOriginal = sqrt(variance)
  }else{

    all_args_cov = c(all_args,
                     list(previousDatasets=previousDatasets))
    all_args_cov = c(all_args_cov,
                     list(parametersPreviousEstimators = parametersPreviousEstimators))

    covariance = do.call(what = calculateCovariance,
                         args = all_args_cov)
    covMatrixOriginal = cbind(rbind(previousCovMatrixOriginal, covariance[-analysisNumber]), covariance)

    standardErrorOriginal = sqrt(covariance[analysisNumber])
  }

  if(correction=="yes"){
    correctionTerm = do.call(what = calculateCorrectionTerm,
                             args = all_args[intersect(x=names(all_args),
                                                       y= formalArgs(calculateCorrectionTerm))])

  }else{
    correctionTerm = 1
  }


  # Calculate (original) SE, information, information time and test statistic
  standardErrorOriginal = standardErrorOriginal*sqrt(correctionTerm)
  informationOriginal=1/(standardErrorOriginal)^2
  informationTimeOriginal = informationOriginal/totalInformation
  testStatisticOriginal = estimateOriginal/standardErrorOriginal



  if(analysisNumber==1){

    # Calculate updated variance and
    # calculate (updated) SE, information, information time and test statistic
    estimateUpdated = estimateOriginal
    standardErrorUpdated = standardErrorOriginal
    informationUpdated = informationOriginal
    informationTimeUpdated = informationTimeOriginal
    testStatisticUpdated = testStatisticOriginal

  }else{

    # Update/orthogonalize the original estimate at the (interim) analysis
    # based on the original covariance matrix and
    # the original estimates
    updated = updateEstimate(
      covMatrixOriginal=covMatrixOriginal,
      estimatesOriginal=c(previousEstimatesOriginal, estimateOriginal))
    estimateUpdated = updated[[1]]
    standardErrorUpdated = sqrt(updated[[2]])*sqrt(correctionTerm)
    informationUpdated=1/(standardErrorUpdated)^2
    informationTimeUpdated = informationUpdated/totalInformation
    testStatisticUpdated = estimateUpdated/standardErrorUpdated

  }


  all_args_decisionOriginal = c(list(testStatistic=testStatisticOriginal,
                                     alpha = alpha, beta = beta, alternative = alternative,
                                     typeOfDesign = typeOfDesign,
                                     typeBetaSpending = typeBetaSpending,
                                     plannedAnalyses = plannedAnalyses,
                                     plannedInformationTimes = plannedInformationTimes,
                                     previousInformationTimes = previousInformationTimesOriginal,
                                     currentInformationTime = informationTimeOriginal,
                                     futilityStopping = futilityStopping), ellipsis_args)

  decisionOriginal = do.call(what = interimDecision,
                             args = all_args_decisionOriginal)

  all_args_decisionUpdated = c(list(testStatistic=testStatisticUpdated,
                                    alpha = alpha, beta = beta, alternative = alternative,
                                    typeOfDesign = typeOfDesign,
                                    typeBetaSpending = typeBetaSpending,
                                    plannedAnalyses = plannedAnalyses,
                                    plannedInformationTimes = plannedInformationTimes,
                                    previousInformationTimes = previousInformationTimesUpdated,
                                    currentInformationTime = informationTimeUpdated,
                                    futilityStopping = futilityStopping), ellipsis_args)

  decisionUpdated = do.call(what = interimDecision,
                            args = all_args_decisionUpdated)

  out <- list(estimateOriginal = estimateOriginal,
              standardErrorOriginal = standardErrorOriginal,
              testStatisticOriginal = testStatisticOriginal,
              decisionOriginal = decisionOriginal,
              informationTimeOriginal = informationTimeOriginal,
              estimateUpdated = estimateUpdated,
              standardErrorUpdated = standardErrorUpdated,
              testStatisticUpdated = testStatisticUpdated,
              decisionUpdated = decisionUpdated,
              informationTimeUpdated = informationTimeUpdated,
              covMatrixOriginal = covMatrixOriginal)

  class(out) <- "interimAnalysis"
  return(out)

}

