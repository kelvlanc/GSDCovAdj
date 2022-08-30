#' Calculating an estimate based on a given dataset
#'
#' @description A function estimating the treatment effect for a given data frame and estimator.
#'
#' @param data A data frame containing the observed data at a given time. This data frame should have the same structure as the data frame outputted by \code{data_at_time_t}.
#' @param estimationMethod A function naming the function to be called to estimate the treatment effect. Included in this package: \code{standardization} and \code{rctmle}.
#' @param estimand A character string indicating the estimand of interest; treatment effect on "difference" scale, "ratio" scale or odds ratio ("oddsratio") scale.
#' @param ... Further arguments for the estimator function.
#'
#' @importFrom methods formalArgs
#'
#' @return An estimated value for the treatment effect based on the observed dataset and estimation method.
calculateEstimate = function(data,
                             estimationMethod,
                             estimand = "difference",
                             ...){

  ellipsis_args = as.list(substitute(list(...)))[-1L]
  all_args = c(list(data=data, estimand=estimand), ellipsis_args)


  # Estimate the treatment effect using the function 'estimationMethod'
  estimate = do.call(what = estimationMethod,
                     args = all_args[intersect(x=names(all_args),
                                               y= formalArgs(estimationMethod))])[[1]]
  estimate

}

#' Calculating the variance of the estimated treatment effect
#'
#' @description A function that calculates the variance of the estimated treatment effect.
#'
#' @param data A data frame containing the observed data at a given time. This data frame should have the same structure as the data frame outputted by \code{data_at_time_t}.
#' @param estimationMethod A function naming the function to be called to estimate the treatment effect. Included in this package: \code{standardization} and \code{rctmle}.
#' @param estimand A character string indicating the estimand of interest; treatment effect on "difference" scale, "ratio" scale or odds ratio ("oddsratio") scale.
#' @param bootstraps The number of bootstraps to calculate the variance.
#' @param ... Further arguments for the estimator function.
#'
#' @importFrom stats var
#' @importFrom parallel mclapply
#'
#' @return An estimated value for the variance of the estimated treatment effect based on the observed dataset and estimation method.
calculateVariance = function(data,
                             estimationMethod,
                             estimand = "difference",
                             bootstraps = 2500,
                             ...){

  ellipsis_args = as.list(substitute(list(...)))[-1L]

  # Use bootstrap to estimate variance
  set.seed(123, "L'Ecuyer")
  med.boot = mclapply(1:bootstraps, function(i) {
    dataNew = data[sample(nrow(data), nrow(data), replace = TRUE), ]

    all_args = c(list(data=dataNew, estimationMethod=estimationMethod, estimand=estimand), ellipsis_args)
    estimate = do.call(what = calculateEstimate,
                       args = all_args)

    estimate

  }, mc.cores = 16)


  # Calculate variance based on the different bootstrap estimates
  variance = var(unlist(med.boot))

  variance

}

#' Calculating the covariance of a sequence of estimated treatment effects
#'
#' @description A function calculating the covariance of the current and previous estimates of the tretment effect.
#'
#' @param data A data frame containing the observed data at a given time. This data frame should have the same structure as the data frame outputted by \code{data_at_time_t}.
#' @param estimationMethod A function naming the function to be called to estimate the treatment effect. Included in this package: \code{standardization} and \code{rctmle}.
#' @param estimand A character string indicating the estimand of interest; treatment effect on "difference" scale, "ratio" scale or odds ratio ("oddsratio") scale.
#' @param bootstraps The number of bootstraps to calculate the covariance.
#' @param previousDatasets A list of data frames with same structure as in output of \code{data_at_time_t}. They represent the observed datasets up to the previous analysis time.
#' @param parametersPreviousEstimators A list of a list with the parameters used for the previous estimates. One can also change the estimation method by adding \code{estimationMethod} to the different lists corresponding with the different analyses.
#' @param ... Further arguments for the estimator function.
#'
#' @importFrom stats cov
#'
#' @return A vector of the estimated covariance(s) between the current estimate and previous estimate(s) as well as the estimated variance of the current estimate.
calculateCovariance = function(data,
                               estimationMethod,
                               estimand = "difference",
                               bootstraps = 2500,
                               previousDatasets=list(),
                               parametersPreviousEstimators = NULL,
                               ...
){

  ellipsis_args = as.list(substitute(list(...)))[-1L]

  analysisNumber = length(previousDatasets)+1

  # Use bootstrap to estimate variance and covariances
  set.seed(123, "L'Ecuyer")
  med.boot = mclapply(1:bootstraps, function(i) {
    #dataNew = data[sample(nrow(data), nrow(data), replace = TRUE), ]
    selection = sample(data$id, nrow(data), replace = TRUE)
    selectionFreq = as.data.frame(table(selection))

    sub_sample_df = data[data$id%in%selection,]
    match_freq = selectionFreq[match(sub_sample_df$id, selectionFreq$selection),]

    sub_sample_df$Freq = match_freq$Freq
    selected_rows = rep(1:nrow(sub_sample_df), sub_sample_df$Freq)
    dataNew = sub_sample_df[selected_rows,]

    estimate=c()

    all_args = c(list(data = dataNew, estimationMethod = estimationMethod, estimand = estimand), ellipsis_args)
    estimate[analysisNumber]  = do.call(what = calculateEstimate,
                                        args = all_args)[[1]]

    for(j in 1:(analysisNumber-1)){

      dataNew1 = previousDatasets[[j]]

      sub_sample_df = dataNew1[dataNew1$id%in%selection,]
      match_freq = selectionFreq[match(sub_sample_df$id, selectionFreq$selection),]

      sub_sample_df$Freq = match_freq$Freq
      selected_rows = rep(1:nrow(sub_sample_df), sub_sample_df$Freq)
      dataNew1 = sub_sample_df[selected_rows,]


      if(is.list(parametersPreviousEstimators)==TRUE){

        ellipsis_args_previous = ellipsis_args
        ellipsis_args_previous[names(parametersPreviousEstimators[[j]])]=parametersPreviousEstimators[[j]]

        if("estimationMethod" %in% names(parametersPreviousEstimators[[j]])){

          all_args = c(list(data = dataNew1, estimand = estimand), ellipsis_args_previous)
          estimate[j]  = do.call(what = calculateEstimate,
                                 args = all_args)[[1]]

        }else{

          all_args = c(list(data = dataNew1, estimationMethod = estimationMethod, estimand = estimand), ellipsis_args_previous)
          estimate[j]  = do.call(what = calculateEstimate,
                                 args = all_args)[[1]]

        }

      }else{
        all_args = c(list(data = dataNew1, estimationMethod = estimationMethod, estimand = estimand), ellipsis_args)
        estimate[j]  = do.call(what = calculateEstimate,
                               args = all_args)[[1]]
      }

    }

    estimate

  }, mc.cores = 16)

  # Calculate covariances and variance based on the different bootstrap estimates
  dataCov = data.frame(matrix(unlist(med.boot), nrow=length(med.boot), byrow=TRUE))

  covariance = cov(dataCov)[analysisNumber,]

  covariance

}


#' Orthogonalizing the original sequence of estimates up to the current analysis
#'
#' @description A function orthogonalizing the original sequence of estimates up to the current analysis.
#'
#' @param covMatrixOriginal A matrix of numeric varlues respresenting the covariance matrix of the (original) estimates up to the previous analysis.
#' @param estimatesOriginal A vector of numeric values containing the (original) estimates up to the previous analysis.
#'
#' @return An object of the class \code{updateEstimate}.
#' \describe{
#'   \item{estimateUpdated}{The updated/orthogonalized estimate at the current analysis.}
#'   \item{varianceUpdated}{The estimated variance of the updated/orthogonalized estimate at the current analysis.}}
updateEstimate = function(covMatrixOriginal,
                          estimatesOriginal){

  # k is the analysis for which we want to update the estimate
  k = length(estimatesOriginal)

  # covMatrixProjection is the kxk covariance matrix of
  # (theta_k-theta_1, ..., theta_k-theta_{k-1}, theta_k)
  covMatrixProjection = matrix(1, k, k)*covMatrixOriginal[k,k] -
    as.vector(rep(1, k))%*%t(as.vector(c(covMatrixOriginal[1:k-1,k], 0))) -
    t(as.vector(rep(1, k))%*%t(as.vector(c(covMatrixOriginal[1:k-1,k], 0)))) +
    rbind(cbind(covMatrixOriginal[1:k-1,1:k-1], 0), 0)

  # A is the Cholesky decomposition of covMatrixProjection
  A = chol(covMatrixProjection)
  # Atilde is a kx(k-1) matrix with the first k-1 columns of A
  Atilde = A[,-k]
  # Ak is a kx1 vector equal to the kth column of A
  Ak = A[,k]

  # W equals the vector # (theta_k-theta_1, ..., theta_k-theta_{k-1}, theta_k)
  W = c(estimatesOriginal[k] - estimatesOriginal[1:k-1], estimatesOriginal[k])

  # scale are the orthogonalizing 'coefficients'
  scale = solve(A)%*%(diag(k)-Atilde%*%solve(t(Atilde)%*%Atilde)
                      %*%t(Atilde))%*%Ak

  # updatedEstimate is the updated/orthogonalized estimate at analysis k
  estimateUpdated = t(scale)%*%W

  # updatedVariance is the variance of the
  # updated/orthogonalized estimate at analysis k
  varianceUpdated = t(scale)%*%t(A)%*%A%*%scale

  out <- list(estimateUpdated = estimateUpdated,
                    varianceUpdated = varianceUpdated)

  class(out) <- "updateEstimate"
  return(out)

}

#' Calculating the correction term for the estimated variance.
#'
#' @description A function that calculates the correction term for the estimated variance.
#'
#' @param data A data frame containing the observed data at a given time. This data frame should have the same structure as the data frame outputted by \code{data_at_time_t}.
#' @param outcome_formula The outcome model to be fitted, when using \code{rctmle} as \code{estimationMethod}.
#' @param y0_formula An object of the class \code{formula}, describing the model to be fitted for the outcome under control when using \code{standardization} as \code{estimationMethod}.
#' @param y1_formula An object of the class \code{formula}, describing the model to be fitted for the outcome under treatment when using \code{standardization} as \code{estimationMethod}.
#' @param estimationMethod A function naming the function to be called to estimate the treatment effect. Included in this package: \code{standardization} and \code{rctmle}.
#' @param treatment_column A character string representing the column name of treatment variable.
#'
#' @importFrom stats model.matrix
#' @importFrom dplyr select contains
#'
#' @return A numeric value representing a small sample size correction term for the estimated variance.
calculateCorrectionTerm = function(data,
                                   outcome_formula=NULL,
                                   y0_formula=NULL,
                                   y1_formula=NULL,
                                   estimationMethod,
                                   treatment_column=NULL){

  if(estimationMethod==rctmle){
    number = length(names(select(data, contains(".r_"))))
    n=length(which(data[,paste(".r_",number,sep='')]==1))
    p=dim(model.matrix(outcome_formula[[number]], data))[2]
    correctionTerm = (n-1)/(n-p)
  }else{
    if(estimationMethod==standardization){
      number = length(names(select(data, contains(".r_"))))
      n1=length(which(data[,paste(".r_",number,sep='')]==1&data[,treatment_column]==1))
      n0=length(which(data[,paste(".r_",number,sep='')]==1&data[,treatment_column]==0))
      p0=dim(model.matrix(y0_formula, data))[2]
      p1=dim(model.matrix(y1_formula, data))[2]
      correctionTerm = (1/(n1-p1)+1/(n0-p0))/(1/(n1-1)+1/(n0-1))

    }
  }

  correctionTerm
}


#' Making a decision based on a given test statistic.
#'
#' @description A function that makes a decision based on a given test statistic corresponding with the current analysis.
#'
#' @param testStatistic A numeric value for the test statistic corresponding with the current analysis.
#' @param alpha The (total/cumulative) siginificance level alpha.
#' @param beta Type II error rate.
#' @param alternative A character string specifying the alternative hypothesis. The alternative is one of the following: two-sided (\code{two.sided}; default), one-sided less (\code{less}) or one-sided more (\code{more}).
#' @param typeOfDesign The type of design (see \code{getDesignGroupSequential} in \code{rpact} package).
#' @param typeBetaSpending The type of beta spending (see \code{getDesignGroupSequential} in \code{rpact} package).
#' @param plannedAnalyses A numeric value specifying the number of planned analyses in the group sequential design.
#' @param plannedInformationTimes A vector of numeric values representing the information times at which analyses (interim and final) are planned. This only needs to be specified if \code{futilityStopping=TRUE}.
#' @param previousInformationTimes A vector or numeric values representing the information times of the estimates up to the previous analysis.
#' @param currentInformationTime The information time corresponding with the estimate at the current analysis.
#' @param futilityStopping \code{futilityStopping=TRUE} if futility stopping is allowed. Default is \code{FALSE}. Note that futility stopping is only possible for one-sided testing.
#' @param ... Further arguments for the arguments of \code{getDesignGroupSequential} in \code{rpact} package.
#'
#' @importFrom rpact getDesignGroupSequential
#'
#' @return A string value indicating whether one can stop for \code{"Futility"}, \code{"Efficacy"} or whether one can not stop (\code{"No"}).
interimDecision = function(testStatistic,
                           alpha = 0.05,
                           beta = 0.20,
                           alternative = "two.sided",
                           typeOfDesign = "asOF",
                           typeBetaSpending = "none",
                           plannedAnalyses,
                           plannedInformationTimes = NULL,
                           previousInformationTimes,
                           currentInformationTime,
                           futilityStopping = FALSE,
                           ...
){

  ellipsis_args = as.list(substitute(list(...)))[-1L]

  # Number of current analysis
  analysisNumber=length(previousInformationTimes)+1

  # Make decision based on test statistics and bounds

  if(alternative=="two.sided"){

    if(analysisNumber==plannedAnalyses){

      all_args2_final = c(list(sided = 2, alpha = alpha, beta = beta,
                               typeOfDesign = typeOfDesign,
                               typeBetaSpending = typeBetaSpending,
                               informationRates = c(previousInformationTimes, 1)),
                          ellipsis_args)
      bound = do.call(what = getDesignGroupSequential,
                      args = all_args2_final)


    }else{

      all_args2_interim = c(list(sided = 2, alpha=alpha, beta = beta,
                                 typeOfDesign=typeOfDesign,
                                 typeBetaSpending = typeBetaSpending,
                                 informationRates = c(previousInformationTimes,
                                                      currentInformationTime,
                                                      plannedInformationTimes[-c(1:analysisNumber)])),
                            ellipsis_args)
      bound = do.call(what = getDesignGroupSequential,
                      args = all_args2_interim)

    }

    decision = ifelse(abs(testStatistic)>bound$criticalValues[analysisNumber],
                      "Efficacy", "No")

  }else{

    if(analysisNumber==plannedAnalyses){

      all_args1_final = c(list(sided = 1, alpha=alpha, beta = beta,
                               typeOfDesign=typeOfDesign,
                               typeBetaSpending = typeBetaSpending,
                               informationRates = c(previousInformationTimes, 1)),
                          ellipsis_args)
      bound = do.call(what = getDesignGroupSequential,
                      args = all_args1_final)

      if(alternative=="less"){

        decision = ifelse(testStatistic<(-bound$criticalValues[analysisNumber]),
                          "Efficacy", "No")

      }else{

        decision = ifelse(testStatistic>bound$criticalValues[analysisNumber],
                          "Efficacy", "No")

      }

    }else{

      all_args1_interim = c(list(sided = 1, alpha=alpha, beta = beta,
                                 typeOfDesign=typeOfDesign,
                                 typeBetaSpending = typeBetaSpending,
                                 informationRates = c(previousInformationTimes,
                                                      currentInformationTime,
                                                      plannedInformationTimes[-c(1:analysisNumber)])),
                            ellipsis_args)
      bound = do.call(what = getDesignGroupSequential,
                      args = all_args1_interim)

      if(futilityStopping == FALSE){

        if(alternative=="less"){

          decision = ifelse(testStatistic<(-bound$criticalValues[analysisNumber]),
                            "Efficacy", "No")

        }else{

          decision = ifelse(testStatistic>bound$criticalValues[analysisNumber],
                            "Efficacy", "No")

        }
      }else{
        if(alternative=="less"){

          decision = ifelse(testStatistic<(-bound$criticalValues[analysisNumber]),
                            "Efficacy", ifelse(testStatistic>(-bound$futilityBounds[analysisNumber]),
                                               "Futility", "No"))

        }else{

          decision = ifelse(testStatistic>bound$criticalValues[analysisNumber],
                            "Efficacy", ifelse(testStatistic<bound$futilityBounds[analysisNumber],
                                               "Futility", "No"))

        }
      }

    }

  }

  decision

}
