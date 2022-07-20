#' Standardization estimator
#'
#' @description A function that estimates the treatment effect via a standardization estimator (also called G-computation).
#'
#' @param data A data frame containing the observed data at a given time. This data frame should have the same structure as the data frame outputted by \code{data_at_time_t}.
#' @param estimand A character string indicating the estimand of interest; treatment effect on "difference" scale, "ratio" scale or odds ratio ("oddsratio") scale.
#' @param y0_formula An object of the class \code{formula}, describing the model to be fitted for the outcome under control.
#' @param y1_formula An object of the class \code{formula}, describing the model to be fitted for the outcome under treatment.
#' @param family A description of the error distribution and link function to be used in the model (see \code{family}).
#' @param treatment_column A character string representing the column name of treatment variable.
#'
#' @importFrom stats glm predict
#'
#' @return An object of the class \code{standardization}.
#' \describe{
#'   \item{estimate}{Original estimate based on a standardization estimator (also called G-computation).}
#'   \item{y1_pred}{Predictions of the outcome under treatment for all recruited participants.}
#'   \item{y0_pred}{Predictions of the outcome under control for all recruited participants.}}
#' @export
standardization = function(data,
                           estimand = "difference",
                           y0_formula,
                           y1_formula,
                           family,
                           treatment_column=NULL){

  # Make a dataset with current cohort 1 data;
  # i.e., the cohort of patients used to fit working models
  number = length(names(select(data, contains(".r_"))))
  dataCoh1 = data[which(data[,paste(".r_",number,sep='')]==1),]

  # Fit working models under control and treatment

  y0_mod = glm(y0_formula, family = family, data = dataCoh1[which(dataCoh1[, treatment_column] == 0),])
  y1_mod = glm(y1_formula, family = family, data = dataCoh1[which(dataCoh1[, treatment_column] == 1),])


  # Make predictions under control and treatment for all patients in the dataset
  y0_pred = predict(y0_mod, newdata = data, type = "response")
  y1_pred = predict(y1_mod, newdata = data, type = "response")

  # Estimate treatment effect
  if(estimand == "difference"){

    estimate = mean(y1_pred) - mean(y0_pred)

  }else{

    if(estimand == "ratio"){

      estimate = mean(y1_pred)/mean(y0_pred)

    }else{

      if(estimand == "oddsratio"){

        estimate = (mean(y1_pred)/(1-mean(y1_pred)))/
          (mean(y0_pred)/(1-mean(y0_pred)))

      }
    }

  }

  out <- list(estimate = estimate,
              y1_pred = y1_pred,
              y0_pred = y0_pred)

  class(out) <- "standardization"
  return(out)
}
