#' Determining dataset corresponding with a given analysis time
#'
#' @description A function that returns a data frame with the observed data at a given \code{analysis_time}.
#'
#' @param data A data frame.
#' @param id_column A character string representing the column name of the participant identifiers.
#' @param enrollment_time A character string representing the column name of the enrollment times.
#' @param treatment_column A character string representing the column name of the treatment variable.
#' @param covariate_columns A vector of character strings representing the column names of the covariates.
#' @param outcome_columns A vector of character strings representing the column names of the outcomes (including intermediate outcomes). For time-to-event endpoints these are the column names of the status indicator.
#' @param outcome_times A vector of character strings representing the column names of the outcome times (including intermediate outcomes). For time-to-event endpoints these are the column names of the times a participant got the event or was censored.
#' @param analysis_time A numeric value for the time of the analysis.
#' @param time_to_event \code{time_to_event=TRUE} for time-to-event endpoints. Default is \code{FALSE}.
#'
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#' @importFrom magrittr %>%
#'
#' @return A data frame with the data of the patients already in the study at \code{analysis_time}. In addition to the original dataset, the output dataset has the additional column(s):
#' \describe{
#' \item{\code{.r_j}}{which equals \code{1} if outcome j is already observed (for a certain participant) at \code{analysis_time} and \code{NA} otherwise.}
#' \item{\code{.time_to_event_j}}{the follow-up time corresponding with event j (for time-to-event endpoints only), truncated by the time of the (interim) analysis.}
#' }
#'
#' @examples
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
#' analysis_dataset
#'
#' @export
data_at_time_t <-
  function(
    data,
    id_column,
    enrollment_time,
    treatment_column,
    covariate_columns,
    outcome_columns,
    outcome_times,
    analysis_time,
    time_to_event = FALSE
  ) {

    # Select only relevant columns
    data <-
      data[ which(data[, enrollment_time] <= analysis_time),
            c(id_column, covariate_columns, enrollment_time,
              treatment_column, outcome_times, outcome_columns)
      ]

    r_matrix <- 1*(!is.na(data[, outcome_columns]))
    r_matrix[which(data[, outcome_times] > analysis_time, arr.ind = TRUE)] <- NA

    for(i in 1:length(outcome_times)){
      data[which(data[, outcome_times[i]] > analysis_time),
           outcome_columns[i]] <- NA
    }

    data <-
      data.frame(
        data,
        setNames(
          object = data.frame(r_matrix),
          nm = paste0(".r_", 1:length(outcome_columns))
        )
      ) %>%
      as_tibble()

    if(time_to_event==TRUE){
      time_to_event_times = paste(".time_to_event_", 1:length(outcome_times), sep="")
      for(i in 1:length(outcome_times)){
        data[,time_to_event_times[i]] = NA
        data[which(!is.na(data[, outcome_columns[i]])),
             time_to_event_times[i]] <- data[which(!is.na(data[, outcome_columns[i]])),
                                             outcome_times[i]]-data[which(!is.na(data[, outcome_columns[i]])),
                                                                    enrollment_time]
        data[which(is.na(data[, outcome_columns[i]])),
             time_to_event_times[i]] <- analysis_time-data[which(is.na(data[, outcome_columns[i]])),
                                                           enrollment_time]
        data[which(is.na(data[, outcome_columns[i]])),
             outcome_columns[i]] <- 0
      }
    }

    return(data)
  }
