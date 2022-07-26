% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_at_time_t.R
\name{data_at_time_t}
\alias{data_at_time_t}
\title{Determining dataset at a given analysis time}
\usage{
data_at_time_t(
  data,
  id_column,
  enrollment_time,
  treatment_column,
  covariate_columns,
  outcome_columns,
  outcome_times,
  analysis_time,
  time_to_event = FALSE
)
}
\arguments{
\item{data}{A data frame.}

\item{id_column}{A character string representing the column name of participant identifiers.}

\item{enrollment_time}{A character string representing the column name of the enrollment times.}

\item{treatment_column}{A character string representing the column name of treatment variable.}

\item{covariate_columns}{A vector of character string representing the column names of the covariates.}

\item{outcome_columns}{A vector of character string representing the column names of the outcomes (including intermediate outcomes). For time-to-event endpoints these are the column names of the status indicator.}

\item{outcome_times}{A vector of character strings representing the column names of the outcome times (including intermediate outcomes). For time-to-event endpoints these are the column names of the times a participant got the event or was censored.}

\item{analysis_time}{A numeric value for the time of the analysis.}

\item{time_to_event}{\code{time_to_event=TRUE} for time-to-event endpoints. Default is \code{FALSE}.}
}
\value{
A data frame with the data of the patients already in the study at \code{analysis_time}. In addition to the original dataset, the output dataset has the additional column(s):
\describe{
\item{\code{.r_j}}{which equals \code{1} if outcome j is already observed (for a certain participant) at \code{analysis_time} and \code{NA} otherwise.}
\item{\code{.time_to_event_j}}{the follow-up time corresponding with event j (for time-to-event endpoints only), truncated by the time of the (interim) analysis.}
}
}
\description{
A function that returns the a data frame with the observed data at a given \code{analysis_time}.
}
\examples{
colon_cancer_enr = colon_cancer
colon_cancer_enr$trt = ifelse(colon_cancer_enr$arm=="Obs", 1, 0)
colon_cancer_enr$enrollmentTime = c(rep(1:100, 9), 1:29)
colon_cancer_enr$deathTime = colon_cancer_enr$months_to_death +
colon_cancer_enr$enrollmentTime
analysis_dataset = data_at_time_t(
data = colon_cancer_enr,
id_column = "id",
analysis_time = 50,
enrollment_time = "enrollmentTime",
treatment_column = "trt",
covariate_columns = c("age" , "sex"),
outcome_columns = c("event_death"),
outcome_times =  c("deathTime"),
time_to_event = TRUE
)
analysis_dataset

}
