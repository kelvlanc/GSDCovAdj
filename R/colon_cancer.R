#' Mock colon cancer data set
#'
#' A dataset containing data that is similar in structure to the `colon`
#' dataset in the \code{survival} package. This is a 3-arm trial involving colon cancer:
#' arms include observation, levamisole, and levamisole + 5-Fluorouracil.
#' Baseline covariates `differ` and `nodes` have missing values that are imputed:
#' a single imputation is performed. This simplifies analysis, likely at the
#' expense of underestimation of standard errors. Multiple iterations of the MICE
#' algorithm are performed.
#' Since follow-up is fairly long (upwards of 8 years in some cases), the time
#' scale is coarsened from days to months (365.25/12 ~ 30.4375 days). This
#' coarsening of the time scale is useful when converting to a survival dataset
#' with one row per (person on-study) x (time-unit at risk).
#'
#' @format A data frame with 15,955 rows and 10 columns:
#' \describe{
#'   \item{id}{Patient id}
#'   \item{arm}{Treatment - Obs(ervation), Lev(amisole), Lev(amisole)+5-FU}
#'   \item{age}{Age in years}
#'   \item{sex}{Patient sex (1=male)}
#'   \item{obstruction}{Obstruction of colon by tumour}
#'   \item{perforation}{Perforation of colon}
#'   \item{organ_adherence}{Adherence to nearby organs}
#'   \item{positive_nodes}{Number of lymph nodes with detectable cancer}
#'   \item{differentiation}{Differentiation of tumour (`1. Well`, `2. Moderate` or `3. Poor`)}
#'   \item{local_spread}{Extent of local spread (`1. Submucosa`, `2. Muscle`, `3. Serosa` or `4. Contiguous structures`)}
#'   \item{time_surgery_registration}{Time from surgery to registration (`0. Short` or `1. Long`)}
#'   \item{event_death}{Observed (1) or Censored (0)}
#'   \item{time_to_death}{Days until death}
#'   \item{event_recurrence}{Observed (1) or Censored (0)}
#'   \item{time_to_recurrence}{Days until recurrence event}
#'   \item{time_to_composite}{Days until first composite event or censoring time}
#'   \item{months_to_death}{Months until death}
#'   \item{months_to_recurrence}{Months until recurrence event}
#'   \item{months_to_composite}{Months until first composite event or censoring time}
#'   \item{event_composite}{Observed (1) or Censored (0)}
#' }
"colon_cancer"

