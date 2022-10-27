#' Estimating estimands for binary and continuous endpoints via a (longitudinal) targeted maximum likelihood estimator
#'
#' @description A function that estimates the treatment effect for binary and continuous outcomes via a (longitudinal) targeted maximum likelihood estimator.
#'
#' @param data A data frame containing the observed data at a given time. This data frame should have the same structure as the data frame outputted by \code{data_at_time_t}. Treatment should be coded as 0 and 1.
#' @param estimand A character string indicating the estimand of interest; treatment effect on "difference" scale, "ratio" scale or odds ratio ("oddsratio") scale.
#' @param propensity_score_formula A formula for the propensity score model - the left-hand-side indicates the binary treatment assignment indicator variable.
#' @param inverse_weight_formulas A list of formulas for computing inverse probability of censoring weights. Only the right-hand side of the formula is used.
#' @param outcome_formulas A list of formulas with one formula per outcome model, ordered from the first to the last observed outcome.
#' @param outcome_type A character string indicating the type of outcome model to be fitted: "gaussian", "logistic", "binomial", or "multinomial-binomial".
#' @param outcome_range A numeric vector of length 2 indicating the range of the outcome. Only used when outcome_type == "logistic".
#' @param absorbing_state A character vector indicating which level (if any) of the outcome is an absorbing state. Only used when outcome_type == "multinomial-binomial".
#' @param absorbing_outcome A value of 0 or 1 denoting whether the absorbing state is a success (1) or failure (0) for the binary final outcome. Only used when outcome_type == "multinomial-binomial".
#' @param impute_formulas A list of formulas with one formula per intermediate outcome, containing the paramaterization of imputation models for intermediate outcomes.
#' @param impute_model A character vector indicating the family of imputation model to use: "gaussian" (default), "binomial", "beta", "pmm", or "multinomial".
#' @param imputation_args A list of arguments to pass to functions performing imputation.
#' @param ci TRUE/FALSE - Should bootstrap-based confidence intervals be returned (TRUE) or just the estimate (FALSE).
#' @param verbose TRUE/FALSE - Should all models and intermediate results be returned (TRUE) or just the estimate (FALSE).
#' @param bootstrap_n Number of bootstrap replicates used to perform bootstrap inference.
#' @param bootstrap_type The type of bootstrap to perform: "bca" (default), "norm", "basic", or "perc".
#' @param alpha Type I Error rate used in the confidence level for bootstrap inference - Defaults to 0.05.
#' @param ... Further arguments for the estimator function.
#'
#' @importFrom stats glm predict
#'
#' @return An object of class \code{standardization}.
#' \describe{
#'   \item{estimate}{Original estimate based on a standardization estimator (also called G-computation).}
#'   \item{y1_pred}{Predictions of the outcome under treatment for all recruited participants.}
#'   \item{y0_pred}{Predictions of the outcome under control for all recruited participants.}}
#' @export
rctmle <-
  function(
    data,
    propensity_score_formula,
    inverse_weight_formulas,
    outcome_formulas,
    outcome_type,
    estimand = "difference",
    outcome_range = NULL,
    absorbing_state = NULL,
    absorbing_outcome = NULL,
    impute_formulas = NULL,
    impute_model = NULL,
    imputation_args = NULL,
    ci = FALSE,
    verbose = FALSE,
    bootstrap_n = 10000,
    bootstrap_type = c("bca", "norm", "basic", "perc")[1],
    alpha = 0.05,
    ...
  ) {

    if(!(estimand %in% c("difference", "ratio", "oddsratio"))){
      stop("Estimand must be one of the following: difference, ratio, or oddsratio.")
    }

    outcome_type <- tolower(outcome_type)
    impute_model <- tolower(impute_model)

    arg_list <- as.list(substitute(list(...)))[-1L]

    if(ci) {
      verbose <- FALSE # Override verbose when bootstrapping
      if(!bootstrap_type %in% c("bca", "norm", "basic", "perc")) {
        stop("Unrecognized bootstrap method: ", bootstrap_type)
      }
    }

    tmle_args <-
      tmle_get_args(
        data = data,
        propensity_score_formula = propensity_score_formula,
        inverse_weight_formulas = inverse_weight_formulas,
        outcome_formulas = outcome_formulas,
        impute_formulas = impute_formulas
      )


    # When absorbing state is specified, check for consistency and carry forward
    # absorbing state values
    if(!is.null(absorbing_state)){
      checked_data <-
        absorbing_state_check(
          data = data,
          y_columns = tmle_args$y_columns,
          outcome_type = outcome_type,
          absorbing_state = absorbing_state,
          absorbing_outcome = absorbing_outcome
        )

      environment(checked_data) <-
        environment(data)

      data <- checked_data
    }

    precheck_results <-
      tmle_precheck(
        data = data,
        x_columns = tmle_args$x_columns,
        tx_column = tmle_args$tx_column,
        y_columns = tmle_args$y_columns,
        imputation_x = tmle_args$imputation_x,
        impute_formulas = impute_formulas,
        impute_model = impute_model,
        outcome_type = outcome_type,
        outcome_range = outcome_range,
        absorbing_state = absorbing_state
      )

    impute <-
      sum(colSums(non_monotone(data = data, y_columns = tmle_args$y_columns)))

    if(outcome_type == "logistic"){
      data[, tmle_args$y_columns] <-
        apply(
          X = data[, tmle_args$y_columns],
          MARGIN = 2,
          FUN = compress_range,
          min = outcome_range[1],
          max = outcome_range[2],
          enforce_range = TRUE
        )
    }

    # Construct left-hand-side of formulas
    tmle_formulas <-
      tmle_get_formulas(
        y_columns = tmle_args$y_columns,
        inverse_weight_formulas = inverse_weight_formulas,
        outcome_formulas = outcome_formulas
      )

    # Re-set environments for formulas updated within functions
    for (i in 1:length(tmle_formulas$inverse_weight_formulas))
      environment(tmle_formulas$inverse_weight_formulas[[i]]) <-
      environment(inverse_weight_formulas[[i]])

    for (i in 1:length(tmle_formulas$outcome_formulas))
      environment(tmle_formulas$outcome_formulas[[i]]) <-
      environment(outcome_formulas[[i]])

    for (i in 1:length(precheck_results$impute_formulas))
      environment(precheck_results$impute_formulas[[i]]) <-
      environment(impute_formulas[[i]])

    arg_list <-
      c(
        list(
          estimand = estimand,
          y_columns = tmle_args$y_columns,
          tx_column = tmle_args$tx_column,
          propensity_score_formula = propensity_score_formula,
          inverse_weight_formulas =
            tmle_formulas$inverse_weight_formulas,
          outcome_formulas =
            tmle_formulas$outcome_formulas,
          outcome_type = outcome_type,
          outcome_range = outcome_range,
          absorbing_state = absorbing_state,
          absorbing_outcome = absorbing_outcome,
          impute = impute,
          impute_formulas = impute_formulas,
          imputation_args = imputation_args,
          impute_model = impute_model,
          verbose = verbose
        ),
        # impute_model, imputation_args, ...
        arg_list
      )


    if(ci){

      tmle_boot <-
        boot(
          data = data,
          statistic = tmle_boot_wrap,
          R = bootstrap_n,
          tmle_args =
            arg_list
        )

      tmle_boot_ci <-
        boot.ci(
          boot.out = tmle_boot,
          conf  = (1 - alpha),
          type = bootstrap_type
        )

      lcl_ucl <-
        tail(x = tmle_boot_ci[bootstrap_type][[1]][1,], 2)

      return(
        c(
          estimate = tmle_boot$t0,
          se = sd(tmle_boot$t),
          lcl = lcl_ucl[1],
          ucl = lcl_ucl[2],
          alpha = alpha
        )
      )
    } else {
      return(
        tmle_boot_wrap(
          data = data,
          tmle_args = arg_list
        )
      )
    }
  }
