#' Targeted maximum likelihood estimator
#'
#' @description A function that estimates the treatment effect via a (longitudinal) targeted maximum likelihood estimator.
#'
#' @param data A data frame containing all observed data for the current analysis
#' @param estimand Estimand of interest; treatment effect on "difference" scale, "ratio" scale or odds ratio ("oddsratio") scale
#' @param propensity_score_formula TODO
#' @param inverse_weight_formulas TODO
#' @param outcome_formulas TODO
#' @param outcome_type TODO
#' @param outcome_range TODO
#' @param absorbing_state TODO
#' @param absorbing_outcome TODO
#' @param impute_formulas TODO
#' @param impute_model TODO
#' @param imputation_args TODO
#' @param ci TODO
#' @param verbose TODO
#' @param bootstrap_n TODO
#' @param bootstrap_type TODO
#' @param alpha TODO
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
tmle <-
  function(
    data,
    estimand = "difference",
    propensity_score_formula,
    inverse_weight_formulas,
    outcome_formulas,
    outcome_type =
      c("gaussian",
        "logistic",
        "binomial",
        "multinomial-binomial")[1],
    outcome_range = NULL,
    absorbing_state = NULL,
    absorbing_outcome = NULL,
    impute_formulas = NULL,
    impute_model =
      c("gaussian",
        "binomial",
        "beta",
        "pmm",
        "multinomial")[1],
    imputation_args = NULL,
    ci = FALSE,
    verbose = FALSE,
    bootstrap_n = 10000,
    bootstrap_type = c("bca", "norm", "basic", "perc")[1],
    alpha = 0.05,
    ...
  ) {

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
