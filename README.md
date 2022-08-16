
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GSDCovAdj

<!-- badges: start -->
<!-- badges: end -->

The goal of GSDCovAdj is to combine group sequential,
information-adaptive designs with covariate adjustment

Kelly Van Lancker, Josh Betz and Michael Rosenblum

## Installation

Before installation of the development version of **GSDCovAdj**, we
recommend installing the package simul from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nt-williams/simul")
```

You can install the development version of GSDCovAdj from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("kelvlanc/GSDCovAdj")
```

## Scope

**GSDCovAdj** implements the methods proposed by Van Lancker, Betz and
Rosenblum (2022) to combine group sequential, information-adaptive
designs with covariate adjustment. The approach is implemented for a
G-computation and targeted maximum likelihood estimator for continuous
and binary outcomes, and for the estimator proposed by Díaz et
al. (2019) for time-to-event outcomes.

## Example

### Installing R Packages

We first install the required packages

``` r
required_packages <-
  c("tidyr", "dplyr", "parallel", "ldbounds")

install.packages(required_packages)
```

Once the required packages are installed, they can be loaded using
`library()`

``` r
library(rpact)
#> Warning: package 'rpact' was built under R version 4.1.2
library(tidyr)
#> Warning: package 'tidyr' was built under R version 4.1.2
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.1.2
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(parallel)
library(ldbounds)
#> Warning: package 'ldbounds' was built under R version 4.1.2

library(simul)
library(GSDCovAdj)
#> Warning: replacing previous import 'betareg::predict' by 'stats::predict' when
#> loading 'GSDCovAdj'
```

### Design Parameters

We first determine the maximum/total information and sample size based
on a design with 2 analyses: 1 interim analyses when 50% of the
information is available in addition to the final analysis. The
two-sided significance level equals 5% and the power to detect a 5%
difference in the success rate (55% in treatment arm versus 50% in
control) equals 90%. An alpha-spending function that approximates the
Pocock boundaries are used for efficacy stopping (futility stopping is
not considered).

``` r
design_par = getDesignGroupSequential(sided = 2, alpha = 0.05, beta=0.1,
                                      informationRates = c(0.50, 1),
                                      typeOfDesign = "asP")
# Determine Inflation Factor
#getDesignCharacteristics(design_par) #1.1110  

inf_total = 1.1110*(qnorm(0.975)+qnorm(0.90))^2/0.05^2

n_total = round(1.1110*(power.prop.test(power=0.90,p1=0.55,p2=0.5, 
                                         alternative="two.sided", 
                                         sig.level=0.05)$n)*2
)
```

We load the code to calculate expit.

``` r
expit = function(x){
  exp(x)/(1+exp(x))
}
```

### Create Simulated Dataset

Create data frame ‘study_data’ with baseline covariates `x_1`, `x_2`,
`x_3` and `x_4`, a patient identifier (`id`), a treatment indicator
(`treatment`), the primary outcome of interest (`y_1`), the time of
enrollment (`enrollment_times`), and the time a patient’s outcome is
measured (`outcome_times`).

``` r
set.seed(12345)
# Generate baseline covariates
baseline_covariates_orig <-
  matrix(
    data =
      mvtnorm::rmvnorm(
        n = n_total,
        mean = matrix(data = 0, nrow = 4),
        sigma = diag(x = 1, nrow = 4)
      ),
    nrow = n_total,
    ncol = 4
  ) %>% 
  data.frame() %>% 
  setNames(
    object = .,
    nm = paste0("x_", 1:4)
  )

# Generate treatment indicator
treatment <-
      rbinom(
        n = n_total,
        size = 1,
        prob = 0.5
      )

# (Counterfactual) outcomes under new experimental treatment
n_outcomes = 1
outcomes1 <-
      matrix(
        data =
          rbinom(
            n = n_total*n_outcomes,
            size = 1,
            prob = expit(-2.5*baseline_covariates_orig$x_1+
                           2*baseline_covariates_orig$x_2-
                           2.5*baseline_covariates_orig$x_3+
                           2.1*baseline_covariates_orig$x_4)
          ),
        nrow = n_total,
        ncol = n_outcomes
      ) %>% 
      data.frame() %>% 
      setNames(
        object = .,
        nm = paste0("y1_", 1:n_outcomes)
      )

# (Counterfactual) outcomes under control
outcomes0 <-
      matrix(
        data =
          rbinom(
            n = n_total*n_outcomes,
            size = 1,
            prob = expit(-2*baseline_covariates_orig$x_1+
                           2.5*baseline_covariates_orig$x_2-
                           2.25*baseline_covariates_orig$x_3-
                           2.1*baseline_covariates_orig$x_4)
          ),
        nrow = n_total,
        ncol = n_outcomes
      ) %>% 
      data.frame() %>% 
      setNames(
        object = .,
        nm = paste0("y0_", 1:n_outcomes)
      )

# Enrollment times
daily_enrollment = 5
enrollment_times <-
      round(
        runif(
          n = n_total, min = 0, max=n_total/daily_enrollment
        )
      )

# Outcome times
outcome_times <-
      setNames(
        object = 365 + enrollment_times,
        nm = paste0("outcome_time_", 1)
      )

# Measured baseline covariates   
baseline_covariates_meas = as.data.frame(cbind(
  x_1 = exp(baseline_covariates_orig$x_1/2),
  x_2 = baseline_covariates_orig$x_2/
  (1+exp(baseline_covariates_orig$x_1))+10,
  x_3 = (baseline_covariates_orig$x_1*
                                  baseline_covariates_orig$x_3/25+0.6)^3,
  x_4 = (baseline_covariates_orig$x_2
                                +baseline_covariates_orig$x_4+20)^2
  ))

# Make dataset    
study.data <-
      tibble(
        id = 1:n_total,
        baseline_covariates_meas,
        enrollment_times,
        treatment,
        outcome_times,
        y_1 = outcomes1$y1_1*treatment+outcomes0$y0_1*(1-treatment)
      )
```

The data can also be directly loaded from Github

``` r
data_url_gsd_data <-
  "https://github.com/jbetz-jhu/CovariateAdjustmentTutorial/raw/main/simData.RData"

load(file = url(data_url_gsd_data))
```

The complete simulated trial data without any missing values are in a
`data.frame` named `study.data`.

-   Randomization Information
    -   `treatment`: Treatment Arm
-   Baseline Information
    -   `x_1`, `x_2`, `x_3` and `x_4`: 4 continuous baseline covariates
    -   `id`: patient identifier
    -   `enrollment_times`: time of enrollment
-   Outcome information:
    -   `y_1`: Binary outcome (1: success; 0: otherwise)
    -   `outcome_times`: time someone’s outcome is measured (365 days
        after enrollment)

### Combining Group Sequential Designs and Covariate Adjustment

#### Interim analysis

Via the function `data_at_time_t()` we construct a dataframe of the data
available at the interim analysis, which is conducted when 50% of the
patients have their primary endpoint available.

``` r
n_total_ia = round(0.5*n_total)
time_ia = sort(study.data$outcome_times)[n_total_ia]
n_recr_ia = length(which(study.data$enrollment_times<=time_ia))

data_ia = data_at_time_t(
  data = study.data,
  id_column = "id",
  enrollment_time = "enrollment_times",
  treatment_column = "treatment",
  covariate_columns = c("x_1", "x_2", "x_3", "x_4"),
  outcome_columns = c("y_1"),
  outcome_times = c("outcome_times"),
  analysis_time = time_ia
      ) 
    
```

We can then call the function `interimAnalysis()` to conduct the interim
analysis. This gives us a decision based on the original estimator and
updated estimator (which is the same as the original one at the first
analysis). In addition, we get the estimates(both the original and
updated estimate), the corresponding standard errors, test statistics,
and information fractions. Finally, we also get the current covariance
matrix based on the original estimates, which we will need at the final
analysis to do the orthogonalization (in order to obtain the updated
estimate).

``` r
results_ia = interimAnalysis(data = data_ia,
                             totalInformation = inf_total, 
                             estimationMethod= standardization,
                             estimand = "difference",
                             null.value = 0,
                             alpha = 0.05,
                             beta = 0.2,
                             alternative = "two.sided", 
                             typeOfDesign = "asP",
                             typeBetaSpending = "none",
                             futilityStopping = FALSE,
                             plannedAnalyses=2,
                             y0_formula=y_1 ~ x_1, 
                             y1_formula=y_1 ~ x_1,
                             family="binomial",
                             treatment_column = "treatment"
                             )
```

##### Results

Decision based on original interim estimator

``` r
results_ia$decisionOriginal
#> [1] "No"
```

So we do not stop the trial early for efficacy based on the original
interim estimator.

Decision based on updated interim estimator

``` r
results_ia$decisionUpdated
#> [1] "No"
```

So we do not stop the trial early for efficacy based on the updated
interim estimator.

Original interim estimate

``` r
results_ia$estimateOriginal
#> [1] 0.005660582
previousEstimates = results_ia$estimateOriginal
```

Updated interim estimate

``` r
results_ia$estimateUpdated
#> [1] 0.005660582
```

Covariance matrix (i.e., variance) of the original interim estimate

``` r
results_ia$covMatrixOriginal
#> [1] 0.0003517528
covMatrix = results_ia$covMatrixOriginal
```

Information time (current information divided by expected information at
end of trial) corresonding with original interim estimate

``` r
results_ia$informationTimeOriginal
#> [1] 0.6088245
previousTimes = results_ia$informationTimeOriginal
```

Information time (current information divided by expected information at
end of trial) corresonding with updated interim estimate

``` r
results_ia$informationTimeUpdated
#> [1] 0.6088245
previousTimesUpd = results_ia$informationTimeUpdated
```

#### Final analysis

Via the function `data_at_time_t()` we construct a dataframe of the data
available at the final analysis (which equals the full dataset!).

``` r
time_fin = sort(study.data$outcome_times)[n_total]
n_recr_fin = length(which(study.data$enrollment_times<=time_fin))

data_fin = data_at_time_t(
  data = study.data,
  id_column = "id",
  enrollment_time = "enrollment_times",
  treatment_column = "treatment",
  covariate_columns = c("x_1", "x_2", "x_3", "x_4"),
  outcome_columns = c("y_1"),
  outcome_times = c("outcome_times"),
  analysis_time = time_fin
      ) 
    
```

We can then call the function `interimAnalysis()` to conduct the final
analysis. This gives us a decision based on the original estimator and
updated estimator (which is the same as the original one at the first
analysis). In addition, we get the estimates(both the original and
updated estimate), the corresponding standard errors, test statistics,
and information fractions. Finally, we also get the current covariance
matrix based on the original estimates, which we will need at the final
analysis to do the orthogonalization (in order to obtain the updated
estimate).

``` r
results_fin = interimAnalysis(data = data_fin, 
                            totalInformation = inf_total, 
                            estimationMethod= standardization,
                            estimand = "difference",
                            previousEstimatesOriginal=previousEstimates,
                            previousCovMatrixOriginal=covMatrix,
                            previousInformationTimesOriginal=previousTimes,
                            previousInformationTimesUpdated=previousTimesUpd,
                            previousDatasets = list(data_ia),
                            null.value = 0,
                            alpha = 0.05, 
                            beta = 0.2,
                            alternative = "two.sided", 
                            typeOfDesign = "asP",
                            typeBetaSpending = "none",
                            futilityStopping = FALSE,
                            plannedAnalyses=2,
                            y0_formula=y_1 ~ x_2, 
                            y1_formula=y_1 ~ x_2,
                            family="binomial",
                            parametersPreviousEstimators = list(list(
                              y0_formula=y_1 ~ x_1,
                              y1_formula=y_1 ~ x_1,
                              family="binomial"
                              )),
                            treatment_column = "treatment"
)
#> Warning: Argument unknown in getDesignGroupSequential(...): 'y0_formula' =
#> formula will be ignored
#> Warning: Argument unknown in getDesignGroupSequential(...): 'y1_formula' =
#> formula will be ignored
#> Warning: Argument unknown in getDesignGroupSequential(...): 'family' =
#> 'binomial' will be ignored
#> Warning: Argument unknown in getDesignGroupSequential(...): 'treatment_column' =
#> 'treatment' will be ignored
#> Warning: Argument unknown in getDesignGroupSequential(...): 'y0_formula' =
#> formula will be ignored
#> Warning: Argument unknown in getDesignGroupSequential(...): 'y1_formula' =
#> formula will be ignored
#> Warning: Argument unknown in getDesignGroupSequential(...): 'family' =
#> 'binomial' will be ignored
#> Warning: Argument unknown in getDesignGroupSequential(...): 'treatment_column' =
#> 'treatment' will be ignored
```

##### Results

Decision based on original final estimator

``` r
results_fin$decisionOriginal
#>   X2 
#> "No"
```

So we do not reject the null hypothesis based on the original final
estimator.

Decision based on updated final estimator

``` r
results_fin$decisionUpdated
#>      [,1]
#> [1,] "No"
```

So we do not reject the null hypothesis based on the updated final
estimator.

Original final estimate

``` r
results_fin$estimateOriginal
#> [1] 0.01689733
```

Updated final estimate

``` r
results_fin$estimateUpdated
#>           [,1]
#> [1,] 0.0152497
```
