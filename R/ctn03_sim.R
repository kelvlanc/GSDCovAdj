#' Mock CTN-0003 data set
#'
#'
#' CTN03 was a phase III two arm trial to assess tapering schedules of the drug
#' buprenorphine, a pharmacotherapy for opioid dependence. At the time of the
#' study design, there was considerable variation in tapering schedules in
#' practice, and a knowledge gap in terms of the best way to administer
#' buprenorphine to control withdrawal symptoms and give the greatest chance of
#' abstinence at the end of treatment. It was hypothesized that a longer taper
#' schedule would result in greater likelihood of a participant being retained
#' on study and providing opioid-free urine samples at the end of the drug taper
#' schedule.
#' Participants were randomized 1:1 to a 7-day or 28-day taper using stratified
#' block randomization across 11 sites in 10 US cities. Randomization was
#' stratified by the maintenance dose of buprenorphine at stabilization:
#' 8, 16, or 24 mg.
#'
#' The data in this package are simulated data, generated from probability models
#' fit to the original study data, and not the actual data from the CTN03 trial.
#' A new dataset was created by resampling with replacement from the original data,
#' and then each variable in the new dataset was iteratively replaced using simulated
#' values from probability models based on the original data.
#' More information can be found at:
#' https://jbetz-jhu.github.io/CovariateAdjustmentTutorial/CTN03_Ordinal.html#ctn03-study-design
#'
#' @format A data frame with 516 rows and 25 columns:
#' \describe{
#'   \item{arm}{Treatment arm: `7-day` or `28-day` taper using}
#'   \item{stability_dose}{Maintenance dose of buprenorphine at stabilization (stratification factor): `8mg`, `16mg` or `24mg` }
#'   \item{age}{Participant's age at baseline in years}
#'   \item{sex}{Participant's sex (`Male` or `Female`)}
#'   \item{race}{Participant's race}
#'   \item{ethnic}{Participant's ethnicity (`Not Hispanic ` or `Hispanic `)}
#'   \item{marital}{Participant's marital status (`Married/Cohabitating`, `Never married` or `Divorced/Separated/Widowed`)}
#'   \item{arsw_score_bl}{Adjective Rating Scale for Withdrawal (ARSW) Score at baseline}
#'   \item{cows_total_score_bl}{Clinical Opiate Withdrawal Scale (COWS) Score at baseline}
#'   \item{cows_category_bl}{COWS Severity Category at baseline - Ordinal (`0. No withdrawal`, `1. Mild withdrawal`, `2. Moderate withdrawal`,`3. Moderately severe withdrawal` or `4. Severe withdrawal`)}
#'   \item{vas_crave_opiates_bl}{Visual Analog Scale (VAS) - Self report of opiate cravings at baseline}
#'   \item{vas_current_withdrawal_bl}{Visual Analog Scale (VAS) - Current withdrawal at baseline}
#'   \item{vas_study_tx_help_bl}{Visual Analog Scale (VAS) - Study treatment helping symptoms at baseline}
#'   \item{uds_opioids_bl}{Urine Drug Screen Result - Opioids at baseline (`Negative` or `Positive`)}
#'   \item{uds_oxycodone_bl}{Urine Drug Screen Result - Oxycodone at baseline (`Negative` or `Positive`)}
#'   \item{uds_any_positive_bl}{Urine Drug Screen - Any positive result at baseline (`Negative` or `Positive`)}
#'   \item{arsw_score_eot}{Adjective Rating Scale for Withdrawal (ARSW) Score at end-of-taper}
#'   \item{cows_total_score_eot}{Clinical Opiate Withdrawal Scale (COWS) Score at end-of-taper}
#'   \item{cows_category_eot}{COWS Severity Category at end-of-taper - Ordinal (`0. No withdrawal`, `1. Mild withdrawal`, `2. Moderate withdrawal`,`3. Moderately severe withdrawal` or `4. Severe withdrawal`)}
#'   \item{vas_crave_opiates_eot}{Visual Analog Scale (VAS) - Self report of opiate cravings at end-of-taper}
#'   \item{vas_current_withdrawal_eot}{Visual Analog Scale (VAS) - Current withdrawal at end-of-taper}
#'   \item{vas_study_tx_help_eot}{Visual Analog Scale (VAS) - Study treatment helping symptoms at end-of-taper}
#'   \item{uds_opioids_eot}{Urine Drug Screen Result - Opioids at end-of-taper (`Negative` or `Positive`)}
#'   \item{uds_oxycodone_eot}{Urine Drug Screen Result - Oxycodone at end-of-taper (`Negative` or `Positive`)}
#'   \item{uds_any_positive_eot}{Urine Drug Screen - Any positive result at end-of-taper (`Negative` or `Positive`)}
#' }
"ctn03_sim"
