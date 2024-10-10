#' We calculate mean estimations of coefficients of the Cox modeling over repetitions for each scenario according to the three estimation approaches
#'
#' @param res : list from 'estimate_approach2' function
#'
#' @return the function returns a list containing:
#' - $result: a dataset with mean coefficients for each scenario:
#'  - Exponential of mean of log Hazard Ratios
#'  - Mean of log Hazard Ratios
#'  - Mean of absolute bias
#'  - Mean relative bias
#'  - Percentage of success in unilateral (estimation of risk alpha if hr = 1, estimation of power if hr < 1)
#'  - Percentage of success in bilateral (estimation of risk alpha if hr = 1)
#'  - Percentage of failures (estimation of power if hr > 1)
#'- $parameters: a dataset containing all the parameters for each scenario
#' @export

calc_mean_coeff2 <- function(res){
  ana <- res$result
  parameters <- res$parameters

  # NULL initialisation of derived variables (necessary to remove a warning during package check)
  scenario <- nrepeat <- NULL

  outcome <- tibble()
  for (s in c(unique(ana$scenario))){
    ana_tmp <- filter(ana, scenario == s)
    l_exp_mean_log_hr <- lapply(c('log_hr_true', 'log_hr_emr', 'log_hr_emr_insee', 'log_hr_emr_insee_imp'), function(x){exp(mean(ana_tmp[[x]]))})
    l_mean_log_hr <- lapply(c('log_hr_true', 'log_hr_emr', 'log_hr_emr_insee', 'log_hr_emr_insee_imp'), function(x){mean(ana_tmp[[x]])})
    l_bias_abs <- lapply(c('log_hr_emr', 'log_hr_emr_insee', 'log_hr_emr_insee_imp'), function(x){mean(ana_tmp[[x]]) - mean(ana_tmp$log_hr_true)})
    l_bias_rel <- lapply(c('log_hr_emr', 'log_hr_emr_insee', 'log_hr_emr_insee_imp'), function(x){(mean(ana_tmp[[x]]) - mean(ana_tmp$log_hr_true))/ mean(ana_tmp$log_hr_true)})
    l_p_suc_uni <- lapply(c('success_uni_cox_true', 'success_uni_cox_emr', 'success_uni_cox_emr_insee', 'success_uni_cox_emr_insee_imp'), function(x){sum(ana_tmp[[x]])/dim(ana_tmp)[1]})
    l_p_suc_bi <- lapply(c('success_bi_cox_true', 'success_bi_cox_emr', 'success_bi_cox_emr_insee', 'success_bi_cox_emr_insee_imp'), function(x){sum(ana_tmp[[x]])/dim(ana_tmp)[1]})
    l_p_fail <- lapply(c('failure_cox_true', 'failure_cox_emr', 'failure_cox_emr_insee', 'failure_cox_emr_insee_imp'), function(x){sum(ana_tmp[[x]])/dim(ana_tmp)[1]})
    names(l_exp_mean_log_hr) <- c('exp_m_log_hr_true', 'exp_m_log_hr_emr', 'exp_m_log_hr_emr_insee', 'exp_m_log_hr_emr_insee_imp')
    names(l_mean_log_hr) <- c('m_log_hr_true', 'm_log_hr_emr', 'm_log_hr_emr_insee', 'm_log_hr_emr_insee_imp')
    names(l_p_suc_uni) <- c('p_suc_uni_true', 'p_suc_uni_emr', 'p_suc_uni_emr_insee', 'p_suc_uni_emr_insee_imp')
    names(l_p_suc_bi) <- c('p_suc_bi_true', 'p_suc_bi_emr', 'p_suc_bi_emr_insee', 'p_suc_bi_emr_insee_imp')
    names(l_p_fail) <- c('p_fail_true', 'p_fail_emr', 'p_fail_emr_insee', 'p_fail_emr_insee_imp')
    names(l_bias_abs) <- c('bias_abs_emr', 'bias_abs_emr_insee', 'bias_abs_emr_insee_imp')
    names(l_bias_rel) <- c('bias_rel_emr', 'bias_rel_emr_insee', 'bias_rel_emr_insee_imp')

    outcome_tmp <- c(unique(ana_tmp$scenario),
                     max(ana_tmp$nrepeat),
                     l_exp_mean_log_hr,
                     l_mean_log_hr,
                     l_p_suc_uni,
                     l_p_suc_bi,
                     l_p_fail,
                     l_bias_abs,
                     l_bias_rel)
    names(outcome_tmp)[1] <- "scenario"
    names(outcome_tmp)[2] <- "nrepeat"

    outcome <- bind_rows(outcome, outcome_tmp)
  }
  outcome <- relocate(outcome, c("scenario", "nrepeat",
             "exp_m_log_hr_true", 'm_log_hr_true', "p_suc_uni_true", "p_suc_bi_true", "p_fail_true",
             "exp_m_log_hr_emr", 'm_log_hr_emr', "bias_abs_emr", "bias_rel_emr", "p_suc_uni_emr", "p_suc_bi_emr", "p_fail_emr",
             "exp_m_log_hr_emr_insee",'m_log_hr_emr_insee', "bias_abs_emr_insee", "bias_rel_emr_insee", "p_suc_uni_emr_insee", "p_suc_bi_emr_insee", "p_fail_emr_insee",
             "exp_m_log_hr_emr_insee_imp", 'm_log_hr_emr_insee_imp', "bias_abs_emr_insee_imp", "bias_rel_emr_insee_imp", "p_suc_uni_emr_insee_imp", "p_suc_bi_emr_insee_imp", "p_fail_emr_insee_imp"))

  mean_res <- list(outcome, parameters)
  names(mean_res) <- c("result", "parameters")

  return(mean_res)
}



