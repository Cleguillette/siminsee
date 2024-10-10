#' We calculate the percentages of patients with events, lost to follow-up, excluded alive or immortal according to the estimation approach (EMR, EMR_INSEE and EMR_INSEE_IMP) for each repetition of each scenario
#'
#' @param sim list from 'simulate_surv' function
#'
#' @return the function returns a list containing:
#' - $result: a dataset with the percentages according to the estimation approach for each repetition and each scenario:
#'  - percentages of patients with event 
#'  - percentages of patients lost to follow-up
#'  - percentages of patients excluded alive
#'  - percentages of patients immortal
#'- $parameters: a dataset containing all the parameters for each scenario 
#' @export

summarize_outcomes <- function(sim){
  # NULL initialisation of derived variables (necessary to remove a warning during package check)
  scenario <- nrepeat <- NULL

  dat <- sim$dat
  parameters <- sim$parameters

  outcome <- tibble()
  for (s in c(unique(dat$scenario))){
    for (n in unique(filter(dat, scenario == s)$nrepeat)){
      dat_tmp <- filter(dat, scenario ==  s & nrepeat == n)
      outcome_tmp <- lapply(c("evt_true",
                              "evt_emr",
                              "lfu_emr",
                              "ea_emr",
                              "evt_emr_insee",
                              "lfu_emr_insee",
                              "ea_emr_insee",
                              "evt_emr_insee_imp",
                              "lfu_emr_insee_imp",
                              "ea_emr_insee_imp",
                              "imm_emr_insee_imp"),
                            function(x){return(sum(dat_tmp[[x]])/dim(dat_tmp)[1])})
      outcome_tmp <- c(s, n, outcome_tmp)
      names(outcome_tmp) <- c("scenario",
                              "nrepeat",
                              "p_evt_true",
                              "p_evt_emr",
                              "p_lfu_emr",
                              "p_ea_emr",
                              "p_evt_emr_insee",
                              "p_lfu_emr_insee",
                              "p_ea_emr_insee",
                              "p_evt_emr_insee_imp",
                              "p_lfu_emr_insee_imp",
                              "p_ea_emr_insee_imp",
                              "p_imm_emr_insee_imp")

      outcome <- bind_rows(outcome, outcome_tmp)
    }
  }

  res <- list(outcome, parameters)
  names(res) <- c("result", "parameters")
  return(res)
}
