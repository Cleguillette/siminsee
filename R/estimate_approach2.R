# INTERN FUNCTION We analyze with a Cox model the data contained in 'dta' to compare 2 groups (for example treatment)
ana_2gr <- function(dat) {
  scenario <- nrepeat <- NULL

  # Cox models for each estimation approach
  cox_true <- coxph(Surv(delevt_true, evt_true) ~ gr, dat)
  cox_emr <- coxph(Surv(delevt_emr, evt_emr) ~ gr, dat)
  cox_emr_insee <- coxph(Surv(delevt_emr_insee, evt_emr_insee) ~ gr, dat)
  cox_emr_insee_imp <- coxph(Surv(delevt_emr_insee_imp, evt_emr_insee_imp) ~ gr, dat)


  ana <- c()
  for (model in list(cox_true, cox_emr, cox_emr_insee, cox_emr_insee_imp)){
    tmp <- c(model$coefficients,
             exp(model$coefficients),
             model$wald.test,
             1-pchisq(model$wald.test, 1),
             if_else(1-pchisq(model$wald.test, 1) < 0.05 & model$coefficients < 0, 1, 0),
             if_else(1-pchisq(model$wald.test, 1) < 0.05 & model$coefficients > 0, 1, 0),
             if_else(1-pchisq(model$wald.test, 1) < 0.05, 1, 0))
    ana <- c(ana, tmp)
  }
  names(ana) <- sapply(c("true", "emr", "emr_insee", "emr_insee_imp"),
                       function(x){
                         return(c(paste0("log_hr_", x),
                                  paste0("hr_", x),
                                  paste0("waldtest_", x),
                                  paste0("pval_test_", x),
                                  paste0("success_uni_cox_", x),
                                  paste0("failure_cox_", x),
                                  paste0("success_bi_cox_", x)))

                       })
  return(ana)

}

#' We perform Cox model analyses of the simulated data to compare the 2 groups (for example treatment) according to the estimation approach (true data, EMR, EMR_INSEE and EMR_INSEE_IMP) for each scenario and each repetition
#'
#' @param sim list from 'simulate_surv' function
#'
#' @return the function returns a list containing:
#' - $result:  a dataset containing, for each estimation approach, each scenario and each repetition:
#'  - the log Hazard Ratio
#'  - the Hazard Ratio
#'  - the statistic of the Wald test
#'  - the p-value associated with the Wald test
#'  - the success in unilateral (0 = no, yes = 1) if p-value < 0.05 & HR < 1
#'  - the failure (0 = no, yes = 1) if p-value < 0.05 & HR > 1
#'  - the success in bilateral (0 = no, yes = 1) if p-value < 0.05
#'- $parameters: a dataset containing all the parameters for each scenario 
#' @export

estimate_approach2 <- function(sim) {
  dat <- sim$dat
  parameters <- sim$parameters

  # NULL initialisation of derived variables (necessary to remove a warning during package check)
  scenario <- nrepeat <- NULL

  outcome <- tibble()
  for (s in c(unique(dat$scenario))){
    for (n in c(unique(filter(dat, scenario == s)$nrepeat))){
      dat_tmp <- filter(dat, scenario == s & nrepeat == n)
      ana <- ana_2gr(dat_tmp)
      ana[["scenario"]] <- s
      ana[["nrepeat"]] <- n
      outcome <- bind_rows(outcome, ana)
    }
  }
  outcome <- relocate(outcome, c("scenario", "nrepeat"))

  res <- list(outcome, parameters)
  names(res) <- c("result", "parameters")

  return(res)

}

