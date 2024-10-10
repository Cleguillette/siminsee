# INTERN FUNCTION We simulate a trial with 'nbp' patients
trial <- function(nbp = 1000,
                  tacc = 60,
                  tfu = 60,
                  lambda_d = 0.03,
                  hr = 1,
                  lambda_lfu1 = 0.01,
                  lambda_lfu2 = 0.02,
                  t_lfu2 = 36,
                  hr_lfu = 1,
                  pinsee0 = 0.95,
                  pinsee1 = 0.95
){

  # NULL initialisation of derived variables (necessary to remove a warning during package check)
  gr <- fu_theo <- delevt_true <- del_cens <- del_cens1 <- evt_true <- info_insee <- NULL
  evt_emr <- evt_insee <- evt_emr_insee <- delevt_emr_insee <- delevt_emr_insee_imp <- ea_emr <- NULL


  dat <- tibble(

    # patient number
    id = seq_len(nbp),   #(idem 1:nbp)

    # Balanced groups 1:1
    gr = gl(2,1, nbp, labels = 0:1),

    # Time of accrual of each patient from start of study  (idem tacc/nbp*1:nbp)
    time_accrual = tacc/nbp*id,

    # Theoretical follow-up duration, time interval between study entry and cut-off date
    fu_theo = tacc + tfu - (tacc/nbp*id),

    # True survival time for each patient according to randomization
    delevt_true = if_else(gr == 0,
                          pmin(fu_theo, rexp(nbp, rate = lambda_d)),
                          pmin(fu_theo, rexp(nbp,lambda_d*hr))),

    # True survival status over the study period
    evt_true = if_else(delevt_true < fu_theo, 1, 0),
    # 1 = dead
    # 0 = True alive at date of analysis

    # Potential time interval from accrual to the date of study withdrawal (following 1-piece exponential distribution)
    del_cens1 = if_else(gr == 0,
                        pmin(fu_theo, rexp(nbp, lambda_lfu1)),
                        pmin(fu_theo, rexp(nbp, lambda_lfu1*hr_lfu))),

    lfu_true1 = if_else(del_cens1 < fu_theo,
                        1,
                        0),

    # Potential time interval from accrual to the date of study withdrawal
    del_cens = case_when(((del_cens1 <= t_lfu2) & (lfu_true1 == 1)) ~ del_cens1,
                         ((gr == 0) & ((del_cens1 > t_lfu2) | (lfu_true1 != 1))) ~ t_lfu2 + pmin(fu_theo - t_lfu2, rexp(nbp, lambda_lfu2)),
                         ((gr == 1) & ((del_cens1 > t_lfu2) | (lfu_true1 != 1))) ~ t_lfu2 + pmin(fu_theo - t_lfu2, rexp(nbp, lambda_lfu2*hr_lfu))),

    # Observed survival time when considering study withdrawal before cut-off date
    delevt_emr = pmin(delevt_true, del_cens),

    evt_emr = if_else(delevt_true < del_cens, 1, 0),
    # 1 = dead
    # 0 = Censored, ie True alive at date of analysis or study withdrawal before cut-off date

    lfu_emr = if_else((del_cens <= delevt_true) & (del_cens < fu_theo), 1, 0),
    # 1 = lost to follow-up
    # 0 = dead or alive at cut-off date

    ea_emr = if_else((del_cens <= delevt_true) & (del_cens == fu_theo), 1, 0),
    # 1 = Alive at cut-off date
    # 0 = dead or lost to follow-up

    # potential evt in the INSEE file
    info_insee = if_else(gr == 0, rbinom(nbp, 1, pinsee0), rbinom(nbp, 1, pinsee1)),

    # evt available in the INSEE file
    evt_insee = evt_true*info_insee,

    # evt available in the hospital file or the insee file
    evt_emr_insee = evt_emr + (1 - evt_emr)*evt_insee,

    # survival time with true date of death if available, else with censoring
    delevt_emr_insee = if_else(evt_emr_insee == 1, delevt_true, del_cens),

    #Patient lost to follow-up
    lfu_emr_insee = if_else(evt_emr_insee == 0 & delevt_emr_insee< fu_theo, 1, 0),      

    # Exclu-vivant
    ea_emr_insee = if_else(evt_emr_insee == 0 & delevt_emr_insee == fu_theo & ea_emr == 1, 1, 0),
    
    # survival time with true date of death if available, with imputation
    delevt_emr_insee_imp = if_else(evt_emr_insee == 1, delevt_true,fu_theo),

    # Observed survival tim when considering study withdrawal before cut-off date
    evt_emr_insee_imp = evt_emr_insee,

    #Patient lost to follow-up
    lfu_emr_insee_imp = if_else(evt_emr_insee == 0 & delevt_emr_insee_imp< fu_theo, 1, 0),
    
    # Exclu-vivant
    ea_emr_insee_imp = if_else(evt_emr_insee == 0 & delevt_emr_insee_imp == fu_theo & evt_true == 0, 1, 0),
    
    # Immortel
    imm_emr_insee_imp = if_else(evt_emr_insee == 0 & delevt_emr_insee_imp == fu_theo & evt_true == 1, 1, 0)) %>%
    
    select(-del_cens1)# remove temporary variable

  return(dat)
}

# INTERN FUNCTION We simulate 'nrepeat' trials with 'nbp' patients
ntrial <- function(nrepeat = 1,
                   nbp = 1000,
                   tacc = 60,
                   tfu = 60,
                   lambda_d = 0.03,
                   hr = 1,
                   lambda_lfu1 = 0.01,
                   lambda_lfu2 = 0.02,
                   t_lfu2 = 36,
                   hr_lfu = 1,
                   pinsee0 = 0.95,
                   pinsee1 = 0.95
){
  dat <- tibble()
  for (n in 1:nrepeat)
  {
    tmp <- trial(nbp, tacc, tfu, lambda_d, hr, lambda_lfu1, lambda_lfu2, t_lfu2, hr_lfu, pinsee0, pinsee1)
    tmp$nrepeat <- n
    dat <- bind_rows(dat, tmp)
  }
  dat <-relocate(dat, "nrepeat")
  return(dat)
}


#' We simulate different trials with possibly different parameters specified in arguments for each scenario.
#' There are 2 groups of patients of identical size and homogeneous recruitment over time 
#'
#' @param scenario : the number of the scenarios (default 1 scenario)
#' @param nrepeat : a vector containing the number of repetitions for each scenario (default 1 repetition)
#' @param nbp : a vector containing the number of patients for each scenario (defaut 1,000 patients)
#' @param tacc : a vector containing the accrual duration in months for each scenario (default 60 months)
#' @param tfu : a vector containing the follow-up duration in months for each scenario (default 60 months)
#' @param lambda_d : a vector containing the rate of the exponential distribution for each scenario (default 0.03)
#' @param hr : a vector containing the Hazard Ratio for survival between group 0 and group 1 for each scenario (default 1)
#' @param lambda_lfu1 : a vector containing the rate of lost to follow-up for patients until time 't_lfu2' for each scenario (default 0.01)
#' @param lambda_lfu2 : a vector containing the rate of lost to follow-up for patients after time 't_lfu2' for each scenario (default 0.02)
#' @param t_lfu2 : a vector containing the time at which rate of lost to follow-up changes for each scenario (default 36)
#' @param hr_lfu : a vector containing the Hazard ratio for lost to follow-up between group 0 and group 1 for each scenario (default 1)
#' @param pinsee0 : a vector containing the administrative file completeness (insee) in group 0 for each scenario (default 0.95)
#' @param pinsee1 : a vector containing the administrative file completeness (insee) in group 1 for each scenario (default 0.95)
#' @param seed : seed used to generate each scenario
#'
#' @return the function returns a list containing:
#'  - $dat: a dataset containing the simulated data for all the scenarios and repetitions 
#'  - $parameters: a dataset containing all the parameters for each scenario 
#' @export

simulate_surv <- function(scenario=1,
                          nrepeat=1,
                          nbp = 1000,
                          tacc = 60,
                          tfu = 60,
                          lambda_d = 0.03,
                          hr = 1,
                          lambda_lfu1 = 0.01,
                          lambda_lfu2 = 0.02,
                          t_lfu2 = 36,
                          hr_lfu = 1,
                          pinsee0 = 0.95,
                          pinsee1 = 0.95,
                          seed = 2023){
  dat <- tibble()
  for (s in 1:scenario) {
    set.seed(seed)
    # Simulation of data for each repeat in each scenario
    tmp <- ntrial(nrepeat[s], nbp[s], tacc[s], tfu[s], lambda_d[s], hr[s], lambda_lfu1[s],  lambda_lfu2[s], t_lfu2[s], hr_lfu[s], pinsee0[s], pinsee1[s])
    tmp$scenario <- s
    dat <- bind_rows(dat, tmp)
  }
  dat <-relocate(dat, "scenario")

  # Parameters used to simulate (useful for graphics)
  parameters <- tibble(scenario = 1:scenario,
                      nrepeat = nrepeat,
                      nbp = nbp,
                      tacc = tacc,
                      tfu = tfu,
                      lambda_d = lambda_d,
                      hr = hr,
                      lambda_lfu1 = lambda_lfu1,
                      lambda_lfu2 = lambda_lfu2,
                      t_lfu2 = t_lfu2,
                      hr_lfu = hr_lfu,
                      pinsee0 = pinsee0,
                      pinsee1 = pinsee1)

  res <- list(dat, parameters)
  names(res) <- c("dat", "parameters")

  return(res)
}


