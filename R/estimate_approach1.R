# INTERN FUNCTION  We analyze with a Cox model the data contained in 'dat' to compare the 3 different estimation approaches with the estimation based on true data
# This function should be used when all patients are in the same group ('HR' in function trial should be equal to 1)
ana_1gr <- function(dat) {

  # NULL initialisation of derived variables (necessary to remove a warning during package check)
  delevt_true <- evt_true <- NULL


  data_true <- dat %>%
    select(c("evt_true", "delevt_true")) %>%
    rename(del = delevt_true,
           evt = evt_true) %>%
    mutate(gr = 0)

  dat_new <- lapply(list("emr", "emr_insee", "emr_insee_imp"),
         function(x){
           dat <- dat %>%
             select(c(paste0("evt_", x), paste0("delevt_", x))) %>%
             rename(del = paste0("delevt_", x),
                    evt = paste0("evt_", x)) %>%
             mutate(gr=1) %>%
             bind_rows(data_true) %>%
             mutate(n=row_number())
          return(dat)})

  ana <- sapply(dat_new,
         function(x){
           cox <- coxph(Surv(del, evt) ~ gr, x)
           HR <- exp(cox$coefficients)
           return(HR)
         }
  )
  names(ana) <- c("hr_emr", "hr_emr_insee", "hr_emr_insee_imp")

  return((ana))
}




#' We estimate Hazard Ratios (with a Cox model) comparing each estimation approach (EMR, EMR_INSEE and EMR_INSEE_IMP) with the estimation based on the "true" data for each simulated scenario  
#' This function should be used when all patients are in the same group ('HR' in function 'simulate_surv' should be equal to 1)
#' 
#' @param sim list from 'simulate_surv' function
#'
#' @return the function returns a list containing:
#' - $result: a dataset with:
#'  - columns corresponding to Hazard ratios comparing each estimation approach with true data
#'  - rows corresponding to each scenario
#'- $parameters: a dataset containing all the parameters for each scenario 
#' @export

estimate_approach1 <- function(sim){
  dat <- sim$dat
  parameters <- sim$parameters

  # NULL initialisation of derived variables (necessary to remove a warning during package check)
  scenario <- nrepeat <- NULL

  outcome <- tibble()
  for (s in c(unique(dat$scenario))){
    for (n in c(unique(filter(dat, scenario == s)$nrepeat))){
      dat_tmp <- filter(dat, scenario == s & nrepeat == n)
      ana <- ana_1gr(dat_tmp)
      ana[["scenario"]] <- s
      ana[["nrepeat"]] <- n
      outcome <- bind_rows(outcome, ana)
    }
  }
  outcome <- relocate(outcome, c(scenario, nrepeat))

  res <- list(outcome, parameters)
  names(res) <- c("result", "parameters")

  return(res)
}


