#' We plot Kaplan-Meier survival curves according to the estimation approach (EMR, EMR_INSEE and EMR_INSEE_IMP) for each of the simulated scenarios
#' This function should be used when all patients are in the same group ('HR' in function 'simulate_surv' should be equal to 1)
#'
#' @param sim list from 'simulate_surv' function
#'
#' @return graphic of survival curves 
#' @export

plot_surv1 <- function(sim){
  # NULL initialisation of derived variables (necessary to remove a warning during package check)
  delevt_true <- delevt_emr <- delevt_emr_insee <- delevt_emr_insee_imp <- evt_true <- evt_emr <- evt_emr_insee  <- NULL
  evt.group3 <- scenario <- lambda_lfu1 <- lambda_lfu2 <- lambda_d <- NULL


  data_tmp <-sim$dat %>%
    select(c("scenario", starts_with(c("del","evt")), -"del_cens", -"evt_insee", -"evt_emr_insee_imp"))%>%
    rename(del.group1 = delevt_true, del.group2 = delevt_emr, del.group3 = delevt_emr_insee, del.group4 = delevt_emr_insee_imp,
           evt.group1 = evt_true, evt.group2 = evt_emr, evt.group3 = evt_emr_insee) %>%
    mutate(evt.group4 = evt.group3) %>%
    pivot_longer(cols=-scenario, names_to = c(".value", "group"), names_sep = "\\.") %>%
    left_join(sim$parameters, by ="scenario") %>%
    mutate(grp_pdv = paste0("&lambda;<sub>LFU<sub>t1</sub></sub>: ", lambda_lfu1, "; &lambda;<sub>LFU<sub>t2</sub></sub>: ", lambda_lfu2),
           lambda0 = paste0("&lambda;<sub>D</sub>: ", round(lambda_d, 4)))

  fit <- survfit( Surv(del, evt) ~ group, data = data_tmp)

  courbe_1gr <- ggsurvplot_facet(fit, data=data_tmp, facet.by=c("lambda0", "grp_pdv"),
                                 legend.labs = c("True data", 'EMR', 'EMR_INSEE', 'EMR_INSEE_IMP'),
                                 palette = c("#d175b8", "#FE6546", "#a5b800", "#45c1ff"),
                                 legend.title = "Estimation approach",
                                 short.panel.labs = TRUE,
                                 censor = FALSE,
                                 conf.int = FALSE,
                                 ggtheme = theme_bw()) +
    xlab("Time (months)") +
    ylab("Survival probability") +
    theme(legend.position = "bottom",
          strip.text.x =element_markdown(),
          strip.text.y =element_markdown())

  return(courbe_1gr)
}
