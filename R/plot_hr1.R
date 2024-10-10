#' We plot Hazard Ratios comparing each estimation approach (EMR, EMR_INSEE and EMR_INSEE_IMP) with the estimation based on the "true" data for each simulated scenario  
#' This function should be used when all patients are in the same group ('HR' in function 'simulate_surv' should be equal to 1)
#' 
#' @param res list from 'estimate_approach1' function
#'
#' @return graphic of Hazard Ratios 
#' @export

plot_hr1 <- function(res){
  # NULL initialisation of derived variables (necessary to remove a warning during package check)
  nrepeat <- hr <- lambda_lfu1 <- lambda_lfu2 <- lambda_d <- tacc <- tfu <- pinsee <- NULL
  pinsee0 <- value <- grp_tacc_tfu <- grp_lfu <- lambda_lfu1 <- lambda_lfu2 <- NULL

  parameters <- dplyr::select(res$parameters, - c(nrepeat, hr))
  data_tmp <- left_join(res$result, parameters, by ="scenario") %>%
    mutate(grp_lfu = paste0("&lambda;<sub>LFU<sub>t1</sub></sub>: ", lambda_lfu1, "<br> &lambda;<sub>LFU<sub>t2</sub></sub>: ", lambda_lfu2),
           lambda_d = paste0("&lambda;<sub>D</sub>: ", round(lambda_d, 4)),
           grp_tacc_tfu = paste0("TACC: ", tacc, "<br> TFU: ", tfu)) %>%
    pivot_longer(cols=c("hr_emr", "hr_emr_insee", "hr_emr_insee_imp"), names_to = "hr")

  HR_1gr <- ggplot(aes(x = pinsee0, y = value, color =hr), data=data_tmp) +
    geom_hline(yintercept = 1) +
    geom_point() +
    scale_colour_manual(values = c("#FE6546", "#a5b800", "#45c1ff"),
                        labels=c('EMR', 'EMR_INSEE', 'EMR_INSEE_IMP')) +
    labs(color = "Estimation approach", x= "INSEE recovery rate", y = "Hazard Ratio / true data") +
    scale_x_continuous(breaks= unique(res$parameters$pinsee0)) +
    facet_nested(rows = vars(lambda_d), cols = vars(grp_tacc_tfu, grp_lfu)) +
    theme_bw() +
    theme(strip.text.x =element_markdown(size=8), strip.text.y =element_markdown(size=8), legend.position = "bottom",
          axis.text.x = element_text(size=6), axis.text.y = element_text(size=6))

  return(HR_1gr)
}
