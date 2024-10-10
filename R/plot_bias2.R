#' We plot the mean absolute biases over the repetitions for each scenario according to the estimation approach (EMR, EMR_INSEE and EMR_INSEE_IMP)
#'This function should be used when there is a difference in survival between the two groups ('hr' in function 'simulate_surv' should not be equal to 1)
#'
#' @param mean_res : list from 'calc_mean_coeff2' function
#'
#' @return graphic of mean absolute bias 
#' @export

plot_bias2 <- function(mean_res){
  # NULL initialisation of derived variables (necessary to remove a warning during package check)
  lambda_d <- grp_lfu <- hr_lfu <- p_suc_ini <- grp_insee <- value <- insee <- insee_y <- pinsee <- pinsee0 <-pinsee1 <- NULL
  lambda_lfu1 <- lambda_lfu2 <- bias_abs <- NULL

  data_tmp <-left_join(mean_res$result, mean_res$parameters, by =c("scenario", "nrepeat"))

    data_tmp <- data_tmp %>%
    mutate(grp_lfu = paste0("&lambda;<sub>LFU<sub>t1</sub></sub>: ", lambda_lfu1, "<br> &lambda;<sub>LFU<sub>t2</sub></sub>: ", lambda_lfu2),
           lambda_d = paste0("&lambda;<sub>D</sub>: ", round(lambda_d, 4)),
           grp_insee = paste0(pinsee0, "<br>", pinsee1),
           hr_lfu = paste0("HR<sub>LFU</sub>: ", hr_lfu)) %>%
    pivot_longer(cols=c('bias_abs_emr', 'bias_abs_emr_insee', 'bias_abs_emr_insee_imp'), names_to = "bias_abs") %>%
    mutate(bias_abs = factor(bias_abs, levels=c('bias_abs_emr', 'bias_abs_emr_insee', 'bias_abs_emr_insee_imp')))

  bias_grp <- ggplot(aes(x = grp_insee, y = value, color =bias_abs), data=data_tmp) +
    geom_hline(yintercept = 0) +
    geom_point() +
    scale_colour_manual(values = c("#FE6546", "#a5b800", "#45c1ff"),
                        labels=c('EMR', 'EMR_INSEE', 'EMR_INSEE_IMP')) +
    labs(color = "Estimation approach", x= NULL, y = "Absolute mean bias")  +
    facet_nested(rows = vars(lambda_d), cols = vars(hr_lfu, grp_lfu)) +
    theme_bw() +
    theme(strip.text.x =element_markdown(size=8), strip.text.y =element_markdown(size=8),
          axis.text.x = element_blank(),
          legend.position="bottom")

  table_insee <- data_tmp %>%
    pivot_longer(c(pinsee0, pinsee1), names_to = "insee", values_to = "pinsee") %>%
    mutate(insee_y = if_else(insee == "pinsee0", 0, 0.05)) %>%
    select(c("hr_lfu", "grp_lfu", "insee", "pinsee", "grp_insee", insee_y)) %>%
    distinct() %>%
    ggplot(aes(x = grp_insee)) +
    geom_text(aes(y = insee_y, label = pinsee), size=2.5) +
    scale_y_continuous(labels=c("p<sub>insee</sub> group 1", "p<sub>insee</sub> group 0"), breaks=c(0,0.05), limits =c(-0.1, .07)) +
    labs(y = "", x = NULL) +
    theme_minimal() +
    facet_grid(cols = vars(hr_lfu, grp_lfu))+
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_markdown(),
          panel.grid = element_blank(),
          strip.text = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm"))

  bias_grp_cbn <- bias_grp +
    plot_spacer() +
    table_insee  +
    plot_layout(ncol = 1,  heights = c(8, -0.3,  1), guides="collect") & theme(legend.position = 'bottom')

return (bias_grp_cbn)
}
