HMPL <- function(net) {
  d = sna::geodist(net)[["gdist"]]
  d = d[upper.tri(d) | lower.tri(d)]
  return(mean(d^-1)^-1)
}

# Visualize network data based on fixed coordinates
subplot_viz_fixed <- function(fixed_coords, net){
  fixed_coord_s <- create_layout(net, layout = 'fr')
  coords_we_need <- fixed_coord[fixed_coord$name %in% fixed_coord_s$name, c(1:3,12)]
  fixed_coord_s$x <- coords_we_need$x
  fixed_coord_s$y <- coords_we_need$y
  
  p <- fixed_coord_s  %>% 
    ggraph() + 
    geom_edge_link(width = 0.4, alpha = 0.5, color = "gray70") +
    geom_node_point(aes(size = degree,
                        color = factor(role_refined_rename,
                                       name_levels)),
                    alpha = .9) +
    scale_size_continuous(range = c(.2,2.5)) +
    theme_void() +
    #scale_color_viridis_d(option = 'A', begin = .2, end = .8) +
    scale_color_manual(values = viridis::inferno(n = 20)[c(7,10,13,16,19)]) +
    xlim(c(-23,24.5)) +
    ylim(c(-25.5,17.5))
  return(p)
}

# Make a results table cleanly for the SM
ergm_result_table <- function(ergm_output){
  sum <- summary(ergm_output)
  bic <- round(sum$bic[1])
  results <- data.frame(sum$coefficients)
  results <- results[,c(1,2,5)]
  colnames(results) = c("Estimate", "SE", "p")
  results$Estimate <- round(results$Estimate, 2)
  results$SE <- round(results$SE, 2)
  results$p <- round(results$p, 3)
  results$Estimate <- ifelse(results$p <= 0.05 & results$p >= 0.01,
                             paste0(results$Estimate, '**'),
                             ifelse(results$p < 0.01, paste0(results$Estimate, '***'),
                                    results$Estimate))
  results$Estimate <- paste0(results$Estimate, '\n(', results$SE, ')')
  results$Coefficient <- rownames(results)
  results <- select(results, Coefficient, Estimate)
  rownames(results) <- NULL
  results <- rbind(results, c("BIC", bic))
  results <- results %>% 
    mutate(Coefficient = case_when(
      Coefficient == "gwdeg.fixed.0.1" ~ "Anti-centralization",
      Coefficient == "gwesp.fixed.0.7" ~ "Triadic closure",
      Coefficient == "gwesp.fixed.0.8" ~ "Triadic closure",
      Coefficient == "gwesp.fixed.0.9" ~ "Triadic closure",
      Coefficient == "gwesp.fixed.0.6" ~ "Triadic closure",
      Coefficient == "gwesp.fixed.0.5" ~ "Triadic closure",
      Coefficient == "nodefactor.role_refined.govt" ~ "Actor-type: Government", 
      Coefficient == "nodefactor.role_refined.org" ~ "Actor-type: Organization",
      Coefficient == "nodefactor.role_refined.production" ~ "Actor-type: Producer",
      Coefficient == "nodefactor.role_refined.post-production" ~ "Actor-type: Company",
      Coefficient == "nodefactor.role_refined.university_ext" ~ "Actor-type: University & extension",
      Coefficient == "nodematch.geo_scale.USA National" ~ "National homophily: US National", 
      Coefficient == "nodematch.geo_scale.International" ~ "Regional homophily: International", 
      Coefficient == "nodematch.geo_scale.North Central Regional" ~ "Regional homophily: North Central",
      Coefficient == "nodematch.geo_scale.Northeast Regional" ~ "Regional homophily: Northeast", 
      Coefficient == "nodematch.geo_scale.South Regional" ~ "Regional homophily: South", 
      Coefficient == "nodematch.geo_scale.West Regional" ~ "Regional homophily: West", 
      Coefficient == "nodefactor.geo_scale.North Central Regional" ~ "Region: North Central",
      Coefficient == "nodefactor.geo_scale.Northeast Regional" ~ "Region: Northeast", 
      Coefficient == "nodefactor.geo_scale.South Regional" ~ "Region: South", 
      Coefficient == "nodefactor.geo_scale.West Regional" ~ "Region: West", 
      Coefficient == "nodefactor.lvl.2" ~ "Scale: National/International (vs. Regional)",
      Coefficient == "nodefactor.national.TRUE" ~ "Scale: National & international (vs. Regional)",
      Coefficient == "nodefactor.scale.national / international" ~ "Scale: National & international",
      Coefficient == "nodefactor.scale.National" ~ "Scale: National (vs. Regional)",
      Coefficient == "nodefactor.scale.International" ~ "Scale: International (vs. Regional)",
      Coefficient == "edgecov.hornet" ~ "Multi-functional tie", 
      Coefficient == "edges" ~ "Edges", 
      Coefficient == "nodecov.generic_count" ~ "N generic connections", 
      Coefficient == "nodefactor.respondent.FALSE" ~ "Survey non-respondent", 
      Coefficient == "nodecov.n_respondents_in_org" ~ "N respondents per node",
      Coefficient == "offset(edgecov.matrices[[1]])" ~ "Fixed -Inf", 
      Coefficient == "offset(edgecov.matrices[[2]])" ~ "Fixed -Inf", 
      Coefficient == "offset(edgecov.matrices[[3]])" ~ "Fixed -Inf", 
      T ~ "")) %>% 
    mutate(Coefficient = factor(Coefficient, coef_order)) %>% 
    arrange(Coefficient) 
  return(results)
}

## Plotting functions ----
prep_models <- function(models, names, n){
  model_list <- data.frame()
  for(i in 1:n){
    if(class(models[[i]]) == "bergm"){
      post_dist <- data.frame(models[[i]]$Theta)
      ci_est <- apply(post_dist, 2, bayestestR::ci)
      df <- data.frame('term' = models[[i]]$specs,
                       'Estimate' = apply(post_dist, 2, mean),
                       'LCI' = unlist(lapply(ci_est, function(x) x[[2]])),
                       'UCI' = unlist(lapply(ci_est, function(x) x[[3]])),
                       "p_val" = NA)
    } else{
      df <- summary(models[[i]])
      df <- as.data.frame(df$coefficients)
      df$CI_abs <- 1.96 * df$`Std. Error`
      df$LCI = df$Estimate - df$CI_abs
      df$UCI = df$Estimate + df$CI_abs
      df$term <- rownames(df)
      df <- df[,c('term', 'Estimate', 'LCI', 'UCI', "Pr(>|z|)")]
      colnames(df)[5] <- 'p_val'
    }
    df <- df %>% 
      mutate(term = case_when(
        term == "gwdeg.fixed.0.1" ~ "Anti-centralization",
        term == "gwdeg.fixed.0.5" ~ "Anti-centralization",
        term == "gwdeg.fixed.0.693147180559945" ~ "Anti-centralization",
        term == "gwesp.fixed.0.693147180559945" ~ "Triadic closure",
        term == "gwesp.fixed.0.7" ~ "Triadic closure",
        term == "gwesp.fixed.0.8" ~ "Triadic closure",
        term == "gwesp.fixed.0.9" ~ "Triadic closure",
        term == "gwesp.fixed.0.6" ~ "Triadic closure",
        term == "gwesp.fixed.0.5" ~ "Triadic closure",
        term == "nodefactor.role_refined.govt" ~ "Actor-type: Government", 
        term == "nodefactor.role_refined.org" ~ "Actor-type: Organization",
        term == "nodefactor.role_refined.production" ~ "Actor-type: Producer",
        term == "nodefactor.role_refined.post-production" ~ "Actor-type: Company",
        term == "nodefactor.role_refined.university_ext" ~ "Actor-type: University & extension",
        term == "nodematch.Region_scale.National US" ~ "Regional-scale homophily: National US", 
        term == "nodematch.Region_scale.National" ~ "Regional-scale homophily: National & international", 
        term == "nodematch.Region_scale.International" ~ "Regional-scale homophily: International", 
        term == "nodematch.Region_scale.North Central" ~ "Regional-scale homophily: North Central",
        term == "nodematch.Region_scale.Northeast" ~ "Regional-scale homophily: Northeast", 
        term == "nodematch.Region_scale.South" ~ "Regional-scale homophily: South", 
        term == "nodematch.Region_scale.West" ~ "Regional-scale homophily: West", 
        term == "nodematch.geo_scale.USA National" ~ "National homophily: US National", 
        term == "nodematch.geo_scale.International" ~ "Regional homophily: International", 
        term == "nodematch.geo_scale.North Central Regional" ~ "Regional homophily: North Central",
        term == "nodematch.geo_scale.Northeast Regional" ~ "Regional homophily: Northeast", 
        term == "nodematch.geo_scale.South Regional" ~ "Regional homophily: South", 
        term == "nodematch.geo_scale.West Regional" ~ "Regional homophily: West", 
        term == "nodematch.role_refined" ~ "Homophily: Actor-type",
        term == "nodefactor.national.TRUE" ~ "Scale: National & international",
        #term == "nodefactor.scale.national" ~ "Scale: National & international",
        term == "nodefactor.scale.national / international" ~ "Scale: National & international",
        term == "nodefactor.lvl.2" ~ "Scale: National/International (vs. Regional)",
        term == "nodefactor.lvl.TRUE" ~ "Scale: National/International (vs. Regional)",
        term == "nodefactor.scale.National" ~ "Scale: National (vs. Regional)",
        term == "nodefactor.scale.International" ~ "Scale: International (vs. Regional)",
        term == "edgecov.network(value_in_info_mat, direct = F)" ~ "Other subsystem structure",
        term == "edgecov.value_in_info_mat_ui" ~ "Multi-functional tie",
        term == "edgecov.info_in_value_mat_ui" ~ "Multi-functional tie",
        term == "edgecov.network(info_in_value_mat, direct = F)" ~ "Other subsystem structure",
        term == "edgecov.multiplex_simple_list[[(i - 1)]]" ~ "Multi-functional tie",
        term == "edgecov.hornet" ~ "Multi-functional tie", 
        term == "edges" ~ "Edges", 
        term == "nodecov.generic_count" ~ "N generic connections", 
        term == "nodefactor.respondent.FALSE" ~ "Survey non-respondent", 
        term == "nodecov.n_respondents_in_org" ~ "N respondents per node",
        term == "offset(edgecov.offset_mat)" ~ "Fixed -Inf", 
        term == "offset(edgecov.matrices[[1]])" ~ "Fixed -Inf", 
        term == "offset(edgecov.matrices[[2]])" ~ "Fixed -Inf", 
        term == "offset(edgecov.matrices[[3]])" ~ "Fixed -Inf", 
        term == "offset(edgecov.matrices[[4]])" ~ "Fixed -Inf", 
        term == "offset(edgecov.matrices[[5]])" ~ "Fixed -Inf",
        term == "offset(edgecov.matrices4[[i]])" ~ "Fixed -Inf", 
        term == "offset(edgecov.matrices_ui[[2]])" ~ "Fixed -Inf", 
        term == "offset(edgecov.matrices_ui[[3]])" ~ "Fixed -Inf", 
        
        T ~ ""))
    df$model <- names[i]
    df <- df %>% 
      mutate(term = factor(term, coef_order)) %>% 
      arrange(model, term)
    model_list <- rbind(df, model_list)
    
  }
  return(model_list)
}


manual_coef_plot <- function(df, color_vals){
  if(length(color_vals) > 2 & length(color_vals) < 5){
    n_legend_row = 2
    #legend_just = 1
  } else  if(length(color_vals) >= 5){
    n_legend_row = 3
    #legend_just = 1
  }else {
    n_legend_row = 1
    #legend_just = 1
  }
  df %>% 
    filter(!(term %in% rm_coefs)) %>% 
    ggplot(aes(x = Estimate, 
               y = factor(term, rev(unique(.$term))))) +
    ggstance::geom_pointrangeh(aes(xmin = LCI, 
                                   xmax = UCI,
                                   color = factor(model, rev(fx_models)), 
                                   shape = factor(model, rev(fx_models))),
                               position = ggstance::position_dodgev(height = .4), 
                               fill = "white", fatten = 2, size = 0.5) +
    #geom_errorbar(aes(xmin=`conf.high`,xmax=`conf.low`), width=0.1, position = pd) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25) + 
    scale_colour_manual(values = color_vals, 
                        limits = fx_models) + 
    scale_shape_manual(values = #21
                         (20:25),
                       limits = fx_models) +
    jtools::theme_nice(base_family = "Times") + 
    jtools::drop_y_gridlines() + 
    theme(axis.title.y = element_blank(),
          axis.title.x = element_text(size = 8),
          text = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.position = "bottom",
          #legend.justification = legend_just,
          legend.spacing.y = unit(.1, 'cm'),
          legend.margin=margin(-10, 40, 0, 0),
          panel.grid.major.x = element_line(linetype = "solid")) + 
    labs(x = "Coefficient Estimate (log-odds)", shape = "", color = "") +
    guides(color=guide_legend(nrow=n_legend_row ,byrow=TRUE),
           shape=guide_legend(nrow=n_legend_row ,byrow=TRUE))
}
