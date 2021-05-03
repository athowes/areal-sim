area_plot <- function(geometry, title) {
  ggplot(geometry) +
    geom_sf() +
    theme_minimal() +
    labs(subtitle = paste0(title)) +
    theme_void()
}

Q_plots <- function(geometry) {
  iid <- diag(nrow(geometry))
  besag <- nb_to_precision(neighbours(geometry))
  ik <- solve(integrated_covariance(geometry, L = 50))
  
  cowplot::plot_grid(
    plot_matrix(iid),
    plot_matrix(besag),
    plot_matrix(ik),
    ncol = 3
  )
}

# Dynamite (bar with standard error) plot
dynamite_plot <- function(df, metric, title = NULL, y_lab = NULL, labs = FALSE, remove_constant = FALSE) {
  metric_mean <- paste0(metric, "_mean")
  metric_se <- paste0(metric, "_se")
  
  if(remove_constant){
    df <- df %>% filter(inf_model != "Constant")
  }
  
  if(is.null(y_lab)) {
    y_lab <- toupper(metric) 
  }
  
  ggplot(df, aes(x = inf_model, y = .data[[metric_mean]], fill = sim_model)) +
    geom_col(position = position_dodge(width = 0.9), alpha = 0.65) +
    geom_errorbar(aes(ymin = .data[[metric_mean]] - .data[[metric_se]], 
                      ymax = .data[[metric_mean]] + .data[[metric_se]]), 
                  position = position_dodge(width = 0.9), alpha = 0.65, width = 0.75) +
    facet_grid(geometry ~ sim_model, scales = "free") +
    labs(x = "Inferential model", y = y_lab, title = title, fill = "Simulation model") +
    guides(fill = labs) +
    scale_fill_manual(values = cbpalette) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

# Boxplots
boxplot <- function(df, metric, title = NULL, y_lab = NULL, labs = FALSE, remove_constant = FALSE) {
  
  .calc_boxplot_stat <- function(x) {
    coef <- 1.5
    n <- sum(!is.na(x))
    stats <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
    names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
    iqr <- diff(stats[c(2, 4)])
    outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
    if (any(outliers)) {
      stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
    }
    return(stats)
  }
  
  if(remove_constant){
    df <- df %>% filter(inf_model != "Constant")
  }
  
  if(is.null(y_lab)) {
    y_lab <- toupper(metric) 
  }
  
  ggplot(df, aes(x = inf_model, y = .data[[metric]])) +
    stat_summary(fun.data = .calc_boxplot_stat, geom = "boxplot", width = 0.5, alpha = 0.9, show.legend = FALSE, fill = lightgrey) +
    stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.5), alpha = 0.7, show.legend = FALSE) +
    facet_grid(geometry ~ sim_model, scales = "free") +
    labs(x = "Inferential model", y = y_lab, title = title, fill = "Simulation model") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

# Score cloropleths
scoropleth <- function(df_id, metric, g, sf, remove_constant = FALSE) {
  metric_mean <- paste0(metric, "_mean")
  
  df <- df_id %>% filter(geometry == g)
  
  if(remove_constant){
    df <- df %>% filter(inf_model != "Constant")
  }
  
  n_infsim <- nrow(unique(df[,c("inf_model", "sim_model")]))
  
  df %>%
    cbind(rep(sf$geometry, n_infsim)) %>%
    st_as_sf() %>%
    ggplot(aes(fill = .data[[metric_mean]])) +
    facet_grid(inf_model ~ sim_model) +
    geom_sf() +
    scale_fill_viridis() +
    labs(fill = toupper(metric)) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}

coverage_histogram <- function(full_df, g) {
  full_df %>%
    dplyr::filter(geometry == g) %>%
    mutate(quantile = replace(quantile, quantile > 1, 1)) %>%
    ggplot(aes(x = quantile)) +
    facet_grid(inf_model ~ sim_model, drop = TRUE, scales = "free") +
    geom_histogram(aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                   bins = 10, fill = lightgrey, col = "#ffffff", alpha = 0.9) +
    geom_hline(linetype = "dashed", yintercept = 0.1, col = "#000000") +
    labs(x = "Quantile", y = "Proportion", title = g)
}

# Only sim_model = "ik" has a true length-scale
lengthscale_plot <- function(df, inf_model, geometry, best = NA, subtitle = NA) {
  
  overall_mean <- df %>%
    filter(sim_model == "IK", inf_model == !!inf_model, geometry == !!geometry) %>%
    summarise(overall_mean = mean(mean)) %>%
    as.numeric()
  
  df %>%
    filter(sim_model == "IK", inf_model == !!inf_model, geometry == !!geometry) %>%
    select(mean, upper, lower) %>%
    tibble::rownames_to_column(var = "id") %>%
    ggplot(aes(x = id, y = mean)) +
    geom_point(alpha = 0.5) + 
    geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.5) +
    geom_hline(yintercept = 2.5, col = lightblue, size = 1.5) +
    geom_hline(yintercept = overall_mean, col = lightgreen, size = 1.5) +
    geom_hline(yintercept = best, col = "#E69F00", size = 1.5) +
    labs(x = "Simulation number", y = "Lengthscale", subtitle = subtitle) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
}
