# Table for specific metric
metric_table <- function(df, metric, title = NULL, subtitle = NULL, latex = FALSE, scale = 1, figures = 2) {
  
  metric_mean <- paste0(metric, "_mean")
  metric_se <- paste0(metric, "_se")
  
  df_wide <- df %>%
    select(geometry, sim_model, inf_model, !!metric_mean, !!metric_se) %>%
    rename(mean = !!metric_mean, se = !!metric_se) %>%
    mutate(mean = scale * signif(mean, figures), 
           se = scale * signif(se, figures),
           val = paste0(mean, " (", se, ")")) %>%
    select(-mean, -se) %>%
    tidyr::spread(inf_model, val)
  
  table <- df_wide %>%
    gt(rowname_col = "sim_model", groupname_col = "geometry") %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) %>%
    tab_header(title = title, subtitle = subtitle) %>%
    tab_spanner(
      label = "Inferential model", 
      columns = everything()
    ) %>%
    tab_stubhead("Simulation model")
  
  # The column with the minimum value of the metric
  min_ind <- pmap_int(
    df %>%
      select(geometry, sim_model, inf_model, !!metric_mean) %>%
      tidyr::spread(inf_model, !!metric_mean) %>%
      ungroup(),
    ~which.min(c(...))
  )
  
  # Adding bold for minimum value to the table
  for(i in seq_along(min_ind)) {
    table <- table %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(columns = min_ind[[i]], rows = i)
      )
  }
  
  if(latex){
    return(
      table %>%
        as_latex() %>%
        as.character() %>%
        cat()
    )
  } else {
    return(table)
  }
}

coverage_table <- function(full_df_renamed, latex = FALSE) {
  
  # Helper function for names
  cov_string <- function(name) paste0(name, "_", c(50, 80, 95), "%")
  names <- c(sapply(levels(full_df_renamed$inf_model), cov_string))
  
  coverage <- full_df_rho %>%
    mutate(in50 = between(quantile, 0.25, 0.75),
           in80 = between(quantile, 0.1, 0.9),
           in95 = between(quantile, 0.025, 0.975)) %>%
    group_by(geometry, sim_model, inf_model) %>%
    summarise("50%" = sum(in50) / n(),
              "80%" = sum(in80) / n(),
              "95%" = sum(in95) / n()) %>%
    tidyr::gather(variable, value, -(geometry:inf_model), factor_key = TRUE) %>%
    tidyr::unite(temp, inf_model, variable) %>%
    tidyr::spread(temp, value) %>%
    select(geometry, sim_model, all_of(names))
  
  # Helper function to remove leading zero from string x
  remove_leading_zero <- function(x) sub("^(-?)0.", "\\1.", x)
  
  table <- coverage %>%
    gt(rowname_col = "sim_model", groupname_col = "geometry") %>%
    tab_spanner_delim(delim = "_") %>%
    fmt_number(columns = everything(), rows = everything(), decimals = 2) %>%
    text_transform(locations = cells_body(), fn = remove_leading_zero) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups())
  
  if(latex){
    return(
      table %>%
        as_latex() %>%
        as.character() %>%
        cat()
    )
  } else {
    return(table)
  }
}