#' Convert the rows of a dataframe to a list.
#' 
#' @param df A dataframe.
rows_to_list <- function(df) {
  x <- as.list((data.frame(t(df))))
  names(x) <- NULL
  return(x)
}

#' Bind the rows of a list together, adding an `id` column.
#' 
#' @param list A list.
list_to_df <- function(list){
  data.frame(dplyr::bind_rows(list, .id = "replicate"))  
}

#' Check that the directory you plan to save to exists before saving.
#' 
#' @param object An object to be saved.
#' @param output_dir The output directory.
#' @param file The name of the file to be saved to (without the preceeding `output_dir`).
safe_saveRDS <- function(object, output_dir, file) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message(output_dir, " was created")
  }
  saveRDS(object, file = paste0(output_dir, "/", file, ".rds"))
}

#' A dictionary to convert from internal names to names used in the manuscript.
#' 
#' @param df A dataframe.
rename_df <- function(df) {
  mutate(df,
         geometry = recode_factor(
           geometry, 
           "grid" = "Grid", 
           "ci" = "Cote d'Ivoire", 
           "tex" = "Texas"),
         sim_model = recode_factor(
           sim_model, 
           "iid" = "IID", 
           "icar" = "Besag", 
           "ik" = "IK"),
         inf_model = recode_factor(
           inf_model, 
           "constant" = "Constant", 
           "iid" = "IID", 
           "icar" = "Besag",
           "besag" = "Besag",
           "bym2" = "BYM2",
           "bym" = "BYM2",
           "fck" = "FCK", 
           "ck" = "CK", 
           "fik" = "FIK", 
           "ik" = "IK")
  )
}

extract_results <- function(variable) {
  do.call("rbind", lapply(files, FUN = function(file) {
    assess <- readRDS(file = paste0("data/results/", ctx_ver, "/", file))
    if(purrr::is_empty(assess[[variable]])) {
      return(NULL)
    }
    df <- assess[[variable]]
    meta_data <- strsplit(file, '[/_.]')
    df$geometry <- meta_data[[1]][1]
    df$sim_model <- meta_data[[1]][2]
    df$inf_model <- meta_data[[1]][3]
    return(df)
  })) %>%
    rename_df()
}

group_mean_and_se <- function(df, group_variables) {
  dots <- lapply(group_variables, as.symbol)
  df %>%
    select(-c(obs, mean, mode, lower, upper)) %>%
    group_by_(.dots = dots) %>%
    summarise(n = n(), across(mse:lds, list(mean = mean, se = ~ sd(.x) / sqrt(length(.x)))))
}
