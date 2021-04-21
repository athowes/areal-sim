# data.frame rows to list
rows_to_list <- function(df) {
  x <- as.list((data.frame(t(df))))
  names(x) <- NULL
  return(x)
}

# with an id column
list_to_df <- function(list){
  data.frame(dplyr::bind_rows(list, .id = "replicate"))  
}

safe_saveRDS <- function(object, output_dir, file) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message(output_dir, " was created")
  }
  saveRDS(object, file = paste0(output_dir, "/", file, ".rds"))
}

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
           "icar" = "ICAR", 
           "ik" = "IK"),
         inf_model = recode_factor(
           inf_model, 
           "constant" = "Constant", 
           "iid" = "IID", 
           "icar" = "Besag", 
           "bym" = "BYM2",
           "fck" = "FCK", 
           "ck" = "CK", 
           "fik" = "FIK", 
           "ik" = "IK")
  )
}