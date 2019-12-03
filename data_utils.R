asnum <- function(x) as.numeric(as.character(x))

most_recent_file <- function(stem, ext=".*"){
  path <- gsub("(.*/)([^/]+)$", "\\1", stem)
  stem <- gsub("(.*/)([^/]+)$", "\\2", stem)
  files <- list.files(path, pattern = paste0("^", stem))
  date_re <- "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  valid_names <- grep(paste0("^",stem,"_?", date_re,"\\.", ext), files, value=TRUE)
  return(paste0(path, max(valid_names)))
}

dated_stem <- function(stem, path=".", fileext=""){
  if(fileext != "") fileext <- paste0(".", fileext)
  sprintf("%s/%s_%s%s", path, stem, Sys.Date(), fileext)
}
