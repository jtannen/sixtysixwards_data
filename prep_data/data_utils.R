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

pretty_div <- function(warddiv){
  if(all(grepl("^[0-9]{4}$", warddiv))){
    paste0(substr(warddiv,1,2),"-",substr(warddiv,3,4))
  } else if(all(grepl("^[0-9]{2}-[0-9]{2}$", warddiv))) {
    warddiv
  } else {
    bad_div <- warddiv[!grepl("^[0-9]{2}-[0-9]{2}$", warddiv)][1]
    stop(sprintf("Don't understand warddiv format. E.g. %s", bad_div))
  }
}
