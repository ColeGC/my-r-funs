rm_except <- function(x) {
  if(is.character(x) == FALSE) stop("x must be a vector of object names as strings")
  toKeep <- c(x, "rm_except")
  everything <- ls(envir = .GlobalEnv)
  to_rm_ind <- everything %in% toKeep
  if(sum(to_rm_ind) == 1) stop("Attempting to remove all objects in workspace.  Use rm(list = ls()).")
  to_rm <- everything[which(!to_rm_ind)]
  rm(list = to_rm, envir = .GlobalEnv)
}
