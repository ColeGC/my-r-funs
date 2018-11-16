getLogs <- function(path = ".", key = ".log$") {
  dirfiles <- dir(path)
  logs <- dirfiles[grepl(key, dirfiles)]
  return(logs)
}

search_log <- function(file, checkWords = c("multiple obs", "uninit", "warning", "error"), prev_len = 50) {
  text <- read_file(file)
  search_word <- function(x, w, l = prev_len) {
    has_word <- grepl(w, x, ignore.case = TRUE)
    if(has_word) {
      word_pos <- unlist(gregexpr(w, x, ignore.case = TRUE))
      word_prev <- lapply(word_pos, function(y) substring(x, y, y + l))
      return(word_prev)
    }
    return(NA)
  }
  has_word <- lapply(checkWords, function(x) search_word(text, x))
  names(has_word) <- checkWords
  has_word_list<- list(file, has_word, any(!is.na(has_word)))
  names(has_word_list) <- c("file", "search_detail", "any_issue")
  return(has_word_list)
}

check_all_files <- function(files, ...) {
  checkedFiles <- lapply(files, function(x) search_log(x, ...))
  names(checkedFiles) <- files
  return(checkedFiles)
}

check_dir <- function(path = ".", ...) {
  logs <- getLogs(path)
  checked <- check_all_files(files = logs, ...)
  return(checked)
}

# checked_logs <- check_dir()
# list2env(checked, envir = .GlobalEnv)
