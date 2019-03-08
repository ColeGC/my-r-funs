formatCase_sql <- function(d = script_dir, f = script_file) {
  d1 <- ifelse(grepl("[/\\]$", d), d, paste0(d, "/"))
  f1 <- ifelse(grepl(".sql$", f, ignore.case = TRUE), gsub(".sql$", "", f), f)
  fpath <- paste0(d1, f1, ".sql")
  script <- readLines(fpath)
  script_lower <- tolower(script)
  terms2upcase <- c("select", "create", "table", "by", "in", "as", "case", "when", "then", "is",
                    "union", "all", "drop", "count", "sum", "from", "distinct", "where", "on", 
                    "inner", "left", "right", "outer", "join", "else", "end", "set", "alter", 
                    "drop", "add", "delete", "column", "default", "update", "modify", "rename",
                    "offset", "rows", "fetch", "next", "only", "and", "or", "not", "eq", "null")
  gen_rx <- function(x) {
    rx <- paste0("\\<(", x, ")\\>")
    return(rx)
  }
  upcase_term <- function(x, terms) {
    rx <- gen_rx(terms)
    for(i in seq_along(rx)) {
      x <- gsub(rx[[i]], toupper(terms[[i]]), x)
    }
    return(x)
  }
  script_fmt <- upcase_term(script_lower, terms2upcase)
  writeLines(script_fmt, paste0(d1, f1, "_format.sql"))
}

script_dir <- "C:/Users/ColeGC/Dropbox (Personal)/Research/Breast Cancer and upper Extremity/Project/SQL Scripts"
script_file1 <- "Create Table of CMS Shoulder Diagnoses.sql"
formatCase_sql(d = script_dir, f = script_file1)