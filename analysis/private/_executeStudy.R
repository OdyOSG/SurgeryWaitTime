# A. File Info -----------------------

# Task: Execute Study


# B. Functions ------------------------

findConfigBlock <- function(lines, startBlock = "# <<<", endBlock = "# >>>") {

  start <- which(lines == "# <<<")
  end <- which(lines == "# >>>")

  ll <- c(start + 1L, end - 1L)

  return(ll)
}


prepStudyTask <- function(lines, value) {

  blockLines <- findConfigBlock(lines)
  start <- blockLines[1]
  end <- blockLines[2]

  configBlock <- lines[rlang::seq2(start, end)]

  new_configBlock <- gsub("\\[block\\]", value, configBlock)

  lines2 <- c(lines[rlang::seq2(1, start - 1L)], new_configBlock, lines[rlang::seq2(end + 1, length(lines))]) |>
    paste(collapse = "\n")

  return(lines2)
}


runStudyTask <- function(file, configBlock, env = rlang::caller_env()) {

  rLines <- readr::read_lines(file)

  newLines <- prepStudyTask(lines = rLines, value = configBlock)

  exprs <- rlang::parse_exprs(newLines)

  res <- NULL

  for (i in seq_along(exprs)) {
    res <- eval(exprs[[i]], env)
  }

  invisible(res)
}
