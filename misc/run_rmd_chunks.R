#!/usr/bin/env Rscript

# Usage: Rscript misc/run-rmd-chunks.R path/to/vignette.Rmd
# Execute every R chunk, including eval=FALSE and purl=FALSE chunks.
# Chunk options, inline R, knitr caching and non-R engines are not evaluated.
# Run only trusted Rmd files: their chunks execute arbitrary R code.

read_rmd_chunks <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  chunks <- list()
  fence <- NULL
  for (i in seq_along(lines)) {
    line <- lines[[i]]
    if (is.null(fence)) {
      opening <- regexec("^ {0,3}(`{3,}|~{3,})(.*)$", line)
      parts <- regmatches(line, opening)[[1L]]
      if (!length(parts)) next
      fence <- parts[[2L]]
      header <- trimws(parts[[3L]])
      is_r <- grepl("^\\{r([[:space:],}]|$)", header)
      if (is_r && !grepl("\\}[[:space:]]*$", header)) {
        stop(sprintf("Malformed R chunk header at line %d", i), call. = FALSE)
      }
      label <- sub("^\\{r[[:space:]]*", "", header)
      label <- trimws(sub("[,}].*$", "", label))
      if (!nzchar(label) || grepl("=", label, fixed = TRUE)) {
        label <- sprintf("unnamed-chunk-%d", length(chunks) + 1L)
      }
      start <- i
      code <- character()
    } else {
      closing <- sprintf("^ {0,3}%s{%d,}[[:space:]]*$",
                         substr(fence, 1L, 1L), nchar(fence))
      if (grepl(closing, line)) {
        if (is_r) {
          chunks[[length(chunks) + 1L]] <- list(
            label = label, line = start, code = code
          )
        }
        fence <- NULL
      } else if (is_r) {
        code <- c(code, line)
      }
    }
  }
  if (!is.null(fence)) {
    stop(sprintf("Unclosed code fence at line %d", start), call. = FALSE)
  }
  chunks
}

run_rmd_chunks <- function(path, envir = new.env(parent = globalenv())) {
  path <- normalizePath(path, mustWork = TRUE)
  chunks <- read_rmd_chunks(path)
  if (!length(chunks)) stop("No R chunks found", call. = FALSE)
  previous_dir <- setwd(dirname(path))
  on.exit(setwd(previous_dir), add = TRUE)
  for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]
    message(sprintf("[%d/%d] %s (line %d)", i, length(chunks),
                    chunk$label, chunk$line))
    tryCatch({
      connection <- textConnection(chunk$code)
      tryCatch(
        source(connection, local = envir, echo = FALSE, print.eval = TRUE),
        finally = close(connection)
      )
    }, error = function(error) {
      stop(sprintf("Chunk '%s' at %s:%d failed: %s",
                   chunk$label, path, chunk$line, conditionMessage(error)),
           call. = FALSE)
    })
  }
  message(sprintf("Executed %d R chunks successfully.", length(chunks)))
  invisible(envir)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L) {
    stop("Usage: Rscript misc/run-rmd-chunks.R path/to/file.Rmd", call. = FALSE)
  }
  run_rmd_chunks(args[[1L]])
}
