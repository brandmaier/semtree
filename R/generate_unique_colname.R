#' 
#' helper function to randomly generate a unique column name
#' This is useful for creating temporary split variables representing 
#' a given dichotomous split.
#' 
#' @param df A data.frame
#' @param prefix String. A prefix for the column
#' @param n The length of the random string
#' 
#' @noRd
generate_unique_colname <- function(df, prefix = "col", n = 12) {
  repeat {
    name <- paste0(prefix, "_",
                   paste(sample(c(letters, LETTERS, 0:9), n, replace = TRUE),
                         collapse = ""))
    if (!name %in% names(df)) return(name)
  }
}