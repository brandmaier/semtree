check_ctsem_available <- function() {
  if (!requireNamespace("ctsem", quietly = TRUE)) {
    stop(
      "Please install the 'ctsem' package to use ctsem model support.",
      call. = FALSE
    )
  }
}