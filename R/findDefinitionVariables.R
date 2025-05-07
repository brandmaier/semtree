findDefinitionVariables <- function(model) {
  def_vars <- c()
  for (mid in 1:length(model@matrices)) {
    mt <- model@matrices[[mid]]
    idx <- startsWith(mt$labels, "data.")
    if (any(idx, na.rm = TRUE)) {
      idx[is.na(idx)] <- FALSE
      labs <- mt$labels[idx]
      labs <- sapply(labs, function(x) {
        substr(x, 6, nchar(x))
      })
      def_vars <- c(def_vars, labs)
    }
  }

  return(def_vars)
}
