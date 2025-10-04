# This was recommended as a safe way to
# suppress CRAN warnings when using ggplot
# evaluations within aes() objects
utils::globalVariables(c(
  "decision", "last_col", "median_value", 
  "name", "rnd", "sort_value", "value"
))