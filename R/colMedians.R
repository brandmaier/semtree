
colMedians <- function(x, na.rm = TRUE)
{
  return(apply(
    X = x,
    FUN = function(x) {
      median(x, na.rm = na.rm)
    },
    MARGIN = 2
  ))
}