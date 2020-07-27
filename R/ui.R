ui_info <- function(...) {
  ui(crayon::yellow(clisymbols::symbol$info), ...)
}

ui_ok<- function(...) {
 ui(crayon::green(clisymbols::symbol$tick), ...)
}

ui_fail<- function(...) {
  ui(crayon::red(clisymbols::symbol$cross), ...)
}

ui_stop <- function(...) {
  ui_fail(...)
  stop(call.=FALSE)
}

ui_warn<- function(...) {
  ui(crayon::yellow(clisymbols::symbol$cross), ...)
}

ui_message <- function(...) {
  ui(clisymbols::symbol$pointer, ...) 
}

ui_bullet <- function(...) {
  ui(clisymbols::symbol$bullet, ...) 
}

ui_debug <- function(...) {
  ui(crayon::blue(clisymbols::symbol$circle_circle), ...) 
}


ui <- function(symbol, ...) {
  x <- list(...)
  x <- paste0(x, collapse = "")
  if (!endsWith(x,"\n")) x <- paste0(x,"\n",collapse = "")
  x <- paste0(symbol," ", x,collapse="")
  cat(x)
}

horiz.line <- function(size=10)
{
  paste0(rep(clisymbols::symbol$upper_block_1,size),sep="",collapse = "")
}

human_readable_time <- function(x)
{
  if (x < 1) 
  { return("less than a second")
  } else if (x < 60) {
    return (paste0(round(x),"s"))
  } else if (x < 600) {
   # round to half-minutes:
    paste0("~",(round(x/30)*30)/60,"min")
  } else if (x < 3600) {
    paste0("~",round(x/60),"min")
  } else {
    paste0("~",round(x/3600*10)/10,"h")
  }
}