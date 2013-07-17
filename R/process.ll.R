#' Process lat/long
#'
#' Internal function.
#'
#' @author David L. Miller
process.ll <- function(x){

  # let N and E be +ve and S and W be -ve
  x.sign <- ifelse(grepl("^[NE]",x),1,-1)
  x <- sub("^[NEWS]","",x)

  # pull the degrees and minutes (decimal
  degs <-  as.numeric(sub("^(\\d+) \\d+\\.\\d+","\\1",x))
  mins <- as.numeric(sub("^\\d+ (\\d+\\.\\d+)","\\1",x))

  # need to then convert to decimal lat/long and return
### check this!
  return(x.sign*(degs+mins/100))

}
