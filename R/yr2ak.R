#############################################################################
###This function generates a vector of DREAM weeks for a range of quarters###
#############################################################################

#'Years to DREAM UI-fund membership
#'
#'Generates a vector of DREAM UI-fund membership that correspond to the input years.
#' @param yr Character vector of format yyyy (e.g. c("2015","2016",...))
#' @return A character vector of DREAM UI-fund membership (e.g. c("ak15","ak16",...)) that correspond to \code{yr}.
#' @export

yr2ak <- function(yr){

  if(any(is.na(yr)) | !is.character(yr)){stop("Wrong input to function")}

  output <- paste0("ak",substr(yr,3,4))

  return(output)

}
