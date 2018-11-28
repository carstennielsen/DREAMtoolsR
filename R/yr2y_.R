#############################################################################
###This function generates a vector of DREAM weeks for a range of quarters###
#############################################################################

#'Years to DREAM benefit weeks
#'
#'Generates a vector of DREAM benefit weeks that correspond to the input years.
#' @param yr Character vector of format yyyy (e.g. c("2015","2016",...))
#' @return A character vector of DREAM benefit weeks (e.g. c("y_1701","y_1702",...)) that correspond to \code{yr}.
#' @export

yr2y_ <- function(yr){

  if(any(is.na(yr)) | !is.character(yr)){stop("Wrong input to function")}

  output <- NULL
  for(i in 1:length(yr)){

    first7days <- seq(as.Date(paste0(yr[i],"-01-01")),length.out=7,by="day")
    first.thursday <- first7days[format(first7days,format="%u")==4]

    weeks <- strftime(seq(first.thursday,as.Date(paste0(yr[i],"-12-31")), by="week"),format="%V")

    output <- c(output,paste0("y_",substr(yr[i],3,4),weeks))

  }

  return(output)

}
