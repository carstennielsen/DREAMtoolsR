#############################################################################
###This function generates a vector of DREAM weeks for a range of quarters###
#############################################################################

#'Quaters to DREAM benefit weeks
#'
#'Generates a vector of DREAM benefit weeks that correspond to the input quarters.
#' @param qtr Character vector of format yy'Q'q (e.g. c("17Q1","17Q2",...))
#' @return A character vector of DREAM benefit weeks (e.g. c("y_1701","y_1702",...)) that correspond to \code{qtr}.
#' @export

qtr2y_ <- function(qtr){


  if(any(is.na(qtr)) | !is.character(qtr)){stop("Wrong input to function")}

  output <- NULL
  for(i in 1:length(qtr)){

    ##Generate vector of first 7 days in quarter
    first7days <- seq(as.Date(zoo::as.yearqtr(qtr[i],format="%yQ%q")),length.out=7,by="day")

    ##Find first thursday
    first.thursday <- first7days[format(first7days,format="%u")==4]

    ##Generate vector of week numbers
    weeks <- strftime(seq(first.thursday,as.Date(zoo::as.yearqtr(qtr[i],format="%yQ%q"),frac=1), by="week"),format="%V")

    ##Generate DREAM benefit week output
    output <- c(output,paste0("y_",substr(qtr[i],1,2),weeks))

  }

return(output)

}


