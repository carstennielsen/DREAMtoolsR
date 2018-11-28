##################################################################################################
###This function generates a vector of DREAM weeks for a range of DREAM employment ratio months###
#################################################################################################

#'DREAM employment ratio months to DREAM benefit weeks
#'
#'Generates a vector of DREAM benefit weeks that correspond to the input years.
#' @param grad_ Character vector of DREAM employment ratio months (e.g. c("grad_2017_01","grad_2017_02",...))
#' @return A character vector of DREAM benefit weeks (e.g. c("y_1701","y_1702",...)) that correspond to \code{yr}.
#' @export

grad_2y_ <- function(grad_){

  if(any(is.na(grad_)) | !is.character(grad_)){stop("Wrong input to function")}

  output <- NULL
  for(i in 1:length(grad_)){

    yr.md <- paste(substr(grad_[i],6,9),substr(grad_[i],11,12),sep="-")

    start <- as.Date(paste(yr.md,"01",sep="-"))

    end <- zoo::as.Date(zoo::as.yearmon(paste(yr.md, "01", sep = ""), "%Y-%m%d"), frac = 1)

    first7days <- seq(start,length.out=7,by="day")
    first.thursday <- first7days[format(first7days,format="%u")==4]

    weeks <- strftime(seq(first.thursday,end, by="week"),format="%V")

    output <- c(output,paste0("y_",substr(yr.md,3,4),weeks))

  }

  return(output)

}
