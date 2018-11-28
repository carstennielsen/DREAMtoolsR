###########################################################################################
###This function generates a vector of employment rate months for a vector of DEAM weeks###
###########################################################################################

#'DREAM benefit weeks to employment rate months
#'
#'Generates a vector of employment rate months that correspond to the input DREAM benefit weeks.
#' @param y_ Character vector of format 'y_'yyww (e.g. c("y_1701","y_1702",...))
#' @return A character vector of employment rate months (e.g. c("grad_2017_01","grad_2017_02 ",...)) that correspond to \code{y_}.
#' @export

y_2grad_ <- function(y_){


  if(!is.character(y_)){stop("Wrong input to function")}

  output <- NULL
  for(i in 1:length(y_)){

    if(is.na(y_[i])){
      push <- NA
    }else{

      ##Get date in format "%Y-W%V-%u" with thursday (day 4) as reference
      week <- paste(paste0("20",substr(as.character(y_[i]),3,4)),paste0("W",substr(as.character(y_[i]),5,6)),"4",sep="-")

      ##Find the date of week
      year.month <- ISOweek::ISOweek2date(week)

      ##push vector
      push <- paste0("grad_",substr(year.month,1,4),"_",substr(year.month,6,7))

    }

    ##Collect as output
    output <- c(output,push)

  }

##Return only unique output
return(unique(output))
}


