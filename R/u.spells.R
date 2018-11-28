###############################################################################
###This function computes the run lengths of a vector of DREAM benefit codes###
###############################################################################

#'DREAM unemployment spell encoding
#'
#'Computes the length, start, and end, of unemployment spells in a DREAM dataset.
#'Unemployment is everything that is not employment. Employment is (by default) defined as 4 consecutive weeks without benefit and with an income the corresponding months.
#' @param data A \code{data.frame} (or \code{data.table}) containing a DREAM dataset.
#' @param row An integer valued row number (1 to \code{nrow}(data)).
#' @param period A character vector of DREAM weeks, e.g. \code{c("y_1701","y_1702","y_1703",...)}. If \code{NULL} then all DREAM benefit weeks in \code{data} is used. The default is \code{NULL}.
#' @param other.exit An integer vector of DREAM benefit codes that identifies exit other than to employment. If \code{NULL} then no other exit. The default is DREAM benefit codes \code{c(651,652)}.
#' @param other.censor An integer vector of DREAM benefit codes that identifies right censoring other than censoring of observations. If \code{NULL} then no other censoring. The default is DREAM benefit codes \code{c(997,998,999)}.
#' @param emp.weeks An integer that defines the number of consecutive weeks without benefit and with an income the corresponding months needed for employment. The default is \code{4}.
#' @return A list with components:\cr\cr
#' \code{lengths}: An integer vector containing the length of each run.\cr\cr
#' \code{start}: A character vector of same length as \code{lengths} with start weeks. \code{NA} imply that the spell start prior to first week in \code{period}.\cr\cr
#' \code{end}: A character vector of same length as \code{lengths} with end weeks. \code{NA} imply that the spell end after last week in \code{period}.\cr\cr
#' \code{right.censored}: A logical vector (\code{TRUE/FALSE}) of same length as \code{lengths} which indicates whether the spell is right censored.
#' @export

u.spells <- function(data, row, period=NULL, other.exit=c(651,652), other.censor=c(997,998,999),  emp.weeks=4){

  ##Subset data to get a row
  if(is.null(period)){
    y_ <- names(data)[substr(names(data),1,2)=="y_"]
  }else{
    y_ <- names(data)[names(data) %in% period]
  }

  ##Vector TRUE is no benefit and FALSE otherwise
  no.benefit <- is.na(as.data.frame(data)[row, y_])

  ##Vector TRUE if income and FALSE otherwiese
  income <- sapply(y_, FUN=function(x){!is.na(as.data.frame(data)[row,DREAMfun::y_2grad_(x)])})

  ##success includes education,
  if(!is.null(other.exit) | !is.null(other.censor)){
    ##then find the the relevant benefit codes in DREAM
    exit <- as.data.frame(data)[row, y_] %in% c(other.exit,other.censor)
    success <- (no.benefit & income) | exit
  ##Otherwise define success as no.benefit and some income
  }else{
    success <- no.benefit & income
  }


  ##Start calculations
  x <- as.numeric(!success)
  n <- length(x)
  y<- x[-1L] != x[-n]
  i<- c(which(y), n)

  ##Successes of less than min.weeks will be defined as faliure
  x[i][x[i]==0 & diff(c(0L, i)) < emp.weeks] <- 1

  m <- length(x[i])
  z <- x[i][-1L] != x[i][-m]
  j <- c(which(z),m)

  ##Find start/end benefit weeks
  start <- NA
  end <- NA
  for( k in 1:length(i[j]) ){
    if( k==1 ){
      start[k] <- NA
    }else{
      start[k] <- y_[i[j][k-1]+1]
      }
    if( k==length(i[j]) ){
      end[k] <- NA
    }else{
      end[k] <- y_[i[j][k]]
    }
  }

return(list( lengths = diff(c(0L, i[j]))[x[i][j]==1],
             start = c(start[x[i][j]==1]),
             end = c(end[x[i][j]==1]),
             right.censored = end[x[i][j]==1] %in% names(data[row, y_] %in% other.censor) | is.na(end[x[i][j]==1]) ))


}
