###############################################################################
###This function computes the run lengths of a vector of DREAM benefit codes###
###############################################################################

#'DREAM run length encoding
#'
#'Computes the length, values, start, and end, of runs of equal values in a DREAM dataset.
#' @param data A \code{data.frame} (or \code{data.table}) containing a DREAM dataset.
#' @param row An integer valued row number (1 to \code{nrow}(data)).
#' @param period A character vector of DREAM weeks (e.g. c("y_1701","y_1702","y_1703",...)). If \code{NULL}, the default, all DREAM benefit weeks in \code{data} is used.
#' @return A list with components:\cr
#' \code{lengths} An integer vector containing the length of each run.\cr
#' \code{values} A integer vector of same length as \code{lengths} with the corresponding values.\cr
#' \code{start} A character vector of same length as \code{lengths} with start weeks. \code{NA} imply that the run start prior to first week in \code{period}.\cr
#' \code{end} A character vector of same length as \code{lengths} with end weeks. \code{NA} imply that the run end after last week in \code{period}.
#' @export

runs<-function(data,row,period=NULL)
{

  ##Subset data to get a row
  if(is.null(period)){
    y_ <- names(data)[substr(names(data),1,2)=="y_"]
    x <- as.data.frame(data)[row,y_]
  }else{
    x <- as.data.frame(data)[row,period]
  }

  ##Get length of row
  n<- length(x)

  ##If length is 0 then return
  if (n == 0L)
    return(list(lengths=integer(),values=x,start=NA,end=NA))

  ##Set identifiers to FALSE
  naRepFlag<-FALSE
  IS_LOGIC<-FALSE

  ####################
  ###Mask NA values###
  ####################

  ##If any of the cells in x are NA
  if(any(is.na(x))){
    ##Then set NA flag to TRUE
    naRepFlag<-TRUE
    ##Then set IS_LoGIC flag to TRUE is x is logical, otherwise FALSE
    IS_LOGIC<-ifelse(typeof(x)=="logical",TRUE,FALSE)

      ##If x is logical, then set NA mask value to 2
      if(typeof(x)=="logical"){
        naMaskVal<-2
      ##If x is character, then set NA mask to 32 random letters
      }else if(typeof(x)=="character"){
        naMaskVal<-paste(sample(c(letters,LETTERS,0:9),32,replace=T),collapse="")
      ##Else set NA mask value to the max cell value added 1
      }else{
        z<-as.numeric(x)
        naMaskVal<-max(0,abs(z[!is.infinite(z)]),na.rm=T)+1
      }

      ##Replace NA cell with mask values
      x[1,which(is.na(x))]<-naMaskVal
  }

  ##Calculate length of run
  z<-as.numeric(x)
  y<- z[-1L] != z[-n]
  i<- c(which(y), n)

  ######################
  ###Unmask NA values###
  ######################

  if(naRepFlag)
    x[1,which(x==naMaskVal)]<-NA

  if(IS_LOGIC)
    x<-as.logical(x)

  ####################################
  ###Find start and end DREAM weeks###
  ####################################

  start <- NA
  end <- NA
  for(k in 1:length(i)){
    if(k==1){start[k] <- NA}else{start[k] <- names(x)[i[k-1]+1]}
    if(k==length(i)){end[k] <- NA}else{end[k] <- names(x)[i[k]]}
  }


##Return a list containing length, values, start, and end
return(list(lengths=diff(c(0L, i)),values=as.numeric(x)[i],start=c(start),end = c(end)))

}
