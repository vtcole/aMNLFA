#' aMNLFA sampling function
#'
#' This function generates a single-record dataset using a random sample of time points from the multiple-record sample.
#' @param input.object The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.
#' @keywords MNLFA
#' @export
#' @examples
#' aMNLFA.sample()


aMNLFA.sample<-function(input.object){

  path = input.object$path
  mrdata = input.object$mrdata
  myindicators = input.object$indicators
  myMeanImpact = input.object$meanimpact
  myVarImpact = input.object$varimpact
  myMeasInvar = input.object$measinvar
  mytime = input.object$time
  myauxiliary = input.object$auxiliary
  myID = input.object$ID

  mrdata<-read.table(paste(path,"/mr.dat",sep=""), header=TRUE,as.is = TRUE)
  varlist<-c(myID,myauxiliary,myindicators,myMeasInvar,myMeanImpact,myVarImpact)
  
  varlist<-unique(varlist)
  #Draw a calibration sample
  if (!is.null(mytime)){
    mrdata$ranuni<-runif(dim(mrdata)[1], min = 0, max = 1)
    mrdata<-mrdata[order(mrdata[myID],mrdata$ranuni),]
    srdata<-mrdata[!duplicated(mrdata[myID]),]
    srdata<-srdata[varlist]
    srdata <- sapply( srdata, as.numeric )
    srdata<-as.data.frame(srdata)
    header<-capture.output(prepareMplusData(srdata, paste(path,"/calibration.dat",sep=""), keepCols=c(varlist)))
  }
  if (is.null(mytime)){
    header<-capture.output(prepareMplusData(mrdata, paste(path,"/calibration.dat",sep=""), keepCols=c(varlist)))
  }
  mruse<-mrdata[varlist]
  mruse<-sapply(mruse,as.numeric)
  mruse<-as.data.frame(mruse)
  if (!is.null(mytime)) write.table(srdata, paste(path,"/srdata.dat",sep=""),quote=FALSE, sep="\t",col.names=TRUE,row.names=FALSE)
  if (is.null(mytime)) write.table(mruse, paste(path,"/srdata.dat",sep=""),quote=FALSE, sep="\t",col.names=TRUE,row.names=FALSE)
  header2<-capture.output(prepareMplusData(mruse, paste(path,"/full.dat",sep=""), keepCols=c(varlist)))


  h1file<-file(paste0(path,"/header.txt"))
  writeLines(header, h1file)
  close(h1file)
  h2file<-file(paste0(path,"/header2.txt"))
  writeLines(header2, h2file)
  close(h2file)
}
