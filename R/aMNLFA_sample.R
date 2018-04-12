#' aMNLFA sampling function
#'
#' This function generates a single-record dataset using a random sample of time points from the multiple-record sample.
#' @param input.object The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.
#' @keywords MNLFA
#' @export
#' @examples
#'  wd <- tempdir()
#'  first<-paste0(system.file(package='aMNLFA'),"/examplefiles")
#'  the.list <- list.files(first,full.names=TRUE)
#'  file.copy(the.list,wd,overwrite=TRUE)
#'  
#'  ob <- aMNLFA::aMNLFA.object(dir = wd, 
#'  mrdata = xstudy, 
#'  indicators = paste0("BIN_", 1:12),
#'  catindicators = paste0("BIN_", 1:12), 
#'  meanimpact = c("AGE", "GENDER", "STUDY"), 
#'  varimpact = c("AGE", "GENDER", "STUDY"), 
#'  measinvar = c("AGE", "GENDER", "STUDY"),
#'  factors = c("GENDER", "STUDY"),
#'  ID = "ID",
#'  thresholds = FALSE)
#'  
#'  aMNLFA.sample(ob)

aMNLFA.sample<-function(input.object){

  dir = input.object$dir
  mrdata = input.object$mrdata
  myindicators = input.object$indicators
  myMeanImpact = input.object$meanimpact
  myVarImpact = input.object$varimpact
  myMeasInvar = input.object$measinvar
  mytime = input.object$time
  myauxiliary = input.object$auxiliary
  myID = input.object$ID

  varlist<-c(myID,myauxiliary,myindicators,myMeasInvar,myMeanImpact,myVarImpact)
  
  varlist<-unique(varlist)
  #Draw a calibration sample
  if (!is.null(mytime)){
    mrdata$ranuni<-stats::runif(dim(mrdata)[1], min = 0, max = 1)
    mrdata<-mrdata[order(mrdata[myID],mrdata$ranuni),]
    srdata<-mrdata[!duplicated(mrdata[myID]),]
    srdata<-srdata[varlist]
    srdata <- sapply( srdata, as.numeric )
    srdata<-as.data.frame(srdata)
    header<-utils::capture.output(MplusAutomation::prepareMplusData(srdata, fixPath(file.path(dir,"calibration.dat",sep="")), keepCols=c(varlist)))
  }
  if (is.null(mytime)){
    header<-utils::capture.output(MplusAutomation::prepareMplusData(mrdata, fixPath(file.path(dir,"calibration.dat",sep="")), keepCols=c(varlist)))
  }
  mruse<-mrdata[varlist]
  mruse<-sapply(mruse,as.numeric)
  mruse<-as.data.frame(mruse)
  if (!is.null(mytime)) utils::write.table(srdata, fixPath(file.path(dir,"srdata.dat",sep="")),quote=FALSE, sep="\t",col.names=TRUE,row.names=FALSE)
  if (is.null(mytime)) utils::write.table(mruse, fixPath(file.path(dir,"srdata.dat",sep="")),quote=FALSE, sep="\t",col.names=TRUE,row.names=FALSE)
  header2<-utils::capture.output(MplusAutomation::prepareMplusData(mruse, fixPath(file.path(dir,"full.dat",sep="")), keepCols=c(varlist)))


  h1file<-fixPath(file.path(dir,"header.txt"))
  writeLines(header, h1file)
  closeAllConnections()
  h2file<-fixPath(file.path(dir,"header2.txt"))
  writeLines(header2, h2file)
  closeAllConnections()
}


write.inp.file <- function(df, outfile) {
  #checking inputs
  if (!is.data.frame(df))
    stop("df argument needs to be a data.frame")
  if (!is.character(outfile))
    stop("outfile argument needs to be a character vector")
  # check if the outfile already exists, if it does clear it.
  if (file.exists(outfile)) file.remove(outfile)
  #replace missing values with ""
  df[is.na(df[,1]),1] <- ""
  
  # next we iteratively write the .inp file from our input dataframe.
  for (i in seq(nrow(df))){
    # if number of characters is <= 90, write line and move to next loop
    if (nchar(df[i,1])<=90) {
      cat(df[i,1],
          sep = "\n",
          file = outfile,
          append = TRUE)
      # if characters between 90 and 160 we break it into two lines, at a space
      # note this can easily be extended to more than 160, if this solution works
    } else {
      # identify space closest to character limit 90
      # first this locates all spaces in the current line
      sp <- as.matrix(stringr::str_locate_all(df[i,1], " ")[[1]])
      # then we select only spaces prior to the 90th character
      sp <- sp[sp[,1]<=90,1]
      # select the space closest to character limit
      sp <- sp[length(sp)]
      # cut vector at that space, and write as two lines
      cat(
        # write first line from 1 to the space "sp"
        substr(df[i,1], 1, sp),
        # write the second line from "sp" to the length of the vector
        substr(df[i,1], sp, nchar(df[i,1])),
        sep = "\n",
        file = outfile,
        append = TRUE)
    }
  }
}

#Remove final slash to allow for better use of file.path down the line.
fixPath<-function(somecharacter) {
  lastcharacter<-base::substring(somecharacter,nchar(somecharacter))
  newcharacter<-base::substr(somecharacter,1,nchar(somecharacter)-1)
  outcharacter<-somecharacter
  if (lastcharacter=="/") {outcharacter<-newcharacter}
  outcharacter
}