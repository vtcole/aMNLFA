#' aMNLFA score generating function
#'
#' This function creates scores generated using aMNLFA. Can only be run after the final model has been fit -- i.e., after the aMNLFA.final function.
#' @param input.object The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.
#' @keywords MNLFA
#' @export
#' @examples
#' \dontrun{
#'  wd <- "./aMNLFA/data"
#   First create aMNLFA object.
#   ob <- aMNLFA::aMNLFA.object(dir          = wd,
#                            mrdata        = xstudy,
#                            indicators    = paste0("BIN_", 1:12), 
#                            catindicators = paste0("BIN_", 1:12), 
#                            meanimpact    = c("AGE", "GENDER", "STUDY"), 
#                            varimpact     = c("AGE", "GENDER", "STUDY"), 
#                            measinvar     = c("AGE", "GENDER", "STUDY"), 
#                            factors       = c("GENDER", "STUDY"), 
#                            ID            = "ID", 
#                            thresholds    = FALSE)
#'  aMNLFA.scores(ob)
#' }

aMNLFA.scores<-function(input.object){
  
  dir = input.object$dir
  mrdata = input.object$mrdata
  myindicators = input.object$indicators
  mycatindicators = input.object$catindicators
  mycountindicators = input.object$countindicators
  myMeanImpact = input.object$meanimpact
  myVarImpact = input.object$varimpact
  myMeasInvar = input.object$measinvar
  mytime = input.object$time
  myauxiliary = input.object$auxiliary
  myID = input.object$ID
  thresholds = input.object$thresholds
  
  mrdata<-read.table(paste(dir,"/mr.dat",sep=""), header=TRUE,as.is = TRUE,na.strings=".")
  srdata<-read.table(paste(dir,"/srdata.dat",sep=""),header=TRUE)
  
  varlist<-c(myID,myauxiliary,myindicators,myMeasInvar,myMeanImpact,myVarImpact)
  varlist<-unique(varlist)
  
  header<-readLines(paste0(dir,"/header.txt"))
  header2<-readLines(paste0(dir,"/header2.txt"))
  
  USEVARIABLES<-paste("USEVARIABLES=")
  semicolon<-paste(";")
  AUXILIARY<-paste("AUXILIARY=")
  AUXILIARY<-append(AUXILIARY,myauxiliary)
  AUXILIARY<-noquote(append(AUXILIARY,semicolon))
  AUXILIARY<-capture.output(cat(AUXILIARY))
  CATEGORICAL<-paste("CATEGORICAL=")
  CATEGORICAL<-append(CATEGORICAL,mycatindicators)
  CATEGORICAL<-noquote(append(CATEGORICAL,semicolon))
  CATEGORICAL<-capture.output(cat(CATEGORICAL))
  COUNT<-paste("COUNT=")
  COUNT<-append(COUNT,mycountindicators)
  COUNT<-noquote(append(COUNT,semicolon))
  COUNT<-capture.output(cat(COUNT))
  ANALYSIS<-noquote("ANALYSIS: ESTIMATOR=ML;ALGORITHM=INTEGRATION;INTEGRATION=MONTECARLO;PROCESSORS=4;")
  ETA<-paste("ETA BY ")
  l<-length(myindicators)
  loadings<-list()
  for (i in 1:l){
    loadings[i]<-paste(ETA,myindicators[i],"*(l",i,");",sep="")
  }
  loadings<-noquote(loadings)
  loadings<-unlist(loadings)
  tech1<-paste("OUTPUT: tech1;")
  MODEL<-paste("MODEL: [ETA@0]; ETA@1;")
  CONSTRAINT<-paste("CONSTRAINT=")
  varMODEL<-paste("MODEL: [ETA@0];ETA*(veta);")
  fixvarMODEL<-paste("MODEL: [ETA@0];ETA@1;")
  MODELCONSTRAINT<-paste("MODEL CONSTRAINT: new(")
  
  
  #############################################################################
  ###Read in final calibration model to obtain final parameter estimates#######
  ##############Output scoring model input#####################################
  #############################################################################
  
  round3output<-MplusAutomation::readModels(paste(dir,"/round3calibration.out",sep=""))
  round3input<-round3output$input$model.constraint
  
  keepvarimpact<-list()
  for (i in 1:length(round3input)){
    if (length(grep("+",round3input[i],value=TRUE,fixed=TRUE))>0) keepvarimpact<-append(keepvarimpact,round3input[i])
  }
  
  foo<-strsplit(as.character(keepvarimpact),"+",fixed=TRUE)
  foo<-strsplit(as.character(unlist(foo)),"*",fixed=TRUE)
  foo<-unlist(foo)
  foo<-foo[!is.na(match(foo,myVarImpact))]
  keepvarimpact<-capture.output(cat(foo))
  round3output<-as.data.frame(round3output$parameters$unstandardized)
  
  ETAPREDLIST<-round3output[which(round3output$paramHeader=="ETA.ON"),]
  
  
  constraints<-round3output[which(round3output$paramHeader=="New.Additional.Parameters"),]
  lambdaconstraints<-constraints[which(substr(constraints$param,1,1)=="L"),]
  lambdaconstraints$itemnum<-substr(lambdaconstraints$param,2,2)
  lambdaconstraints$item<-myindicators[as.numeric(lambdaconstraints$itemnum)]
  lambdaconstraints$predictornum<-substr(lambdaconstraints$param,3,4)
  lambdaconstraints$predictor<-ifelse(lambdaconstraints$predictornum=="00","intercept",myMeasInvar[as.numeric(substr(lambdaconstraints$param,3,3))])
  
  lambdainvlist<-unique(unlist(lambdaconstraints$item))
  
  lambda<-round3output[which(grep(".BY",round3output$paramHeader)>0),]
  
  round4<-unlist(round3output$param)
  keepround4<-myindicators
  for (i in 1:length(round4))
  {
    if (length(grep(round4[i],myMeasInvar))>0) keepround4<-append(keepround4,round4[i])
    if (length(grep(round4[i],myMeanImpact))>0) keepround4<-append(keepround4,round4[i])
    if (length(grep(round4[i],myVarImpact))>0) keepround4<-append(keepround4,round4[i])
  }
  keepround4<-unique(keepround4)
  keepround4<-keepround4[!is.na(keepround4)]
  keepround4<-capture.output(cat(keepround4))
  
  lc<-lambdaconstraints$predictor[which(lambdaconstraints$predictor!="intercept")]
  lc<-unique(lc)
  constraints<-round3output[which(round3output$paramHeader=="New.Additional.Parameters"),]
  varimpact<-constraints[which(substr(constraints$param,1,1)=="V"),]
  varimpact<-substr(varimpact$param,2,3)
  keepvarimpact<-myVarImpact[as.numeric(varimpact)]
  con<-unique(append(keepvarimpact,lc))
  CONSTRAINT<-noquote(append(CONSTRAINT,con))
  CONSTRAINT<-append(CONSTRAINT,semicolon)
  CONSTRAINT<-capture.output(cat(CONSTRAINT))
  CONSTRAINT<-ifelse(length(con)>0,CONSTRAINT,"!")
  
  meanimpact<-round3output[which(round3output$paramHeader=="ETA.ON"),]
  keepmeanimpact<-meanimpact$param
  ETAON3<-"!"
  if(length(keepmeanimpact)>0) ETAON3<-paste("ETA ON ",keepmeanimpact,"@",sep="")
  #writing scoring model
  scoringinput<-as.data.frame(NULL)
  
  scoringinput[1,1]<-paste("TITLE: Scoring Model")
  scoringinput[2,1]<-header2[2]
  scoringinput[3,1]<-header2[3]
  scoringinput[4,1]<-header2[4]
  scoringinput[5,1]<-header2[5]
  scoringinput[6,1]<-ifelse(length(header2)>5,header[6],"!")
  scoringinput[7,1]<-ifelse(length(header2)>6,header[7],"!")
  scoringinput[8,1]<-ifelse(length(header2)>7,header[8],"!")
  scoringinput[9,1]<-ifelse(length(header2)>8,header[9],"!")
  scoringinput[10,1]<-paste("USEVARIABLES= ",keepround4,semicolon,sep="")
  clus<-ifelse(is.null(mytime),"!",paste("cluster=",myID,semicolon,sep=""))
  aux<-append(myauxiliary,myID)
  aux<-unique(aux)
  aux<-ifelse(length(aux)>0,paste("AUXILIARY=",aux,semicolon,sep=""),"!")
  scoringinput[11,1]<-aux
  scoringinput[12,1]<-clus
  scoringinput[13,1]<-ifelse(length(mycatindicators)>0,CATEGORICAL,"!")
  scoringinput[14,1]<-ifelse(length(mycountindicators)>0,COUNT,"!")
  scoringinput[15,1]<-CONSTRAINT
  scAN<-paste(ANALYSIS,"type=complex;",sep="")
  scoringinput[16,1]<-ifelse(is.null(mytime),ANALYSIS,scAN)
  scoringinput[17,1]<-ifelse(length(keepvarimpact)>0,varMODEL,fixvarMODEL)
  
  
  l<-length(myindicators)
  scoreloadings<-list()
  for (i in 1:l){
    scoreloadings[i]<-paste(ETA,myindicators[i],sep="")
    scoreloadings[i]<-ifelse(lambda$pval[i] !=999,paste(" @",lambda$est[i],semicolon,sep=""),paste("*(l",i,")",semicolon,sep=""))
    scoreloadings[i]<-paste(ETA,myindicators[i],scoreloadings[i],sep="")
  }
  
  for (i in 1:l){
    scoringinput[17+i,1]<-scoreloadings[i]
  }
  for (i in 1:length(ETAON3)){
    scoringinput[17+l+i,1]<-paste(ETAON3[i],ETAPREDLIST$est[i],semicolon,sep="")
  }
  
  
  intdif<-round3output[grep(".ON",round3output$paramHeader),]
  intcode<-character(0)
  if (nrow(intdif)>0)
    {
    intdif$item<-read.table(text = intdif$paramHeader, sep = ".", as.is = TRUE)$V1
    intdif<-intdif[which(intdif$item!="ETA"),]
    keepcols<-c("param","est","item")
    intdif<-intdif[,keepcols]
    intcode<-paste(intdif$item," ON ",intdif$param,"@",intdif$est,semicolon,sep="")
    
    for (i in 1:length(intcode)){
      scoringinput[17+l+length(ETAON3)+i,1]<-intcode[i]
    }
  }
  
    
  
  thresh<-round3output[which(round3output$paramHeader=="Thresholds"|round3output$paramHeader=="Intercepts"&round3output$param!="ETA"),]
  for (i in 1:length(myindicators)){
    scoringinput[17+l+length(ETAON3)+length(intcode)+i,1]<-paste("[",thresh$param[i],"@",thresh$est[i],"];",sep="")
  }
  MODELCON<-paste("MODEL CONSTRAINT:")
  scoringinput[18+l+length(ETAON3)+length(intcode)+length(myindicators),1]<-ifelse(length(con)>0,MODELCON,"!")
  varval<-round3output[which(round3output$paramHeader=="New.Additional.Parameters"&substr(round3output$param,1,1)=="V"),]
  vstart<-data.frame(NULL)
  for (v in 1:length(keepvarimpact)){
    vstart[v,1]<-capture.output(cat(paste(varval$est[v],"*",keepvarimpact[v],"+",sep="")))
  }
  vstart<-paste(capture.output(cat(noquote(unlist(vstart)))),sep="")
  
  scoringinput[19+2*l+length(ETAON3)+length(intcode),1]<-ifelse(length(keepvarimpact)>0,paste("veta=1*exp("),"!")
  scoringinput[20+2*l+length(ETAON3)+length(intcode),1]<-ifelse(length(keepvarimpact)>0,vstart ,"!")
  scoringinput[21+2*l+length(ETAON3)+length(intcode),1]<-ifelse(length(keepvarimpact)>0,paste("0);"),"!")
  
  #lambda constraints
  lval<-round3output[which(round3output$paramHeader=="New.Additional.Parameters"&substr(round3output$param,1,1)=="L"),]
  #reorg so that all params for same item on same row
  lval$item<-substr(lval$param,2,3)
  lval$predictor<-as.numeric(substr(lval$param,3,4))
  lval$eq<-numeric(0)
  
  if ((dim(lval)[1])>0){
  for (i in 1:dim(lval)[1]){
    if (lval$predictor[i]==0) lval$eq[i]<-paste("l",lval$item[i],"=",lval$est[i],sep="")
    for (j in 1:length(myMeasInvar)){
      if (lval$predictor[i]==j) lval$eq[i]<-paste("+",lval$est[i],"*",myMeasInvar[j],sep="")
    }}}
  keep<-c("item","predictor","eq")
  lval<-lval[keep]
  wide<-reshape(lval, idvar = "item", timevar = "predictor", direction = "wide")
  wide<-wide[,-1]
  for (i in 1:dim(wide)[1]){
    line<-wide[i,]
    line<-line[!is.na(line)]
    line<-capture.output(cat(line))
    line<-append(line,semicolon)
    line<-capture.output(cat(line))
    scoringinput[22+2*l+length(ETAON3)+length(intcode)+i,1]<-ifelse(dim(lval)[1]>0,line,"!")
  }
  scoringinput[23+2*l+length(ETAON3)+length(intcode)+length(dim(wide)[1]),1]<-tech1
  scoringinput[24+2*l+length(ETAON3)+length(intcode)+length(dim(wide)[1]),1]<-paste("SAVEDATA: SAVE=FSCORES; FILE=scores.dat;")
  
  #write.table(scoringinput,paste(dir,"/scoring.inp",sep=""),append=F,row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.inp.file(scoringinput,paste(dir,"/scoring.inp",sep=""))
  message("Check '", dir, "/' for Mplus inp file for scoring model (run this manually).")
}