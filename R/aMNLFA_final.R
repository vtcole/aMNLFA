#' aMNLFA final model fitting function
#'
#' This function generates the final aMNLFA model on which scores will be based. Can only be run after a model containing all impact and DIF effects -- i.e., after the aMNLFA.simultaneous function.
#' @param input.object The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.
#' @keywords MNLFA
#' @export
#' @examples
#' \dontrun{
#'  wd <- "./aMNLFA/data"
#   First create aMNLFA object.
#   ob <- aMNLFA::aMNLFA.object(path          = wd,
#                            mrdata        = xstudy,
#                            indicators    = paste0("BIN_", 1:12), 
#                            catindicators = paste0("BIN_", 1:12), 
#                            meanimpact    = c("AGE", "GENDER", "STUDY"), 
#                            varimpact     = c("AGE", "GENDER", "STUDY"), 
#                            measinvar     = c("AGE", "GENDER", "STUDY"), 
#                            factors       = c("GENDER", "STUDY"), 
#                            ID            = "ID", 
#                            thresholds    = FALSE)
#'  aMNLFA.final(ob)
#' }

aMNLFA.final<-function(input.object){
  
  path = input.object$path
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
  
  varlist<-c(myID,myauxiliary,myindicators,myMeasInvar,myMeanImpact,myVarImpact)
  varlist<-unique(varlist)
  
  header<-readLines(paste0(path,"/header.txt"))
  
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
    loadings[i]<-paste(ETA,myindicators[i],"*(l_",i,");",sep="")
  }
  loadings<-noquote(loadings)
  loadings<-unlist(loadings)
  tech1<-paste("OUTPUT: tech1;")
  MODEL<-paste("MODEL: [ETA@0]; ETA@1;")
  CONSTRAINT<-paste("CONSTRAINT=")
  varMODEL<-paste("MODEL: [ETA@0];ETA*(veta);")
  MODELCONSTRAINT<-paste("MODEL CONSTRAINT: new(")
  #################################################################################################
  #################Use p<.1 criterion for impact###################################################
  #################Meas Invar: Lambdas: Use FDR with 5% error rate, # items * # predictors#########
  ########Meas Invar: Lambdas: Use FDR with 5% error rate, # items w/0 lambda * # predictors ######
  #################################################################################################
  
  ##Read in output and test for sig effects at p<.01
  round2output<-MplusAutomation::readModels(paste(path,"/round2calibration.out",sep=""))
  round2output<-as.data.frame(round2output$parameters$unstandardized)
  
  meanimpact<-round2output[which(round2output$paramHeader=="ETA.ON"),]
  meanimpact<-meanimpact[which(meanimpact$pval<.1),]
  keepmeanimpact<-meanimpact$param
  for (j in 1:length(keepmeanimpact)){
    if(length(grep("_",keepmeanimpact[j]))>0) keepmeanimpact<-append(keepmeanimpact,substr(keepmeanimpact[j],1,3))
    if(length(grep("_",keepmeanimpact[j]))>0) keepmeanimpact<-append(keepmeanimpact,substr(keepmeanimpact[j],5,7))
  }
  keepmeanimpact<-keepmeanimpact[which(keepmeanimpact!=2)]
  keepmeanimpact<-unique(keepmeanimpact)
  threeletterlist<-substr(myMeanImpact,1,3)
  test<-(keepmeanimpact[match(threeletterlist,keepmeanimpact)])
  pos<-match(threeletterlist,test)
  pos<-pos[!is.na(pos)]
  pos<-unique(pos)
  add<-myMeanImpact[pos]
  keepmeanimpact<-unique(append(keepmeanimpact,add))
  keepmeanimpact2<-match(keepmeanimpact,myMeanImpact)
  keepmeanimpact2<-keepmeanimpact2[!is.na(keepmeanimpact2)]
  keepmeanimpact<-myMeanImpact[keepmeanimpact2]
  
  ##Read in var impact script and test for impact at p<.1
  
  varimpact<-round2output[which(round2output$paramHeader=="New.Additional.Parameters"&round2output$pval<.1),]  ######alpha <.1 to trim###########
  varimpact<-noquote(substr(varimpact$param,2,3))
  myVarImpact2<-as.data.frame(myVarImpact)
  myVarImpact3<-as.data.frame(NULL)
  for (i in 1:length(varimpact)){
    myVarImpact3[i,1]<-myVarImpact2[varimpact[i],]
  }
  keepvarimpact<-noquote(t(myVarImpact3))
  
  for (j in 1:length(keepvarimpact)){
    if(length(grep("_",keepvarimpact[j]))>0) keepvarimpact<-append(keepvarimpact,substr(keepvarimpact[j],1,3))
    if(length(grep("_",keepvarimpact[j]))>0) keepvarimpact<-append(keepvarimpact,substr(keepvarimpact[j],5,7))
  }
  keepvarimpact<-keepvarimpact[which(keepvarimpact!=2)]
  keepvarimpact<-unique(keepvarimpact)
  threeletterlist<-substr(myVarImpact,1,3)
  test<-(keepvarimpact[match(threeletterlist,keepvarimpact)])
  pos<-match(threeletterlist,test)
  pos<-pos[!is.na(pos)]
  pos<-unique(pos)
  add<-myVarImpact[pos]
  keepvarimpact<-unique(append(keepvarimpact,add))
  keepvarimpact2<-match(keepvarimpact,myVarImpact)
  keepvarimpact2<-keepvarimpact2[!is.na(keepvarimpact2)]
  keepvarimpact<-myVarImpact[keepvarimpact2]
  
  lambdaconstraints<-round2output[which(round2output$paramHeader=="New.Additional.Parameters"&substr(round2output$param,1,1)=="L"),]
  #The following line obtains the last 2 characters of each parameter; it uses str_sub rather than stringr to handle names of different length
  lambdaconstraints<-lambdaconstraints[which(stringr::str_sub(lambdaconstraints$param,start=-2)!="00"),]
  lambdaconstraints<-lambdaconstraints[order(lambdaconstraints$pval),]
  ###FDR correction: BH-crit=(m-rank+1)*.05/(2*m); m=# indicators*# predictors; rank = highest-to-lowest p-value amongst in m
  m<-length(myindicators)*length(myMeasInvar)
  
  # ----------------------------- seem to be having trouble here
  if (nrow(lambdaconstraints)==0) {lambdaconstraints[1,] <- NA}
  lambdaconstraints$sig<-NA
  # ---------------------------------

  for (l in 1:dim(lambdaconstraints)[1]){
    lambdaconstraints$rank[l]<-m+1-l
  }
  lambdaconstraints$BHcrit<-.05*(m-lambdaconstraints$rank+1)/(2*m)
  lambdaconstraints$sig<-ifelse(lambdaconstraints$pval<lambdaconstraints$BHcrit,"TRUE","FALSE")
  lambdaconstraints<-lambdaconstraints[which(lambdaconstraints$sig=="TRUE"),]
  lambdaconstraints<-noquote(substr(lambdaconstraints$param,2,3))
  alllambdadf<-data.frame(matrix(nrow=length(myindicators),ncol=length(myMeasInvar)))
  #Need to figure out what to do if there are more than ten covariates
  item_i<-stringr::str_sub(lambdaconstraints,0,1)
  col_i<-stringr::str_sub(lambdaconstraints,start=-1)
  if (length(lambdaconstraints)>0) {
    for (i in 1:length(lambdaconstraints)){
      for (r in 1:length(myindicators)){
        for (c in 1:length(myMeasInvar)){
          alllambdadf[r,c]<-ifelse(dim(lambdaconstraints)[1]==0,NA,ifelse(item_i[i]==r & col_i[i]==c,myMeasInvar[c],NA))
        }}}}
  
  colnames(alllambdadf)<-myMeasInvar
  rownames(alllambdadf)<-myindicators
  
  for (i in 1:length(myindicators)){
    keeplambda<-unlist(as.list(alllambdadf[i,1:length(myMeasInvar)]))
    keeplambda<-keeplambda[!is.na(keeplambda)]
    names(keeplambda)<-NULL
    for (j in 1:length(keeplambda)){
      if(length(grep("_",keeplambda[j]))>0) keeplambda<-append(keeplambda,substr(keeplambda[j],1,3))
      if(length(grep("_",keeplambda[j]))>0) keeplambda<-append(keeplambda,substr(keeplambda[j],5,7))
    }
    keeplambda<-unique(keeplambda)
    threeletterlist<-substr(myMeasInvar,1,3)
    test<-(keeplambda[match(threeletterlist,keeplambda)])
    pos<-match(threeletterlist,test)
    pos<-pos[!is.na(pos)]
    pos<-unique(pos)
    add<-myMeasInvar[pos]
    keeplambda<-unique(append(keeplambda,add))
    keeplambda2<-match(keeplambda,myMeasInvar)
    keeplambda2<-keeplambda2[!is.na(keeplambda2)]
    keeplambda<-myMeasInvar[keeplambda2]
    if (length(keeplambda)>0)
      for (j in 1:length(myMeasInvar)){
        for (k in 1:length(keeplambda)){
          if(length(grep(keeplambda[k],myMeasInvar[j]))>0)  alllambdadf[i,j]<-myMeasInvar[j]
        }
      }
  }
  
  
  interceptconstraints<-round2output[grep(".ON",round2output$paramHeader),]
  interceptconstraints<-interceptconstraints[which(interceptconstraints$paramHeader!="ETA.ON"),]
  
  interceptconstraints$item_k<-read.table(text = interceptconstraints$paramHeader, sep = ".", as.is = TRUE)$V1
  interceptconstraints$itemnum<-match(interceptconstraints$item_k, myindicators)
  interceptconstraints$prenum<-match(interceptconstraints$param,myMeasInvar)
  
  interceptconstraints$siglambda<-"FALSE"
  if (length(lambdaconstraints)>0)
    for (i in 1:length(lambdaconstraints)){
      interceptconstraints$siglambda<-ifelse(substr(lambdaconstraints[i],1,1)==interceptconstraints$itemnum&substr(lambdaconstraints[i],2,2)==interceptconstraints$prenum,"TRUE","FALSE")
    }
  
  int_totest<-interceptconstraints[which(interceptconstraints$siglambda=="FALSE"),]
  int_totest<-int_totest[order(int_totest$pval),]
  mitem<-length(unique(unlist(int_totest$item_k)))
  m<-mitem*length(myMeasInvar)
  int_totest$sig<-NA
  for (l in 1:dim(int_totest)[1]){
    int_totest$rank[l]<-m+1-l
  }
  int_totest$BHcrit<-.05*(m-int_totest$rank+1)/(2*m)
  int_totest$sig<-ifelse(int_totest$pval<int_totest$BHcrit,"TRUE","FALSE")
  int_totest<-int_totest[which(int_totest$sig=="TRUE"),]
  int<-interceptconstraints[which(interceptconstraints$siglambda=="TRUE"),]
  int<-plyr::rbind.fill(int_totest,int)
  allintdf<-data.frame(matrix(nrow=length(myindicators),ncol=length(myMeasInvar)))
  colnames(allintdf)<-myMeasInvar
  rownames(allintdf)<-myindicators
  intnumbers<-paste(int$itemnum,int$prenum,sep="")
  item_i<-int$itemnum
  col_i<-int$prenum
  if (length(intnumbers)>0){
  for (i in 1:length(intnumbers)){
    for (r in 1:length(myindicators)){
      for (c in 1:length(myMeasInvar)){
        if(item_i[i]==r & col_i[i]==c) allintdf[r,c]<-myMeasInvar[c]
      }}}}
  
  
  for (i in 1:length(myindicators)){
    keepintercept<-unlist(as.list(allintdf[i,1:length(myMeasInvar)]))
    keepintercept<-keepintercept[!is.na(keepintercept)]
    names(keepintercept)<-NULL
    for (j in 1:length(keepintercept)){
      if(length(grep("_",keepintercept[j]))>0) keepintercept<-append(keepintercept,substr(keepintercept[j],1,3))
      if(length(grep("_",keepintercept[j]))>0) keepintercept<-append(keepintercept,substr(keepintercept[j],5,7))
    }
    keepintercept<-unique(keepintercept)
    threeletterlist<-substr(myMeasInvar,1,3)
    test<-(keepintercept[match(threeletterlist,keepintercept)])
    pos<-match(threeletterlist,test)
    pos<-pos[!is.na(pos)]
    pos<-unique(pos)
    add<-myMeasInvar[pos]
    keepintercept<-unique(append(keepintercept,add))
    keepintercept2<-match(keepintercept,myMeasInvar)
    keepintercept2<-keepintercept2[!is.na(keepintercept2)]
    keepintercept<-myMeasInvar[keepintercept2]
    if (length(keepintercept)>0)
      for (j in 1:length(myMeasInvar)){
        for (k in 1:length(keepintercept)){
          if(length(grep(keepintercept[k],myMeasInvar[j]))>0)  allintdf[i,j]<-myMeasInvar[j]
        }
      }
  }
  
  
  colnames(allintdf)<-NULL
  allmeasdif<-cbind(alllambdadf,allintdf)
  diflist<-allmeasdif[1,]
  for (i in 2:length(myindicators)){
    diflist<-append(diflist,allmeasdif[i,])
  }
  diflist<-noquote(unique(unlist(diflist)))
  diflist<-diflist[!is.na(diflist)]
  
  
  keepmeanimpact<-unique(append(keepmeanimpact,keepvarimpact))
  useround3<-c(diflist,keepmeanimpact,keepvarimpact)
  useround3<-append(useround3,myindicators)
  useround3<-unlist(useround3)
  useround3<-unique(useround3)
  useround3<-useround3[!is.na(useround3)]
  useround3<-capture.output(cat(useround3))
  ETAON2<-paste("ETA ON ",keepmeanimpact,semicolon,sep="")
  ETAON3<-paste("ETA ON ",keepmeanimpact,"@",sep="")
  
  uniquelambda<-unique(unlist(alllambdadf))
  uniquelambda<-uniquelambda[!is.na(uniquelambda)]
  con<-unique(append(keepvarimpact,uniquelambda))
  CONSTRAINT<-noquote(append(CONSTRAINT,con))
  CONSTRAINT<-append(CONSTRAINT,semicolon)
  CONSTRAINT<-capture.output(cat(CONSTRAINT))
  CONSTRAINT<-ifelse(length(con)>0,CONSTRAINT,"!")
  
  round3input<-as.data.frame(NULL)
  
  round3input[1,1]<-paste("TITLE: Round 3 Final Calibration Model")
  round3input[2,1]<-header[2]
  round3input[3,1]<-header[3]
  round3input[4,1]<-header[4]
  round3input[5,1]<-header[5]
  round3input[6,1]<-ifelse(length(header)>5,header[6],"!")
  round3input[7,1]<-ifelse(length(header)>6,header[7],"!")
  round3input[8,1]<-ifelse(length(header)>7,header[8],"!")
  round3input[9,1]<-ifelse(length(header)>8,header[9],"!")
  round3input[10,1]<-paste("USEVARIABLES= ",useround3,semicolon,sep="")
  round3input[11,1]<-ifelse(length(myauxiliary)>0,AUXILIARY,"!")
  round3input[12,1]<-ifelse(length(mycatindicators)>0,CATEGORICAL,"!")
  round3input[13,1]<-ifelse(length(mycountindicators)>0,COUNT,"!")
  round3input[14,1]<-CONSTRAINT
  round3input[15,1]<-ANALYSIS
  #fixvarMODEL<-paste("MODEL: [ETA@0];ETA@1;") #commenting this out so that constraint statement gets made, even if there is no variance impact
  #round3input[16,1]<-ifelse(length(con)>0,varMODEL,fixvarMODEL)
  round3input[16,1]<-varMODEL
  l<-length(loadings)
  for (i in 1:l){
    round3input[16+i,1]<-loadings[i]
  }
  round3input[17+l,1]<-ifelse(length(keepmeanimpact)>0,capture.output(cat(ETAON2 )),"!")
  
  for (i in 1:l){
    predlist<-unlist(allintdf[i,])
    predlist<-predlist[!is.na(predlist)]
    predlist<-predlist[predlist!="NA"]
    predlist<-capture.output(cat(predlist))
    round3input[17+l+i,1]<-ifelse(length(predlist)>0,paste(myindicators[i]," on ",predlist,";",sep=""),"!")
  }
  vstart<-data.frame(NULL)
  if (length(keepvarimpact)>0)
    for (v in 1:length(keepvarimpact)){
      vstart[v,1]<-paste("v",v,"*0",sep="")
    }
  
  vstart<-ifelse(length(keepvarimpact)>0,paste(capture.output(cat(noquote(unlist(vstart)))),sep=""),"")
  round3input[18+2*l,1]<-capture.output(cat(append(MODELCONSTRAINT,vstart)) )
  m<-length(myMeasInvar)
  ind<-length(myindicators)
  for (i in 1:ind){
    predlist2<-unlist(alllambdadf[i,1:m])
    predlist2<-predlist2[!is.na(predlist2)]
    eq<-as.data.frame(NULL)
    start<-as.data.frame(NULL)
    eq[1,1]<-ifelse(length(predlist2)>0,paste("l_",i,"=l",i,"00",sep=""),"!")
    start[1,1]<-ifelse(length(predlist2)>0,paste("l",i,"00*1 ",sep=""),"!")
    if (length(predlist2)>0)
      for (w in 1:length(predlist2)){
        eq[1+w,1]<-ifelse(length(predlist2)>0,paste("+l",i,w,"*",predlist2[w],sep="") ,"!")
        start[1+w,1]<-ifelse(length(predlist2)>0,paste(" l",i,w,"*0",sep="") ,"!")
      }
    round3input[18+3*l+i,1]<-paste(capture.output(cat(noquote(unlist(eq)))),semicolon,sep="")
    round3input[18+2*l+i,1]<-paste(capture.output(cat(noquote(unlist(start)))),sep="")
  }
  round3input[18+2*l+ind,1]<-paste(");")
  
  veq<-as.data.frame(NULL)
  veq[1,1]<-"veta=1*exp("
  if (length(keepvarimpact)>0)
    for (v in 1:length(keepvarimpact)){
      veq[v+1,1]<-paste("v",v,"*",keepvarimpact[v],"+",sep="")
    }
  v<-length(keepvarimpact)
  veq[v+2,1]<-paste("0)")
  
  round3input[19+2*l+2*ind,1]<-paste(capture.output(cat(noquote(unlist(veq)))),semicolon,sep="")
  round3input[20+2*l+2*ind,1]<-tech1
  if(is.null(mytime)){round3input[21+2*l+2*ind,1]<-paste("SAVEDATA: SAVE=FSCORES; FILE=scores.dat;")}
  
  #write.table(round3input,paste(path,"/round3calibration.inp",sep=""),append=F,row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.inp.file(round3input,paste(path,"/round3calibration.inp",sep=""))
  message("Check '", path, "/' for Mplus inp file for round 2 calibration model (run this manually). \nNOTE: There is some Mplus output which is currently not able to be read in. \nIf your output contains the phrase 'BRANT WALD TEST FOR PROPORTIONAL ODDS,' please delete this section (including the heading itself) as well as everything after it. The last thing in your output should be 'LOGISTIC REGRESSION ODDS RATIO RESULTS.'")  
}
