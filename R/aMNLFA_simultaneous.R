#' aMNLFA simultaneous model fitting function
#'
#' This function generates the simultaneous aMNLFA model from all the initial inputs.
#' @param input.object The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.
#' @keywords MNLFA
#' @export
#' @examples
#' aMNLFA.simultaneous()

aMNLFA.simultaneous<-function(input.object){

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

  USEVARIABLES<-paste("USEVARIABLES=")
  semicolon<-paste(";")
  AUXILIARY<-ifelse(length(myauxiliary)>0,paste("AUXILIARY="),paste("!"))
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
  ####ROUND 1 USES p<.05 AS GATE TO GET TO ROUND 2 FOR MEAS INV and p<.1 for IMPACT####################

  ##Read in mean impact script and test for impact at p<.1
  meanimpact<-readModels(paste(path,"/meanimpactscript.out",sep=""))
  meanimpact<-as.data.frame(meanimpact$parameters$unstandardized)
  meanimpact<-meanimpact[which(meanimpact$paramHeader=="ETA.ON"),]
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
  varimpact<-readModels(paste(path,"/varimpactscript.out",sep=""))
  varimpact<-as.data.frame(varimpact$parameters$unstandardized)

  varimpact<-varimpact[which(varimpact$paramHeader=="New.Additional.Parameters"&varimpact$pval<.1),]  ######alpha <.1 to trim###########
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
  threeletterlist<- stringr::stringr::str_sub(myVarImpact,1,3)
  test<-(keepvarimpact[match(threeletterlist,keepvarimpact)])
  pos<-match(threeletterlist,test)
  pos<-pos[!is.na(pos)]
  pos<-unique(pos)
  add<-myVarImpact[pos]
  keepvarimpact<-unique(append(keepvarimpact,add))
  keepvarimpact2<-match(keepvarimpact,myVarImpact)
  keepvarimpact2<-keepvarimpact2[!is.na(keepvarimpact2)]
  keepvarimpact<-myVarImpact[keepvarimpact2]

  alllambdadf<-data.frame(matrix(c(NA),nrow=length(myindicators),ncol=1+length(myMeasInvar)))
  colnames(alllambdadf)=c("items",myMeasInvar)
  alllambdadf$items=myindicators
  for (i in 1:length(myindicators)){
    dif<-readModels(paste(path,"/measinvarscript_",myindicators[i],".out",sep=""))
    dif<-dif$parameters$unstandardized
    lambdadif<-dif[which(dif$paramHeader=="New.Additional.Parameters"),]
    lambdadif$param<-noquote(stringr::str_sub(lambdadif$param,-2,-1))
    lambdadif<-lambdadif[which(lambdadif$param!="00"),]
    lambdadif$param<-myMeasInvar[(lambdadif$param)]
    keep<-c("param","pval")
    lambdadif<-noquote(t(lambdadif[keep]))
    colnames(lambdadif)=lambdadif[1,]
    lambdadif=t(as.matrix(lambdadif[-1,]))
    lambda_min=apply(lambdadif,1,function(x) {as.numeric(min(x))}) #Get the minimum p. value associated with lambda or all of the thresholds. If any are under .05, we retain the covariate.
    lambda_index<-nrow(lambdadif) #Gets the number of covariates there are, regardless of whether we're using thresholds or not.
    alllambdadf[i,2:(lambda_index+1)]<-lambda_min
    for (j in 1:length(myMeasInvar)){
      alllambdadf[i,j+1]<-ifelse(alllambdadf[i,j+1]<.05,myMeasInvar[j],NA)
    }
    keeplambda<-unlist(as.list(alllambdadf[i,2:(lambda_index+1)]))
    keeplambda<-keeplambda[!is.na(keeplambda)]
    names(keeplambda)<-NULL
    for (j in 1:length(keeplambda)){
      if(length(grep("_",keeplambda[j]))>0) keeplambda<-append(keeplambda,stringr::str_sub(keeplambda[j],1,3))
      if(length(grep("_",keeplambda[j]))>0) keeplambda<-append(keeplambda,stringr::str_sub(keeplambda[j],5,7))
    }
    keeplambda<-unique(keeplambda)
    threeletterlist<-stringr::str_sub(myMeasInvar,1,3)
    test<-(keeplambda[match(threeletterlist,keeplambda)])
    pos<-match(threeletterlist,test)
    pos<-pos[!is.na(pos)]
    pos<-unique(pos)
    add<-myMeasInvar[pos]
    keeplambda<-unique(append(keeplambda,add))
    keeplambda2<-match(keeplambda,myMeasInvar)
    keeplambda2<-keeplambda2[!is.na(keeplambda2)]
    keeplambda<-myMeasInvar[keeplambda2]
    for (j in 2:(lambda_index+1)){
      if (length(keeplambda)>0){
      for (k in 1:length(keeplambda)){
        if(length(grep(keeplambda[k],myMeasInvar[j-1]))>0)  alllambdadf[i,j]<-myMeasInvar[j-1]
        }
      }
    }
  }

  colnames(alllambdadf)<-NULL
  
  allinterceptdf<-data.frame(matrix(c(NA),nrow=length(myindicators),ncol=1+length(myMeasInvar)))
  
  if (thresholds==FALSE) {
    colnames(allinterceptdf)=c("items",myMeasInvar)
    allinterceptdf$items=myindicators
    for (i in 1:length(myindicators)){
      dif<-readModels(paste(path,"/measinvarscript_",myindicators[i],".out",sep=""))
      dif<-dif$parameters$unstandardized
      keep<-c("param","pval")
      intdif<-dif[grep(".ON",dif$paramHeader),]
      intdif<-noquote(t(intdif[keep]))
      colnames(intdif)=intdif[1,]
      intdif=intdif[-1,]
      allinterceptdf[i,2:(length(intdif)+1)]<-intdif
      for (j in 1:length(myMeasInvar)){
        allinterceptdf[i,j+1]<-ifelse(allinterceptdf[i,j+1]<.05,myMeasInvar[j],NA)
      }
      keepintercept<-unlist(as.list(allinterceptdf[i,2:length(myMeasInvar)+1]))
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
        for (j in 2:length(myMeasInvar)+1){
          for (k in 1:length(keepintercept)){
            if(length(grep(keepintercept[k],myMeasInvar[j-1]))>0)  allinterceptdf[i,j]<-myMeasInvar[j-1]
          }
        }
      }
    }
  colnames(allinterceptdf)<-NULL
  allmeasdif<-cbind(alllambdadf,allinterceptdf)
  diflist<-allmeasdif[1,]
  for (i in 2:length(myindicators)){
    diflist<-append(diflist,allmeasdif[i,])
  }
  diflist<-noquote(unique(unlist(diflist)))
  diflist<-diflist[!is.na(diflist)]

  keepmeanimpact<-unique(append(keepmeanimpact,keepvarimpact))
  ##Writing script with all parameters with p<.05
  useround2<-c(diflist,keepmeanimpact,keepvarimpact)
  useround2<-noquote(unique(useround2))
  useround2<-capture.output(cat(useround2))
  ETAON2<-paste("ETA ON ",keepmeanimpact,semicolon,sep="")
  uniquelambda<-unique(unlist(alllambdadf[,-1]))
  uniquelambda<-uniquelambda[!is.na(uniquelambda)]
  con<-unique(append(keepvarimpact,uniquelambda))
  CONSTRAINT<-noquote(append(CONSTRAINT,con))
  CONSTRAINT<-append(CONSTRAINT,semicolon)
  CONSTRAINT<-capture.output(cat(CONSTRAINT))

  header<-readLines(paste0(path,"/header.txt"))

  round2input<-as.data.frame(NULL)
  round2input[1,1]<-paste("TITLE: Round 2 Calibration Model")
  round2input[2,1]<-header[2]
  round2input[3,1]<-header[3]
  round2input[4,1]<-header[4]
  round2input[5,1]<-header[5]
  round2input[6,1]<-ifelse(length(header)>5,header[6],"!")
  round2input[7,1]<-ifelse(length(header)>6,header[7],"!")
  round2input[8,1]<-ifelse(length(header)>7,header[8],"!")
  round2input[9,1]<-ifelse(length(header)>8,header[9],"!")
  round2input[10,1]<-paste("USEVARIABLES= ",useround2,semicolon,sep="")
  round2input[11,1]<-AUXILIARY
  round2input[12,1]<-ifelse(length(mycatindicators)>0,CATEGORICAL,"!")
  round2input[13,1]<-ifelse(length(mycountindicators)>0,COUNT,"!")
  round2input[14,1]<-CONSTRAINT
  round2input[15,1]<-ANALYSIS
  round2input[16,1]<-varMODEL
  l<-length(loadings)
  for (i in 1:l){
    round2input[16+i,1]<-loadings[i]
  }
  round2input[17+l,1]<-ifelse(length(keepmeanimpact)>0,capture.output(cat(ETAON2)),"!")
  for (i in 1:dim(allinterceptdf)[1]){
    for (j in 2:dim(allinterceptdf)[2]){
      allinterceptdf[i,j]<-ifelse(is.na(allinterceptdf[i,j]),alllambdadf[i,j],allinterceptdf[i,j])
    }
  }
  for (i in 1:l){
    predlist<-unlist(allinterceptdf[i,2:l])
    predlist<-predlist[!is.na(predlist)]
    predlist<-capture.output(cat(predlist))
    round2input[17+l+i,1]<-ifelse(length(predlist)>0,paste(myindicators[i]," on ",predlist,";",sep=""),"!")
  }

  vstart<-data.frame(NULL)
  if (length(keepvarimpact)>0)
    for (v in 1:length(keepvarimpact)){
      vstart[v,1]<-paste("v",v,"*0",sep="")
    }
  vstart<-ifelse(length(keepvarimpact)>0,paste(capture.output(cat(noquote(unlist(vstart)))),sep=""),"!")
  round2input[18+2*l,1]<-capture.output(cat(append(MODELCONSTRAINT,vstart)) )
  m<-length(myMeasInvar)
  ind<-length(myindicators)
  for (i in 1:ind){
    predlist2<-unlist(alllambdadf[i,2:m+1])
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
    round2input[19+3*l+i,1]<-paste(capture.output(cat(noquote(unlist(eq)))),semicolon,sep="")
    round2input[18+2*l+i,1]<-paste(capture.output(cat(noquote(unlist(start)))),sep="")
  }
  round2input[19+3*l,1]<-paste(");")

  veq<-as.data.frame(NULL)
  veq[1,1]<-"veta=1*exp("
  if (length(keepvarimpact)>0)
    for (v in 1:length(keepvarimpact)){
      veq[v+1,1]<-paste("v",v,"*",keepvarimpact[v],"+",sep="")
    }
  v<-length(keepvarimpact)
  veq[v+2,1]<-paste("0)")

  round2input[20+3*l+ind,1]<-paste(capture.output(cat(noquote(unlist(veq)))),semicolon,sep="")
  round2input[21+3*l+ind,1]<-tech1


  #write.table(round2input,paste(path,"/round2calibration.inp",sep=""),append=F,row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.inp.file(round2input,paste(path,"/round2calibration.inp",sep=""))
  message("Check '", path, "/' for Mplus inp file for round 2 calibration model (run this manually).")
}
