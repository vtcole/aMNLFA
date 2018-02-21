#' aMNLFA score plotting function
#'
#' This function creates plots of scores generated using aMNLFA. Can only be run after the aMNLFA.scores function.
#' @param input.object The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.
#' @keywords MNLFA
#' @export
#' @examples
#' aMNLFA.scoreplots()


aMNLFA.scoreplots<-function(input.object){
  
  path = input.object$path
  mrdata = input.object$mrdata
  myindicators = input.object$indicators
  mytime = input.object$time
  myfactors = input.object$factors
  myauxiliary = input.object$auxiliary
  myID = input.object$ID
  
  #####Read in scores and merge with data
  MplusOutput<-paste(path,"/scoring.out",sep="")
  modelResults <- readModels(MplusOutput)
  varorder<-modelResults$savedata_info$fileVarNames
  factorscores<-read.table(paste(path,"/scores.dat",sep=""),head=FALSE)
  colnames(factorscores)<-varorder
  keep<-c(myauxiliary,"ETA")
  factorscores<-factorscores[keep]
  data_plus_scores<-merge(factorscores,mrdata,by=myauxiliary)
  sc<-data_plus_scores
  if (mytime !="NA") sc$time<-unlist(data_plus_scores[mytime])
  if (mytime !="NA") sc$time<-as.numeric(sc$time)
  if (mytime !="NA") sc$time<-round(sc$time,.1)
  if (mytime !="NA") sc$time<-as.factor(sc$time)
  if (mytime !="NA") p<-ggplot(sc, aes(factor(time),ETA))+ geom_boxplot()+labs(x=paste(mytime))+ ggtitle("Factor Score Estimates over Time")
  if (mytime !="NA") filename<-paste(path,"/eta_by_time.png",sep="")
  if (mytime !="NA") png(filename=filename,
                         units="in",
                         width=11,
                         height=8.5,
                         pointsize=12,
                         res=72)
  if (mytime !="NA") p
  if (mytime !="NA") dev.off()
  #Visualize indicators as a function of time and moderators
  #re-structure data to allow facet_wrap visualization
  if (mytime !="NA") etalong<-melt(data_plus_scores,id.vars=c(myauxiliary,mytime,myfactors,"ETA"),measure.vars="ETA")
  if (mytime =="NA") etalong<-melt(data_plus_scores,id.vars=c(myauxiliary,myfactors,"ETA"),measure.vars="ETA")
  attach(etalong)
  etalong$AvgEtaScore<-as.character(etalong$value)
  etalong$AvgEtaScore<-as.numeric(etalong$AvgEtaScore)
  if (mytime !="NA") etalong$time<-unlist(etalong[mytime])
  if (mytime !="NA") etalong$time<-as.numeric(etalong$time)
  if (mytime !="NA") etalong$time<-round(etalong$time,.1)
  if (mytime !="NA") etalong$time<-as.factor(etalong$time)
  mrdata<-mrdata[order(mrdata[myID]),]
  srdatacheck<-mrdata[!duplicated(mrdata[myID]),]
  N<-dim(srdatacheck)[1]
  min<-.01*N
  if (mytime != "NA") aggetalong<-aggregate(AvgEtaScore~variable+time,etalong,FUN="mean")
  if (mytime != "NA") aggetalong2<-aggregate(AvgEtaScore~variable+time,etalong,FUN="length")
  if (mytime != "NA") aggetalong$N<-aggetalong2$AvgEtaScore
  if (mytime != "NA") margeta<-ggplot(aggetalong,aes(x=time,y=AvgEtaScore))+ facet_wrap(~ variable,nrow=1) +geom_point(aes(size=N))+stat_smooth(se=FALSE)+theme_bw()+labs(title="Average Factor Score Estimate over Time")+ theme(legend.position="bottom")
  #plot for each moderator
  l<-length(myfactors)
  if (mytime != "NA") p=list()
  if (mytime != "NA") p[[1]]<-margeta
  if (mytime != "NA")
    for (i in 1:l){
      keep<-c("AvgEtaScore","time",myfactors[i])
      etalongmod<-etalong[keep]
      names(etalongmod)[3]<-"Moderator"
      ic<-etalongmod$Moderator=="."|is.na(etalongmod$Moderator)|etalongmod$Moderator=="NA"
      cc_long<-etalongmod[which(ic=="FALSE"),]
      aggetalongmod<-aggregate(AvgEtaScore~time+Moderator,cc_long,FUN="mean")
      aggetalongmod<-aggregate(AvgEtaScore~time+Moderator,etalongmod,FUN="mean")
      aggetalongmod_2<-aggregate(AvgEtaScore~time+Moderator,etalongmod,FUN="length")
      aggetalongmod$N<-aggetalongmod_2$AvgEtaScore
      aggetalongmod<-aggetalongmod[which(aggetalongmod$N>min),]
      title<-paste("Average Factor ScoreEstimate over Time by ",myfactors[i],sep="")
      p[[i+1]]<-ggplot(aggetalongmod,aes(x=time,y=AvgEtaScore)) +geom_point(aes(size=N,colour=Moderator))+stat_smooth(se=FALSE,aes(colour=Moderator))+theme_bw()+labs(title=title)+ theme(legend.position="bottom") + guides(size=FALSE)
    }
  if (mytime != "NA") filename<-paste(path,"/factorscoreplots.png",sep="")
  if (mytime != "NA") png(filename=filename,
                          units="in",
                          width=11,
                          height=8.5,
                          pointsize=12,
                          res=72)
  if (mytime != "NA") if (length(myfactors)>1) do.call(grid.arrange,p)
  if (mytime != "NA") if (length(myfactors)==1) p
  if (mytime != "NA") dev.off()
  if (mytime != "NA") message("Check '", path, "/' for png file with factor score plots")
  
  if (mytime == "NA")
    l<-length(myfactors)
  p<-list()
  for (i in 1:l){
    keep<-c("AvgEtaScore",myfactors[i])
    etalongmod<-etalong[keep]
    names(etalongmod)[2]<-"Moderator"
    ic<-etalongmod$Moderator=="."|is.na(etalongmod$Moderator)|etalongmod$Moderator=="NA"
    cc_long<-etalongmod[which(ic=="FALSE"),]
    title<-paste("Average Factor ScoreEstimate by ",myfactors[i],sep="")
    p[[i+1]]<-ggplot(cc_long,aes(factor(Moderator),AvgEtaScore)) +geom_boxplot()+theme_bw()+labs(title=title)+ theme(legend.position="bottom")
  }
  if (mytime == "NA") filename<-paste(path,"/factorscoreplots.png",sep="")
  if (mytime == "NA") png(filename=filename,
                          units="in",
                          width=11,
                          height=8.5,
                          pointsize=12,
                          res=72)
  if (mytime == "NA") if (length(myfactors)>1) do.call(grid.arrange,p)
  if (mytime == "NA") if (length(myfactors)==1) p
  if (mytime == "NA") dev.off()
  if (mytime == "NA") message("Check '", path, "/' for png file with factor score plots")
  
  ##Empirical ICCs
  itemlong<-melt(data_plus_scores,id.vars=c(myfactors,myindicators,"ETA"),measure.vars=myindicators)
  itemlong$value<-as.character(itemlong$value)
  itemlong$value<-as.numeric(itemlong$value)
  attach(itemlong)
  itemlong$roundETA<-ifelse(itemlong$ETA < -2.75,-3,itemlong$ETA)
  itemlong$roundETA<-ifelse(itemlong$ETA > -2.75&itemlong$ETA< -2.25,-2.5,itemlong$roundETA)
  itemlong$roundETA<-ifelse(itemlong$ETA > -2.25&itemlong$ETA< -1.75,-2,itemlong$roundETA)
  itemlong$roundETA<-ifelse(itemlong$ETA > -1.75&itemlong$ETA< -1.25,-1.5,itemlong$roundETA)
  itemlong$roundETA<-ifelse(itemlong$ETA > -1.25&itemlong$ETA< -.75,-1,itemlong$roundETA)
  itemlong$roundETA<-ifelse(itemlong$ETA > -.75&itemlong$ETA< -.25,-.5,itemlong$roundETA)
  itemlong$roundETA<-ifelse(itemlong$ETA > -.25&itemlong$ETA< .25,0,itemlong$roundETA)
  itemlong$roundETA<-ifelse(itemlong$ETA > .25&itemlong$ETA< .75,.5,itemlong$roundETA)
  itemlong$roundETA<-ifelse(itemlong$ETA > .75&itemlong$ETA< 1.25,1,itemlong$roundETA)
  itemlong$roundETA<-ifelse(itemlong$ETA > 1.25&itemlong$ETA< 1.75,1.5,itemlong$roundETA)
  itemlong$roundETA<-ifelse(itemlong$ETA > 1.75&itemlong$ETA< 2.25,2,itemlong$roundETA)
  itemlong$roundETA<-ifelse(itemlong$ETA > 2.25&itemlong$ETA< 2.75,2.5,itemlong$roundETA)
  itemlong$roundETA<-ifelse(itemlong$ETA > 2.75,3,itemlong$roundETA)
  aggitemlong<-aggregate(value~variable+roundETA,data=itemlong,FUN="mean")
  aggitemlong$eta<-aggitemlong$roundETA
  aggitemlong$item_response<-aggitemlong$value
  ICC<-ggplot(aggitemlong,aes(x=eta,y=item_response))+ facet_wrap(~ variable,nrow=1) +stat_smooth(method='lm',formula=y~exp(x)/(1+exp(x)),se=FALSE)+theme_bw()+labs(title="Empirical Item Characteristic Curves")+ theme(legend.position="bottom")
  filename<-paste(path,"/ICCplots.png",sep="")
  png(filename=filename,
      units="in",
      width=11,
      height=8.5,
      pointsize=12,
      res=72)
  ICC
  dev.off()
  message("Check '", path, "/' for png file with empirical ICC plots")
  
  write.table(data_plus_scores, paste(path,"/mr_with_scores.dat",sep=""), sep="\t",col.names=TRUE,row.names=FALSE)
  
  message("Check '", path, "/' for merged data file")
}