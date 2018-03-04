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
  keep<-c(myID,"ETA")
  factorscores<-factorscores[keep]
  data_plus_scores<-merge(factorscores,mrdata,by=myID)
  sc<-data_plus_scores
  if (is.null(mytime) == 0) sc$time<-unlist(data_plus_scores[mytime])
  if (is.null(mytime) == 0) sc$time<-as.numeric(sc$time)
  if (is.null(mytime) == 0) sc$time<-round(sc$time,.1)
  if (is.null(mytime) == 0) sc$time<-as.factor(sc$time)
  if (is.null(mytime) == 0) p<-ggplot(sc, aes(factor(time),ETA))+ geom_boxplot()+labs(x=paste(mytime))+ ggtitle("Factor Score Estimates over Time")
  if (is.null(mytime) == 0) filename<-paste(path,"/eta_by_time.png",sep="")
  if (is.null(mytime) == 0) png(filename=filename,
                         units="in",
                         width=11,
                         height=8.5,
                         pointsize=12,
                         res=72)
  if (is.null(mytime) == 0) p
  if (is.null(mytime) == 0) dev.off()
  #Visualize indicators as a function of time and moderators
  #re-structure data to allow facet_wrap visualization
  if (is.null(mytime) == 0) etalong<-melt(data_plus_scores,id.vars=c(myID,mytime,myfactors,"ETA"),measure.vars="ETA")
  if (is.null(mytime) == 1) etalong<-melt(data_plus_scores,id.vars=c(myID,myfactors,"ETA"),measure.vars="ETA")
  attach(etalong)
  etalong$AvgEtaScore<-as.character(etalong$value)
  etalong$AvgEtaScore<-as.numeric(etalong$AvgEtaScore)
  if (is.null(mytime) == 0) etalong$time<-unlist(etalong[mytime])
  if (is.null(mytime) == 0) etalong$time<-as.numeric(etalong$time)
  if (is.null(mytime) == 0) etalong$time<-round(etalong$time,.1)
  if (is.null(mytime) == 0) etalong$time<-as.factor(etalong$time)
  mrdata<-mrdata[order(mrdata[myID]),]
  srdatacheck<-mrdata[!duplicated(mrdata[myID]),]
  N<-dim(srdatacheck)[1]
  min<-.01*N
  if (is.null(mytime) == 0) aggetalong<-aggregate(AvgEtaScore~variable+time,etalong,FUN="mean")
  if (is.null(mytime) == 0) aggetalong2<-aggregate(AvgEtaScore~variable+time,etalong,FUN="length")
  if (is.null(mytime) == 0) aggetalong$N<-aggetalong2$AvgEtaScore
  if (is.null(mytime) == 0) margeta<-ggplot(aggetalong,aes(x=time,y=AvgEtaScore))+ facet_wrap(~ variable,nrow=1) +geom_point(aes(size=N))+stat_smooth(se=FALSE)+theme_bw()+labs(title="Average Factor Score Estimate over Time")+ theme(legend.position="bottom")
  #plot for each moderator
  l<-length(myfactors)
  if (is.null(mytime) == 0) p=list()
  if (is.null(mytime) == 0) p[[1]]<-margeta
  if (is.null(mytime) == 0)
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
  if (is.null(mytime) == 0) filename<-paste(path,"/factorscoreplots.png",sep="")
  if (is.null(mytime) == 0) png(filename=filename,
                          units="in",
                          width=11,
                          height=8.5,
                          pointsize=12,
                          res=72)
  if (is.null(mytime) == 0) if (length(myfactors)>1) do.call(grid.arrange,p)
  if (is.null(mytime) == 0) if (length(myfactors)==1) p
  if (is.null(mytime) == 0) dev.off()
  if (is.null(mytime) == 0) message("Check '", path, "/' for png file with factor score plots")
  
  if (is.null(mytime) == 1)
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
  if (is.null(mytime) == 1) filename<-paste(path,"/factorscoreplots.png",sep="")
  if (is.null(mytime) == 1) png(filename=filename,
                          units="in",
                          width=11,
                          height=8.5,
                          pointsize=12,
                          res=72)
  if (is.null(mytime) == 1) if (length(myfactors)>1) do.call(grid.arrange,p)
  if (is.null(mytime) == 1) if (length(myfactors)==1) p
  if (is.null(mytime) == 1) dev.off()
  if (is.null(mytime) == 1) message("Check '", path, "/' for png file with factor score plots")
  
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