#' aMNLFA score plotting function
#'
#' This function creates plots of scores generated using aMNLFA. Can only be run after the aMNLFA.scores function.
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
#'  aMNLFA.scoreplots(ob)

aMNLFA.scoreplots<-function(input.object){
  
  dir = input.object$dir
  mrdata = input.object$mrdata
  myindicators = input.object$indicators
  mytime = input.object$time
  myfactors = input.object$factors
  myauxiliary = input.object$auxiliary
  myID = input.object$ID
  
  #####Read in scores and merge with data
  MplusOutput<-fixPath(file.path(dir,"scoring.out",sep=""))
  modelResults <- MplusAutomation::readModels(MplusOutput)
  varorder<-modelResults$savedata_info$fileVarNames
  factorscores<-utils::read.table(fixPath(file.path(dir,"scores.dat",sep="")),header=FALSE)
  colnames(factorscores)<-varorder
  keep<-c(myID,"ETA")
  factorscores<-factorscores[keep]
  data_plus_scores<-merge(factorscores,mrdata,by=myID)
  sc<-data_plus_scores
  if (is.null(mytime) == 0) sc$time<-unlist(data_plus_scores[mytime])
  if (is.null(mytime) == 0) sc$time<-as.numeric(sc$time)
  if (is.null(mytime) == 0) sc$time<-round(sc$time,.1)
  if (is.null(mytime) == 0) sc$time<-as.factor(sc$time)
  if (is.null(mytime) == 0) p<-with(sc,ggplot2::ggplot(sc, aes(factor(time),ETA))) + with(sc,ggplot2::geom_boxplot()) + with(sc,ggplot2::labs(x=paste(mytime))) + with(sc,ggplot2::ggtitle("Factor Score Estimates over Time"))
  if (is.null(mytime) == 0) filename<-fixPath(file.path(dir,"eta_by_time.png",sep=""))
  if (is.null(mytime) == 0) grDevices::png(filename=filename,
                         units="in",
                         width=11,
                         height=8.5,
                         pointsize=12,
                         res=72)
  if (is.null(mytime) == 0) p
  if (is.null(mytime) == 0) grDevices::dev.off()
  #Visualize indicators as a function of time and moderators
  #re-structure data to allow facet_wrap visualization
  if (is.null(mytime) == 0) etalong<-reshape2::melt(data_plus_scores,id.vars=c(myID,mytime,myfactors,"ETA"),measure.vars="ETA")
  if (is.null(mytime) == 1) etalong<-reshape2::melt(data_plus_scores,id.vars=c(myID,myfactors,"ETA"),measure.vars="ETA")
  
  #attach(etalong) #Should not use attach in here -- downstream references now resolved
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
  if (is.null(mytime) == 0) aggetalong<-stats::aggregate(AvgEtaScore~variable+time,etalong,FUN="mean")
  if (is.null(mytime) == 0) aggetalong2<-stats::aggregate(AvgEtaScore~variable+time,etalong,FUN="length")
  if (is.null(mytime) == 0) aggetalong$N<-aggetalong2$AvgEtaScore
  if (is.null(mytime) == 0) margeta<-with(aggetalong,ggplot2::ggplot(aggetalong,aes(x=time,y=AvgEtaScore))) + with(aggetalong,ggplot2::facet_wrap(~ variable,nrow=1)) + with(aggetalong,ggplot2::geom_point(aes(size=N))) + with(aggetalong,ggplot2::stat_smooth(se=FALSE)) + with(aggetalong,ggplot2::theme_bw()) + with(aggetalong,ggplot2::labs(title="Average Factor Score Estimate over Time")) + with(aggetalong,ggplot2::theme(legend.position="bottom"))
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
      aggetalongmod<-stats::aggregate(AvgEtaScore~time+Moderator,cc_long,FUN="mean")
      aggetalongmod<-stats::aggregate(AvgEtaScore~time+Moderator,etalongmod,FUN="mean")
      aggetalongmod_2<-stats::aggregate(AvgEtaScore~time+Moderator,etalongmod,FUN="length")
      aggetalongmod$N<-aggetalongmod_2$AvgEtaScore
      aggetalongmod<-aggetalongmod[which(aggetalongmod$N>min),]
      title<-paste("Average Factor ScoreEstimate over Time by ",myfactors[i],sep="")
      p[[i+1]]<-with(aggetalongmod,ggplot2::ggplot(aggetalongmod,aes(x=time,y=AvgEtaScore))) + with(aggetalongmod,ggplot2::geom_point(aes(size=N,colour=Moderator))) + with(aggetalongmod,ggplot2::stat_smooth(se=FALSE,aes(colour=Moderator))) + with(aggetalongmod,ggplot2::theme_bw()) + with(aggetalongmod,ggplot2::labs(title=title)) + with(aggetalongmod,ggplot2::theme(legend.position="bottom")) + with(aggetalongmod,ggplot2::guides(size=FALSE))
    }
  if (is.null(mytime) == 0) filename<-fixPath(file.path(dir,"factorscoreplots.png",sep=""))
  if (is.null(mytime) == 0) grDevices::png(filename=filename,
                          units="in",
                          width=11,
                          height=8.5,
                          pointsize=12,
                          res=72)
  if (is.null(mytime) == 0) if (length(myfactors)>1) {
    graphics::par(mfrow=c(1,length(myfactors)))
    p
    graphics::par(mfrow=c(1,1))
  }
    #do.call(gridExtra::grid.arrange,p)
  if (is.null(mytime) == 0) if (length(myfactors)==1) p
  if (is.null(mytime) == 0) grDevices::dev.off()
  if (is.null(mytime) == 0) message("Check '", dir, "/' for png file with factor score plots")
  
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
    p[[i+1]]<-with(cc_long,ggplot2::ggplot(cc_long,ggplot2::aes(x=factor(Moderator),y=AvgEtaScore))) + with(cc_long,ggplot2::geom_boxplot()) + with(cc_long,ggplot2::theme_bw()) + with(cc_long,ggplot2::labs(title=title)) + with(cc_long,ggplot2::theme(legend.position="bottom"))
  }
  if (is.null(mytime) == 1) filename<-fixPath(file.path(dir,"factorscoreplots.png",sep=""))
  if (is.null(mytime) == 1) grDevices::png(filename=filename,
                          units="in",
                          width=11,
                          height=8.5,
                          pointsize=12,
                          res=72)
  if (is.null(mytime) == 1) if (length(myfactors)>1) {
    graphics::par(mfrow=c(1,length(myfactors)))
    p
    graphics::par(mfrow=c(1,1))
  }
    #do.call(gridExtra::grid.arrange,p)
  if (is.null(mytime) == 1) if (length(myfactors)==1) p
  if (is.null(mytime) == 1) grDevices::dev.off()
  if (is.null(mytime) == 1) message("Check '", dir, "/' for png file with factor score plots")
  
  ##Empirical ICCs
  itemlong<-reshape2::melt(data_plus_scores,id.vars=c(myfactors,myindicators,"ETA"),measure.vars=myindicators)
  itemlong$value<-as.character(itemlong$value)
  itemlong$value<-as.numeric(itemlong$value)
  
  #attach(itemlong) #Should not have attach in here -- removed downstream references
  
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
  aggitemlong<-stats::aggregate(value~variable+roundETA,data=itemlong,FUN="mean")
  aggitemlong$eta<-aggitemlong$roundETA
  aggitemlong$item_response<-aggitemlong$value
  ICC<-with(aggitemlong,ggplot2::ggplot(aggitemlong,ggplot2::aes(x=eta,y=item_response))) + with(aggitemlong,ggplot2::facet_wrap(~ variable,nrow=1)) + with(aggitemlong,ggplot2::stat_smooth(method='lm',formula=y~exp(x)/(1+exp(x)),se=FALSE)) + with(aggitemlong,ggplot2::theme_bw()) + with(aggitemlong,ggplot2::labs(title="Empirical Item Characteristic Curves")) + with(aggitemlong,ggplot2::theme(legend.position="bottom"))
  filename<-fixPath(file.path(dir,"ICCplots.png",sep=""))
  grDevices::png(filename=filename,
      units="in",
      width=11,
      height=8.5,
      pointsize=12,
      res=72)
  ICC
  grDevices::dev.off()
  message("Check '", dir, "/' for png file with empirical ICC plots")
  
  utils::write.table(data_plus_scores, fixPath(file.path(dir,"mr_with_scores.dat",sep="")), sep="\t",col.names=TRUE,row.names=FALSE)
  
  message("Check '", dir, "/' for merged data file")
}