#' aMNLFA item plotting function
#'
#' This function generates plots of item endorsement by time, and by each covariate. This is necessary for determining which covariates to use in the MNLFA.
#' @param input.object The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.
#' @keywords MNLFA
#' @export
#' @examples
#' aMNLFA.itemplots()

aMNLFA.itemplots<-function(input.object){
  
  path = input.object$path
  mrdata = input.object$mrdata
  myindicators = input.object$indicators
  mytime = input.object$time
  myfactors = input.object$factors
  myauxiliary = input.object$auxiliary
  myID = input.object$ID
  
  #Visualize indicators as a function of time and moderators
  #re-structure data to allow facet_wrap visualization
  if (!is.na(mytime)) indlong<-melt(mrdata,id.vars=c(myauxiliary,mytime,myfactors),measure.vars=myindicators)
  if (is.na(mytime)) indlong<-melt(mrdata,id.vars=c(myauxiliary,myfactors),measure.vars=myindicators)
  
  attach(indlong)
  indlong$AvgItemResponse<-as.character(indlong$value)
  indlong$AvgItemResponse<-as.numeric(indlong$AvgItemResponse)
  if (!is.na(mytime)) indlong$time<-as.numeric(unlist(indlong[mytime]))
  if (!is.na(mytime)) indlong$time<-round(indlong$time,.1)
  mrdata<-mrdata[order(mrdata[myID]),]
  srdatacheck<-mrdata[!duplicated(mrdata[myID]),]
  N<-dim(srdatacheck)[1]
  min<-.01*N
  if (!is.na(mytime)) aggindlong<-aggregate(AvgItemResponse~variable+time,indlong,FUN="mean")
  if (!is.na(mytime)) aggindlong2<-aggregate(AvgItemResponse~variable+time,indlong,FUN="length")
  if (is.na(mytime)) aggindlong<-aggregate(AvgItemResponse~variable,indlong,FUN="mean")
  if (is.na(mytime)) aggindlong2<-aggregate(AvgItemResponse~variable,indlong,FUN="length")
  aggindlong$N<-aggindlong2$AvgItemResponse
  aggindlong<-aggindlong[which(aggindlong$N>min),]
  if (!is.na(mytime)) marg<-ggplot(aggindlong,aes(x=time,y=AvgItemResponse))+ facet_wrap(~ variable,nrow=1) +geom_point(aes(size=N))+stat_smooth(se=FALSE)+theme_bw()+labs(title="Average Indicator Responses over Time")+ theme(legend.position="bottom")
  #plot for each moderator
  l<-length(myfactors)
  
  if (!is.na(mytime)) for (i in 1:l){
    p<-NULL
    keep<-c("AvgItemResponse","time","variable",myfactors[i])
    indlongmod<-indlong[keep]
    names(indlongmod)[4]<-"Moderator"
    ic<-indlongmod$Moderator=="."|is.na(indlongmod$Moderator)|indlongmod$Moderator=="NA"
    cc_long<-indlongmod[which(ic=="FALSE"),]
    aggindlongmod<-aggregate(AvgItemResponse~variable+time+Moderator,cc_long,FUN="mean")
    aggindlongmod_2<-aggregate(AvgItemResponse~variable+time+Moderator,cc_long,FUN="length")
    aggindlongmod$N<-aggindlongmod_2$AvgItemResponse
    title<-paste("Average Indicator Responses over Time by ",myfactors[i],sep="")
    aggindlongmod$Moderator<-as.factor(aggindlongmod$Moderator)
    p<-ggplot(aggindlongmod,aes(x=time,y=AvgItemResponse))+ facet_wrap(~ variable,nrow=round(sqrt(length(myfactors)))) +geom_point(aes(size=N,colour=Moderator))+stat_smooth(se=FALSE,aes(colour=Moderator))+theme_bw()+labs(title=title)+ theme(legend.position="bottom") + guides(size=FALSE)
    
    filename<-paste(path,"/itemplots",myfactors[i],".png",sep="")
    png(filename=filename,
        units="in",
        width=11,
        height=8.5,
        pointsize=12,
        res=72)
    print(p)
    dev.off()
  }
  
  
  if (is.na(mytime)) for (i in 1:l){
    for (j in 1:length(myindicators)){
      p<-NULL
      indlong2<-indlong[which(variable==myindicators[j]),]
      keep<-c("value",myfactors[i])
      indlongmod<-indlong2[keep]
      names(indlongmod)[2]<-"Moderator"
      ic<-indlongmod$Moderator=="."|is.na(indlongmod$Moderator)|indlongmod$Moderator=="NA"
      cc_long<-indlongmod[which(ic=="FALSE"),]
      title<-paste(myindicators[j],"Responses Distribution by ",myfactors[i],sep="")
      p<-ggplot(cc_long,aes(factor(Moderator),value))+geom_boxplot()+theme_bw()+labs(title=title)+ theme(legend.position="bottom")
      filename<-paste(path,"/",myindicators[j]," plots",myfactors[i],".png",sep="")
      png(filename=filename,
          units="in",
          width=11,
          height=8.5,
          pointsize=12,
          res=72)
      print(p)
      dev.off()
    }}
  
  message("Check '", path, "/' for png file with item plots")
}