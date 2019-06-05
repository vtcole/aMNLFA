#' aMNLFA simultaneous model fitting function
#'
#' This function generates the simultaneous aMNLFA model from all the initial inputs.
#' @param input.object The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.
#' @keywords MNLFA
#' @export
#' @examples
#'  wd <- tempdir()
#'  first<-paste0(system.file(package='aMNLFA'), "/examplefiles")
#'  the.list <- list.files(first, full.names=TRUE)
#'  file.copy(the.list, wd, overwrite=TRUE)
#'    
#'  ob <- aMNLFA::aMNLFA.object(dir = wd,  
#'  mrdata = xstudy,  
#'  indicators = paste0("BIN_",  1:12), 
#'  catindicators = paste0("BIN_",  1:12),  
#'  meanimpact = c("AGE",  "GENDER",  "STUDY"),  
#'  varimpact = c("AGE",  "GENDER",  "STUDY"),  
#'  measinvar = c("AGE",  "GENDER",  "STUDY"), 
#'  factors = c("GENDER",  "STUDY"), 
#'  ID = "ID", 
#'  thresholds = FALSE)
#'  
#'  aMNLFA.simultaneous(ob)

aMNLFA.simultaneous <- function(input.object){

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

  varlist <- c(myID,  myauxiliary,  myindicators,  myMeasInvar,  myMeanImpact,  myVarImpact)
  varlist <- unique(varlist)

  USEVARIABLES <- paste("USEVARIABLES=")
  semicolon <- paste(";")
  AUXILIARY <- ifelse(length(myauxiliary)>0,  paste("AUXILIARY="),  paste("!"))
  AUXILIARY <- append(AUXILIARY,  myauxiliary)
  AUXILIARY <- noquote(append(AUXILIARY,  semicolon))
  AUXILIARY <- utils::capture.output(cat(AUXILIARY))
  CATEGORICAL <- paste("CATEGORICAL=")
  CATEGORICAL <- append(CATEGORICAL,  mycatindicators)
  CATEGORICAL <- noquote(append(CATEGORICAL,  semicolon))
  CATEGORICAL <- utils::capture.output(cat(CATEGORICAL))
  COUNT <- paste("COUNT=")
  COUNT <- append(COUNT,  mycountindicators)
  COUNT <- noquote(append(COUNT,  semicolon))
  COUNT <- utils::capture.output(cat(COUNT))
  ANALYSIS <- noquote("ANALYSIS: ESTIMATOR=ML;ALGORITHM=INTEGRATION;INTEGRATION=MONTECARLO;PROCESSORS=4;")
  ETA <- paste("ETA BY ")
  l <- length(myindicators)
  loadings <- list()
  for (i in 1:l){
    loadings[i] <- paste(ETA,  myindicators[i],  "*(L",  i,  ");",  sep="")
  }
  loadings <- noquote(loadings)
  loadings <- unlist(loadings)
  tech1 <- paste("OUTPUT: tech1;")
  MODEL <- paste("MODEL: [ETA@0]; ETA@1;")
  CONSTRAINT <- paste("CONSTRAINT=")
  varMODEL <- paste("MODEL: [ETA@0];ETA*(veta);")
  MODELCONSTRAINT <- paste("MODEL CONSTRAINT: new(")
  
  ####ROUND 1 USES p<.05 AS GATE TO GET TO ROUND 2 FOR MEAS INV and p<.1 for IMPACT####################

  ##Read in mean impact script and test for impact at p<.1
  meanimpact <- MplusAutomation::readModels(fixPath(file.path(dir,  "meanimpactscript.out",  sep="")))
  meanimpact <- as.data.frame(meanimpact$parameters$unstandardized)
  meanimpact <- meanimpact[which(meanimpact$paramHeader=="ETA.ON"),  ]
  meanimpact <- meanimpact[which(meanimpact$pval<.1),  ]
  keepmeanimpact <- meanimpact$param
  for (j in 1:length(keepmeanimpact)){
    if(length(grep("_",  keepmeanimpact[j]))>0) keepmeanimpact <- append(keepmeanimpact,  substr(keepmeanimpact[j],  1,  3))
    if(length(grep("_",  keepmeanimpact[j]))>0) keepmeanimpact <- append(keepmeanimpact,  substr(keepmeanimpact[j],  5,  7))
  }
  keepmeanimpact <- keepmeanimpact[which(keepmeanimpact!=2)]
  keepmeanimpact <- unique(keepmeanimpact)
  threeletterlist <- substr(myMeanImpact,  1,  3)
  test <- (keepmeanimpact[match(threeletterlist,  keepmeanimpact)])
  pos <- match(threeletterlist,  test)
  pos <- pos[!is.na(pos)]
  pos <- unique(pos)
  add <- myMeanImpact[pos]
  keepmeanimpact <- unique(append(keepmeanimpact,  add))
  keepmeanimpact2 <- match(keepmeanimpact,  myMeanImpact)
  keepmeanimpact2 <- keepmeanimpact2[!is.na(keepmeanimpact2)]
  keepmeanimpact <- myMeanImpact[keepmeanimpact2]

  meanimpactmodel <- paste0("ETA ON ", paste0(keepmeanimpact), ";\n", collapse = "")
  
  
  ##Read in var impact script and test for impact at p<.1
  varimpact <- MplusAutomation::readModels(fixPath(file.path(dir,  "varimpactscript.out",  sep="")))
  varimpact <- as.data.frame(varimpact$parameters$unstandardized)

  varimpact <- varimpact[which(varimpact$paramHeader=="New.Additional.Parameters"&varimpact$pval<.1),  ]  ######alpha <.1 to trim###########
  varimpact <- noquote(substr(varimpact$param,  2,  3))
  myVarImpact2 <- as.data.frame(myVarImpact)
  myVarImpact3 <- as.data.frame(NULL)
  for (i in 1:length(varimpact)){
    myVarImpact3[i,  1] <- myVarImpact2[varimpact[i],  ]
  }
  keepvarimpact <- noquote(t(myVarImpact3))

  for (j in 1:length(keepvarimpact)){
    if(length(grep("_",  keepvarimpact[j]))>0) keepvarimpact <- append(keepvarimpact,  substr(keepvarimpact[j],  1,  3))
    if(length(grep("_",  keepvarimpact[j]))>0) keepvarimpact <- append(keepvarimpact,  substr(keepvarimpact[j],  5,  7))
  }
  keepvarimpact <- keepvarimpact[which(keepvarimpact!=2)]
  keepvarimpact <- unique(keepvarimpact)
  threeletterlist <-  stringr::str_sub(myVarImpact,  1, 3)
  test <- (keepvarimpact[match(threeletterlist, keepvarimpact)])
  pos <- match(threeletterlist, test)
  pos <- pos[!is.na(pos)]
  pos <- unique(pos)
  add <- myVarImpact[pos]
  keepvarimpact <- unique(append(keepvarimpact, add))
  keepvarimpact2 <- match(keepvarimpact, myVarImpact)
  keepvarimpact2 <- keepvarimpact2[!is.na(keepvarimpact2)]
  keepvarimpact <- myVarImpact[keepvarimpact2]

  keepmeanimpact <- unique(append(keepmeanimpact, keepvarimpact))
  
  #MEASUREMENT INVARIANCE
  #START WITH LOADINGS BECAUSE THRESHOLDS AND INTERCEPTS DEPEND ON THEM
  
  dif <- list()
  l.design <- matrix(NA, l, length(myMeasInvar))
  l.string.new <- list()
  l.string.eq <- list()
  
  for (ll in 1:l) {
    dif[[ll]] <- MplusAutomation::readModels(fixPath(file.path(dir, paste("measinvarscript_", myindicators[ll], ".out", sep=""))))$parameters$unstandardized #Note that this dif list will be called later in either the intercept or threshold DIF portion
    l.input <- l.fun(ll, dif[[ll]], myMeasInvar, .05)
    l.design[ll,] <- l.input$mat
    l.string.new <- append(l.string.new, l.input$new.con)
    l.string.eq <- append(l.string.eq, l.input$eq.con)
  }
  
  l.string.new <- unlist(l.string.new)
  l.string.eq <- unlist(l.string.eq)
  
  uniqueloading <- myMeasInvar[apply(l.design,2,sum) > 0]
  
  #THRESHOLDS
  #Code branches here, because all subsequent steps (i.e., finding threshold DIF, writing output) differ if we have thresholds vs. intercepts
  if (thresholds == TRUE) {
    t.design <- matrix(NA, l, length(myMeasInvar))
    t.string.new <- list()
    t.string.eq <- list()
    t.string.model <- list()
    
    #Apply the threshold function to get constraint "new" declarations, constraint equations, and model statements for thresholds
    for (tt in 1:l) {
      t.input <- t.fun(tt, dif[[tt]], myMeasInvar, .05, l.design)
      t.design[tt,] <- t.input$mat
      t.string.new <- append(t.string.new, t.input$new.con)
      t.string.eq <- append(t.string.eq, paste(t.input$eq.con, collapse = ""))
      t.string.model <- append(t.string.model, t.input$model.con)
      }
      
      t.string.new <- unlist(t.string.new)
      t.string.eq <- unlist(t.string.eq)
      t.string.model <- unlist(t.string.model)
    
      uniquethreshold <- myMeasInvar[apply(t.design,2,sum) > 0]
      
      diflist <- unique(uniquethreshold, uniqueloading)
    
      ##Writing script with all parameters with p<.05
      useround2 <- c(diflist, keepmeanimpact, keepvarimpact, myindicators)
      useround2 <- noquote(unique(useround2))
      useround2 <- utils::capture.output(cat(useround2))
      ETAON2 <- paste("ETA ON ", keepmeanimpact, semicolon, sep="")
      con <- unique(c(keepvarimpact, uniqueloading, uniquethreshold))
      con <- con[!is.na(con)]
      CONSTRAINT <- noquote(append(CONSTRAINT, con))
      CONSTRAINT <- append(CONSTRAINT, semicolon)
      CONSTRAINT <- utils::capture.output(cat(CONSTRAINT))
    
      header <- readLines(fixPath(file.path(dir, "header.txt")))
        
      round2input <- as.data.frame(NULL)
      round2input[1, 1] <- paste("TITLE: Round 2 Calibration Model")
      round2input[2, 1] <- header[2]
      round2input[3, 1] <- header[3]
      round2input[4, 1] <- header[4]
      round2input[5, 1] <- header[5]
      round2input[6, 1] <- ifelse(length(header)>5, header[6], "!")
      round2input[7, 1] <- ifelse(length(header)>6, header[7], "!")
      round2input[8, 1] <- ifelse(length(header)>7, header[8], "!")
      round2input[9, 1] <- ifelse(length(header)>8, header[9], "!")
      round2input[10, 1] <- paste("USEVARIABLES= ", useround2, semicolon, sep="")
      round2input[11, 1] <- AUXILIARY
      round2input[12, 1] <- ifelse(length(mycatindicators)>0, CATEGORICAL, "!")
      round2input[13, 1] <- ifelse(length(mycountindicators)>0, COUNT, "!")
      round2input[14, 1] <- CONSTRAINT
      round2input[15, 1] <- ANALYSIS
      round2input[16, 1] <- varMODEL
      l <- length(loadings)
      current <- 17 #Setting a counter variable
      
      #Define factor loadings
      for (l1 in 1:l){
        round2input[current, 1] <- loadings[l1]
        current <- current + 1 
      }
      
      #Define thresholds
      for (t1 in 1:length(t.string.model)) {
        round2input[current, 1] <- t.string.model[t1]
        current <- current + 1
      }
      
      #Add mean impact
      round2input[current, 1] <- meanimpactmodel
      current <- current + 1
      
      #Generate syntax for variance impact
      vstart <- data.frame(NULL)
      if (length(keepvarimpact)>0)
        for (v1 in 1:length(keepvarimpact)){
          vstart[v1, 1] <- paste("v", v1, "*0", sep="")
        }
      vstart <- ifelse(length(keepvarimpact)>0, paste(utils::capture.output(cat(noquote(unlist(vstart)))), sep=""), "!")
      
      #Model constraint statement starts here
      round2input[current, 1] <- utils::capture.output(cat(append(MODELCONSTRAINT, vstart)) )
      current <- current + 1
      
      #Declare new variance parameters
      for (v2 in 1:length(vstart)) {
        round2input[current, 1] <- vstart[v2]
        current <- current + 1
      }
      
      #Declare new threshold parameters
      if(length(t.string.new) > 0) {
        for (t2 in 1:length(t.string.new)) {
          round2input[current, 1] <- t.string.new[t2]
          current <- current + 1
        }
      }
      
      #Declare new loading parameters
      if(length(l.string.new) > 0) {
        for (l2 in 1:length(l.string.new)) {
          round2input[current, 1] <- l.string.new[l2]
          current <- current + 1
        }
      }
      
      round2input[current, 1] <- ");" #This terminates the declaration of new parameters
      current <- current + 1
      
      #Define variance impact
      veq <- as.data.frame(NULL)
      veq[1, 1] <- "veta=1*exp("
      if (length(keepvarimpact)>0)
        for (v in 1:length(keepvarimpact)){
          veq[v+1, 1] <- paste("v", v, "*", keepvarimpact[v], "+", sep="")
        }
      v <- length(keepvarimpact)
      veq[v+2, 1] <- paste("0)")
  
      round2input[current, 1] <- paste(utils::capture.output(cat(noquote(unlist(veq)))), semicolon, sep="")
      current <- current + nrow(veq)
      
      #Define loading DIF
      if(length(l.string.eq) > 0) {
        for (l3 in 1:length(l.string.eq)) {
          round2input[current, 1] <- l.string.eq[l3]
          current <- current + 1
        }
      }
      
      #Define threshold DIF
      if(length(t.string.eq) > 0) {
        for (t3 in 1:length(t.string.eq)) {
          round2input[current, 1] <- t.string.eq[t3]
          current <- current + 1
        }
      }
  
      round2input[current, 1] <- tech1
  }
  
  if (thresholds == FALSE) {
    i.design <- matrix(NA, l, length(myMeasInvar))
    i.string.model <- list()
    
    #Apply the threshold function to get constraint "new" declarations, constraint equations, and model statements for thresholds
    for (ii in 1:l) {
      i.input <- i.fun(ii, dif[[ii]], myMeasInvar, .05, l.design)
      i.design[ii,] <- i.input$mat
      i.string.model <- append(i.string.model, i.input$model.con)
    }

    i.string.model <- unlist(i.string.model)
    
    uniqueintercept <- myMeasInvar[apply(i.design,2,sum) > 0]
    
    diflist <- unique(uniqueintercept, uniqueloading)
    
    ##Writing script with all parameters with p<.05
    useround2 <- c(diflist, keepmeanimpact, keepvarimpact, myindicators)
    useround2 <- noquote(unique(useround2))
    useround2 <- utils::capture.output(cat(useround2))
    ETAON2 <- paste("ETA ON ", keepmeanimpact, semicolon, sep="")
    con <- unique(c(keepvarimpact, uniqueloading)) #Note that the uniquethreshold object is omitted here
    con <- con[!is.na(con)]
    CONSTRAINT <- noquote(append(CONSTRAINT, con))
    CONSTRAINT <- append(CONSTRAINT, semicolon)
    CONSTRAINT <- utils::capture.output(cat(CONSTRAINT))
    
    header <- readLines(fixPath(file.path(dir, "header.txt")))
    
    round2input <- as.data.frame(NULL)
    round2input[1, 1] <- paste("TITLE: Round 2 Calibration Model")
    round2input[2, 1] <- header[2]
    round2input[3, 1] <- header[3]
    round2input[4, 1] <- header[4]
    round2input[5, 1] <- header[5]
    round2input[6, 1] <- ifelse(length(header)>5, header[6], "!")
    round2input[7, 1] <- ifelse(length(header)>6, header[7], "!")
    round2input[8, 1] <- ifelse(length(header)>7, header[8], "!")
    round2input[9, 1] <- ifelse(length(header)>8, header[9], "!")
    round2input[10, 1] <- paste("USEVARIABLES= ", useround2, semicolon, sep="")
    round2input[11, 1] <- AUXILIARY
    round2input[12, 1] <- ifelse(length(mycatindicators)>0, CATEGORICAL, "!")
    round2input[13, 1] <- ifelse(length(mycountindicators)>0, COUNT, "!")
    round2input[14, 1] <- CONSTRAINT
    round2input[15, 1] <- ANALYSIS
    round2input[16, 1] <- varMODEL
    l <- length(loadings)
    current <- 17 #Setting a counter variable
    
    #Define factor loadings
    for (l1 in 1:l){
      round2input[current, 1] <- loadings[l1]
      current <- current + 1 
    }
    
    #Add intercept DIF
    for (i1 in 1:length(i.string.model)) {
      round2input[current, 1] <- i.string.model[i1]
      current <- current + 1
    }
    
    #Add mean impact
    round2input[current, 1] <- meanimpactmodel
    current <- current + 1
    
    #Generate syntax for variance impact
    vstart <- data.frame(NULL)
    if (length(keepvarimpact)>0)
      for (v1 in 1:length(keepvarimpact)){
        vstart[v1, 1] <- paste("v", v1, "*0", sep="")
      }
    vstart <- ifelse(length(keepvarimpact)>0, paste(utils::capture.output(cat(noquote(unlist(vstart)))), sep=""), "!")

    #Model constraints statement begins here
    round2input[current, 1] <- utils::capture.output(cat(append(MODELCONSTRAINT)) )
    current <- current + 1

    #Declare the new variance parameters
    for (v2 in 1:length(vstart)) {
      round2input[current, 1] <- vstart[v2]
      current <- current + 1
    }

    #Declare the new loading parameters
    if(length(l.string.new) > 0) {
      for (l2 in 1:length(l.string.new)) {
        round2input[current, 1] <- l.string.new[l2]
        current <- current + 1
      }
    }
    
    round2input[current, 1] <- ");" #This terminates the declaration of new parameters
    current <- current + 1
    
    #Define variance impact
    veq <- as.data.frame(NULL)
    veq[1, 1] <- "veta=1*exp("
    if (length(keepvarimpact)>0)
      for (v in 1:length(keepvarimpact)){
        veq[v+1, 1] <- paste("v", v, "*", keepvarimpact[v], "+", sep="")
      }
    v <- length(keepvarimpact)
    veq[v+2, 1] <- paste("0)")
    
    round2input[current, 1] <- paste(utils::capture.output(cat(noquote(unlist(veq)))), semicolon, sep="")
    current <- current + nrow(veq)
    
    #Define loading DIF
    if(length(l.string.eq) > 0) {
      for (l3 in 1:length(l.string.eq)) {
        round2input[current, 1] <- l.string.eq[l3]
        current <- current + 1
      }
    }
    
    round2input[current, 1] <- tech1
  }
  
  #write.table(round2input, file.path(dir, "round2calibration.inp", sep=""), append=F, row.names=FALSE, col.names=FALSE, quote=FALSE)
  write.inp.file(round2input, fixPath(file.path(dir, "round2calibration.inp", sep="")))
  message("COMPLETE. Check '",  dir,  "/' for Mplus inp file for round 2 calibration model (run this manually). \nNOTE: After running  your round 2 calibration,  there may be some output that cannot be read in properly as a result of recent changes within Mplus. This will lead to errors in subsequent steps. \nAs a temporary fix the problem,  please delete all output that comes after the 'LOGISTIC REGRESSION ODDS RATIO RESULTS' section after running your round 3 calibration,  before proceeding to the next step. \nThis message will appear after all subsequent steps.")  
}
