####FUNCTIONS FOR DEALING WITH INPUT
#For some reason fixPath isn't working -- temporarily getting rid of it. 2/15/2018
fixPath <- function(somecharacter) {
  somecharacter
}

#Remove final slash to allow for better use of file.path down the line.
#fixPath<-function(somecharacter) {
#  lastcharacter<-base::substring(somecharacter,nchar(somecharacter))
#  newcharacter<-base::substr(somecharacter,1,nchar(somecharacter)-1)
#  outcharacter<-somecharacter
#  if (lastcharacter=="/") {outcharacter<-newcharacter}
#  outcharacter
#}

####FUNCTIONS FOR READING IN DIF OUTPUT AND WRITING CORRESPONDING INPUT
#This is the function which reads some loading DIF output (i.e., output testing the effects of all covariates on a given loading) and writes the item-specific inputs for all significant covariates
#Outputs: the declaration of loadings in the MODEL section; the declaration of loadings in the NEW subsection of the CONSTRAINTS section; the equations for the loadings in the CONSTRAINTS section; and a JxP design matrix (where J = number of indicators; P = number of covariates) containing a record of whether the effect of the pth covariate on the jth item is significant.
l.fun <- function(j, params, covslist, alpha) {
  mat <- rep(0, length(covslist))
  new.con <- paste0("L", j, "_00")
  eq.con <- paste0("L", j, "= L", j, "_00")
  the.prefix <- paste0("L", j, "_")
  #the.infix <- paste0("_", k, "_")
  thisitem <- subset(params, startsWith(params$param, the.prefix) == TRUE)
  for (p in 1:length(covslist)) {
    the.suffix <- paste0("_", p)
    thiscov <- subset(thisitem, endsWith(thisitem$param, the.suffix) == TRUE)
    if (is.na(thiscov$param[1]) == FALSE && thiscov$pval < alpha) {
      mat[p] <- 1
      new.con.add <- paste0("L", j, "_", p) 
      new.con <- append(new.con, new.con.add)
      eq.con.add <- paste0("+ \nL", j, "_", p, "*", covslist[p]) 
      eq.con <- append(eq.con, eq.con.add)
    }
  }
  eq.con <- append(eq.con, ";")
  list(new.con = new.con, eq.con = eq.con, mat = mat)
}

#This is a function which reads in the threshold DIF output and writes the necessary input, similar to the loading DIF functions above. Note that it writes the input if the corresponding loading is significant.
t.fun <- function(j, params, covslist, alpha, inmat) {
  mat <- rep(0, length(covslist))
  eq.con <- list()
  new.con <- list()
  model.con <- list()
  the.prefix <- paste0("T", j, "_")
  thisitem <- subset(params, startsWith(params$param, the.prefix) == TRUE)
  K <- max(as.numeric(sub(".*_ *(.*?) *_.*", "\\1", thisitem$param))) #finds the maximum number of levels by finding the largest threshold, which is found by looking between the 2 underscores in the parameter name
  for (k in 1:K) {
    new.con.k <- paste0("T", j, "_", k, "_00")
    eq.con.k <- paste0("\n T", j, "_", k, "= T", j, "_", k, "_00")
    model.con.k <- paste0("[", myindicators[j], "$", k, "] (", the.prefix, k, ");")
    for (p in 1:length(covslist)) {
      the.suffix <- paste0("_", p)
      thiscov <- subset(thisitem, endsWith(thisitem$param, the.suffix) == TRUE)
      if ((is.na(thiscov$param[1]) == FALSE && min(thiscov$pval) < alpha) | inmat[j, p] == 1) { #If DIF effect is significant for any of the thresholds, retain all thresholds
        mat[p] <- 1
        new.con.k.add <- paste0("T", j, "_", k, "_", p) 
        new.con.k <- append(new.con.k, new.con.k.add)
        eq.con.k.add <- paste0("+ \nT", j, "_", k, "_", p, "*", covslist[p]) 
        eq.con.k <- append(eq.con.k, eq.con.k.add)
      }
    }
    new.con <- append(new.con, new.con.k)
    eq.con <- append(eq.con, paste0(paste(eq.con.k, collapse = ""), ";"))
    model.con <- append(model.con, model.con.k)
  }
  list(new.con = new.con, eq.con = eq.con, model.con = model.con, mat = mat)
}

#This is a function which reads in the intercept DIF output and writes the necessary input, similar to the loading DIF functions above. Note that it writes the input if the corresponding loading is significant.
i.fun <-function(j, params, covslist, alpha, inmat) {
  mat <- rep(0, length(covslist))
  model.con <- list()
  the.header <- paste0(myindicators[j],".ON")
  thisitem <- subset(params, params$paramHeader == the.header)
  model.args <- list()
  for (p in 1:length(covslist)) {
    thiscov <- subset(thisitem, thisitem$param == covslist[p])
    if ((is.na(thiscov$param[1]) == FALSE && min(thiscov$pval) < alpha) | inmat[j, p] == 1) { #If alpha is less than .05 and/or loading DIF is significant
      mat[p] <- 1
      model.args <- append(model.args, myMeasInvar[p])
    } 
  }
  if (length(model.args) > 0) {
    model.con <- paste0(myindicators[j], " ON ", model.args, ";\n")
  }
  list(model.con = model.con, mat = mat)
}
