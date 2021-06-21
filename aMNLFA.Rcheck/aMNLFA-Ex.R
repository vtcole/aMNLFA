pkgname <- "aMNLFA"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "aMNLFA-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('aMNLFA')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("aMNLFA.final")
### * aMNLFA.final

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aMNLFA.final
### Title: aMNLFA final model fitting function
### Aliases: aMNLFA.final
### Keywords: MNLFA

### ** Examples

 wd <- tempdir()
 first<-paste0(system.file(package='aMNLFA'),"/examplefiles")
 the.list <- list.files(first,full.names=TRUE)
 file.copy(the.list,wd,overwrite=TRUE)
 ob <- aMNLFA::aMNLFA.object(dir = wd, 
 mrdata = xstudy, 
 indicators = paste0("BIN_", 1:12),
 catindicators = paste0("BIN_", 1:12), 
 meanimpact = c("AGE", "GENDER", "STUDY"), 
 varimpact = c("AGE", "GENDER", "STUDY"), 
 measinvar = c("AGE", "GENDER", "STUDY"),
 factors = c("GENDER", "STUDY"),
 ID = "ID",
 thresholds = FALSE)
 
 aMNLFA.final(ob)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("aMNLFA.final", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("aMNLFA.initial")
### * aMNLFA.initial

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aMNLFA.initial
### Title: aMNLFA initial model fitting function
### Aliases: aMNLFA.initial
### Keywords: MNLFA

### ** Examples

 wd <- tempdir()
 first<-paste0(system.file(package='aMNLFA'),"/examplefiles")
 the.list <- list.files(first,full.names=TRUE)
 file.copy(the.list,wd,overwrite=TRUE)
 ob <- aMNLFA::aMNLFA.object(dir = wd, 
 mrdata = xstudy, 
 indicators = paste0("BIN_", 1:12),
 catindicators = paste0("BIN_", 1:12), 
 meanimpact = c("AGE", "GENDER", "STUDY"), 
 varimpact = c("AGE", "GENDER", "STUDY"), 
 measinvar = c("AGE", "GENDER", "STUDY"),
 factors = c("GENDER", "STUDY"),
 ID = "ID",
 thresholds = FALSE)
 
 aMNLFA.initial(ob)
 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("aMNLFA.initial", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("aMNLFA.itemplots")
### * aMNLFA.itemplots

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aMNLFA.itemplots
### Title: aMNLFA item plotting function
### Aliases: aMNLFA.itemplots
### Keywords: MNLFA

### ** Examples

 wd <- tempdir()
 first<-paste0(system.file(package='aMNLFA'),"/examplefiles")
 the.list <- list.files(first,full.names=TRUE)
 file.copy(the.list,wd,overwrite=TRUE)
 ob <- aMNLFA::aMNLFA.object(dir = wd, 
 mrdata = xstudy, 
 indicators = paste0("BIN_", 1:12),
 catindicators = paste0("BIN_", 1:12), 
 meanimpact = c("AGE", "GENDER", "STUDY"), 
 varimpact = c("AGE", "GENDER", "STUDY"), 
 measinvar = c("AGE", "GENDER", "STUDY"),
 factors = c("GENDER", "STUDY"),
 ID = "ID",
 thresholds = FALSE)
 
 aMNLFA.itemplots(ob)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("aMNLFA.itemplots", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("aMNLFA.object")
### * aMNLFA.object

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aMNLFA.object
### Title: aMNLFA object function
### Aliases: aMNLFA.object
### Keywords: MNLFA

### ** Examples

 wd <- tempdir()
 first<-paste0(system.file(package='aMNLFA'),"/examplefiles")
 the.list <- list.files(first,full.names=TRUE)
 file.copy(the.list,wd,overwrite=TRUE)
 
 ob <- aMNLFA::aMNLFA.object(dir = wd, 
 mrdata = xstudy, 
 indicators = paste0("BIN_", 1:12),
 catindicators = paste0("BIN_", 1:12), 
 meanimpact = c("AGE", "GENDER", "STUDY"), 
 varimpact = c("AGE", "GENDER", "STUDY"), 
 measinvar = c("AGE", "GENDER", "STUDY"),
 factors = c("GENDER", "STUDY"),
 ID = "ID",
 thresholds = FALSE)
 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("aMNLFA.object", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("aMNLFA.sample")
### * aMNLFA.sample

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aMNLFA.sample
### Title: aMNLFA sampling function
### Aliases: aMNLFA.sample
### Keywords: MNLFA

### ** Examples

 wd <- tempdir()
 first<-paste0(system.file(package='aMNLFA'),"/examplefiles")
 the.list <- list.files(first,full.names=TRUE)
 file.copy(the.list,wd,overwrite=TRUE)
 
 ob <- aMNLFA::aMNLFA.object(dir = wd, 
 mrdata = xstudy, 
 indicators = paste0("BIN_", 1:12),
 catindicators = paste0("BIN_", 1:12), 
 meanimpact = c("AGE", "GENDER", "STUDY"), 
 varimpact = c("AGE", "GENDER", "STUDY"), 
 measinvar = c("AGE", "GENDER", "STUDY"),
 factors = c("GENDER", "STUDY"),
 ID = "ID",
 thresholds = FALSE)
 
 aMNLFA.sample(ob)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("aMNLFA.sample", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("aMNLFA.scoreplots")
### * aMNLFA.scoreplots

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aMNLFA.scoreplots
### Title: aMNLFA score plotting function
### Aliases: aMNLFA.scoreplots
### Keywords: MNLFA

### ** Examples

 wd <- tempdir()
 first<-paste0(system.file(package='aMNLFA'),"/examplefiles")
 the.list <- list.files(first,full.names=TRUE)
 file.copy(the.list,wd,overwrite=TRUE)
 
 ob <- aMNLFA::aMNLFA.object(dir = wd, 
 mrdata = xstudy, 
 indicators = paste0("BIN_", 1:12),
 catindicators = paste0("BIN_", 1:12), 
 meanimpact = c("AGE", "GENDER", "STUDY"), 
 varimpact = c("AGE", "GENDER", "STUDY"), 
 measinvar = c("AGE", "GENDER", "STUDY"),
 factors = c("GENDER", "STUDY"),
 ID = "ID",
 thresholds = FALSE)
 
 aMNLFA.scoreplots(ob)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("aMNLFA.scoreplots", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("aMNLFA.scores")
### * aMNLFA.scores

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aMNLFA.scores
### Title: aMNLFA score generating function
### Aliases: aMNLFA.scores
### Keywords: MNLFA

### ** Examples

 wd <- tempdir()
 first<-paste0(system.file(package='aMNLFA'),"/examplefiles")
 the.list <- list.files(first,full.names=TRUE)
 file.copy(the.list,wd,overwrite=TRUE)
 
 ob <- aMNLFA::aMNLFA.object(dir = wd, 
 mrdata = xstudy, 
 indicators = paste0("BIN_", 1:12),
 catindicators = paste0("BIN_", 1:12), 
 meanimpact = c("AGE", "GENDER", "STUDY"), 
 varimpact = c("AGE", "GENDER", "STUDY"), 
 measinvar = c("AGE", "GENDER", "STUDY"),
 factors = c("GENDER", "STUDY"),
 ID = "ID",
 thresholds = FALSE)
 
 aMNLFA.scores(ob)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("aMNLFA.scores", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("aMNLFA.simultaneous")
### * aMNLFA.simultaneous

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aMNLFA.simultaneous
### Title: aMNLFA simultaneous model fitting function
### Aliases: aMNLFA.simultaneous
### Keywords: MNLFA

### ** Examples

 wd <- tempdir()
 first<-paste0(system.file(package='aMNLFA'),"/examplefiles")
 the.list <- list.files(first,full.names=TRUE)
 file.copy(the.list,wd,overwrite=TRUE)
   
 ob <- aMNLFA::aMNLFA.object(dir = wd, 
 mrdata = xstudy, 
 indicators = paste0("BIN_", 1:12),
 catindicators = paste0("BIN_", 1:12), 
 meanimpact = c("AGE", "GENDER", "STUDY"), 
 varimpact = c("AGE", "GENDER", "STUDY"), 
 measinvar = c("AGE", "GENDER", "STUDY"),
 factors = c("GENDER", "STUDY"),
 ID = "ID",
 thresholds = FALSE)
 
 aMNLFA.simultaneous(ob)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("aMNLFA.simultaneous", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
