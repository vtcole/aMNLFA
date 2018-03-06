# loading packages
library("devtools")
# library('stringr')

# pull the most up to date package from github
install_github(repo       = "mlgiordano1/aMNLFA-1@testing")
library("aMNLFA")

# set the wd
wd <- "c:/users/mgiordan/git/amnlfa-1/testing"
setwd(wd)

df <- read.table("ss0ni1im0df2pd0r1.dat")
names(df) <- c("ID", "AGECENT", "GENEFF", "STUDYEFF", "STUDYAGE",
               "ETA", "STUDYETA", "ZETA", "W", "Z1", "Z2", "Z3", "Z4", 
               paste0("BIN_", 1:12))
write.table(df, "mr.dat")

ob <- aMNLFA::aMNLFA.object(path    = getwd(), 
                      mrdata        = df,
                      indicators    = paste0("BIN_", 1:12), 
                      catindicators = paste0("BIN_", 1:12), 
                      meanimpact    = c("AGECENT", "GENEFF", "STUDYEFF"), 
                      varimpact     = c("AGECENT", "GENEFF", "STUDYEFF"), 
                      measinvar     = c("AGECENT", "GENEFF", "STUDYEFF"), 
                      factors       = c("GENEFF", "STUDYEFF"), 
                      ID            = "ID", 
                      thresholds    = FALSE)
# make plots
aMNLFA::aMNLFA.itemplots(ob)
# sample
aMNLFA::aMNLFA.sample(ob)
# make scripts
aMNLFA::aMNLFA.initial(ob)
MplusAutomation::runModels(replaceOutfile = "modifiedDate")
# take all marginally sig dif
aMNLFA::aMNLFA.simultaneous(ob)
MplusAutomation::runModels(replaceOutfile = "modifiedDate")
# make the final model
aMNLFA.final(ob)
MplusAutomation::runModels(replaceOutfile = "modifiedDate")

