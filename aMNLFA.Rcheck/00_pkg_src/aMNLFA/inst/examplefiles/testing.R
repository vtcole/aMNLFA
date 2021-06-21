# set the wd
wd <- "c:/users/mgiordan/desktop/testing"
setwd(wd)

# loading packages
library("devtools")

# pull the most up to date package from github
install_github(repo = "vtcole/aMNLFA")
# install_github(repo = "mlgiordano1/aMNLFA-1@dev")
library("aMNLFA")

# # read original data frame
# df <- read.table("ss0ni1im0df2pd0r1.dat")
# # add names
# names(df) <- c("ID", "AGE", "GENDER", "STUDY", "STUDYAGE",
#                "ETA", "STUDYETA", "ZETA", "W", "Z1", "Z2", "Z3", "Z4", 
#                paste0("BIN_", 1:12))
# # subset just the relevant variables
# df <- df[,c("ID", "AGE", "GENDER", "STUDY", paste0("BIN_", 1:12))]
# # save as .rds file
# saveRDS(df, "mnlfa_example_data.rds")
df <- readRDS("mnlfa_example_data.rds")

# doing the aMNLFA thing
ob <- aMNLFA::aMNLFA.object(path          = wd,
                            mrdata        = df,
                            indicators    = paste0("BIN_", 1:12), 
                            catindicators = paste0("BIN_", 1:12), 
                            meanimpact    = c("AGE", "GENDER", "STUDY"), 
                            varimpact     = c("AGE", "GENDER", "STUDY"), 
                            measinvar     = c("AGE", "GENDER", "STUDY"), 
                            factors       = c("GENDER", "STUDY"), 
                            ID            = "ID", 
                            thresholds    = FALSE)

# make plots
aMNLFA::aMNLFA.itemplots(ob)
# sample
aMNLFA.sample(ob)
# make scripts
aMNLFA::aMNLFA.initial(ob)
MplusAutomation::runModels(replaceOutfile = "modifiedDate")
# take all marginally sig dif
aMNLFA::aMNLFA.simultaneous(ob)
MplusAutomation::runModels(replaceOutfile = "modifiedDate")
# make the final model
aMNLFA.final(ob)
MplusAutomation::runModels(replaceOutfile = "modifiedDate")


