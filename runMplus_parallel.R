runmplus <- function(path = tmp, 
                     cores= detectCores()-1,
                     outPath = tmp,
                     logPath = tmp,
                     pattern = NULL) {
  library(doParallel)
  library(stringr)
  #### Mplus function
  mplus <- function(x, out = outfiles, log = logfiles) {
    # Mplus_command (path must be added to system variables)
    Mplus_command <- "mplus"
    
    system2(
      Mplus_command,
      args = c(x, out),
      stdout = log ,
      wait = TRUE
    )
  }
  
  # Create a vector of .inp files
  if (is.null(pattern))                                       
    pattern <-  ".inp"
  inpfiles <- list.files(path = path ,
                         pattern = pattern,
                         full.names = TRUE)
  
  # Warning Indicator
  if (length(inpfiles) < 1)
    stop("No Mplus input files detected in the target directory: ",
         directory)
  
  # Create directories if they don't exist
  if(!dir.exists(outPath)) dir.create(outPath)
  if(!dir.exists(logPath)) dir.create(logPath)
  
  # Create vectors of outfiles and logfiles for naming
  outfiles <- str_replace(inpfiles, pattern = ".inp", ".out")
  logfiles <- str_replace(inpfiles, pattern = ".inp", ".log")
  
  # Run Models in Parallel
  cl <- makeCluster(cores)
  registerDoParallel(cores)
  foreach(i = iter(inpfiles),
          j = icount(length(inpfiles)),
          .inorder = FALSE) %dopar% mplus(i, out = outfiles[j], log = logfiles[j])
  stopCluster(cl)
  return(print("DONE RUNNING MODELS"))
}
