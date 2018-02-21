#' Function for generating Mplus input files.
#'
#' This is a helper function which the user will not interface with; it is used to create Mplus inputs.
#' @param df The data file to use.
#' @param outfile The output Mplus script.
#' @keywords MNLFA
#' @export
#' @examples
#' write.mplus.script()


write.inp.file <- function(df, outfile) {
  #checking inputs
  if (!is.data.frame(df))
    stop("df argument needs to be a data.frame")
  if (!is.character(outfile))
    stop("outfile argument needs to be a character vector")
  # check if the outfile already exists, if it does clear it.
  if (file.exists(outfile)) file.remove(outfile)
  #replace missing values with ""
  df[is.na(df[,1]),1] <- ""
  
  # next we iteratively write the .inp file from our input dataframe.
  for (i in seq(nrow(df))){
    # if number of characters is <= 90, write line and move to next loop
    if (nchar(df[i,1])<=90) {
      cat(df[i,1],
          sep = "\n",
          file = outfile,
          append = TRUE)
      # if characters between 90 and 160 we break it into two lines, at a space
      # note this can easily be extended to more than 160, if this solution works
    } else {
      # identify space closest to character limit 90
      # first this locates all spaces in the current line
      sp <- as.matrix(stringr::str_locate_all(df[i,1], " ")[[1]])
      # then we select only spaces prior to the 90th character
      sp <- sp[sp[,1]<=90,1]
      # select the space closest to character limit
      sp <- sp[length(sp)]
      # cut vector at that space, and write as two lines
      cat(
        # write first line from 1 to the space "sp"
        substr(df[i,1], 1, sp),
        # write the second line from "sp" to the length of the vector
        substr(df[i,1], sp, nchar(df[i,1])),
        sep = "\n",
        file = outfile,
        append = TRUE)
    }
  }
}