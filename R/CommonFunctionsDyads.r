# From jaspMachineLearning:
cleanSequence <- function(sequence) {
  sequence <- gsub(",", "\n", sequence)
  sequence <- gsub(";", "\n", sequence)
  sequence <- unlist(strsplit(sequence, split = "\n"))
  sequence <- trimws(sequence, which = c("both"))
  sequence <- sequence[sequence != ""]
  return(sequence)
}