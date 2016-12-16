availableCpus <- function() {
 output <- system("wc -l $PBS_NODEFILE",intern=T)
 tokens <- strsplit(output, " ")[[1]]
 return(tokens[1])
}