#' Write fasta from dataframe
#' 
#' Adapated Nicholas Hathaway (https://bootstrappers.umassmed.edu/guides/main/r_writeFasta.html)
#' Function to write a dataframe of sequences to a fasta
#' @param data dataframe containing two columns: ORF and seq
#' @param filename file to write fasta to
#' @examples
#' writeFasta(df %>% select(ORF,seq),filename);
#' @return returns the number of fasta files written
#' @export

writeFasta <- function(data, filename){
  fastaLines = c()
  for (rowNum in 1:nrow(data)){
    fastaLines = c(fastaLines, as.character(paste(">", data[rowNum,"ORF"], sep = "")))
    fastaLines = c(fastaLines,as.character(data[rowNum,"seq"]))
  }
  fileConn<-file(filename)
  writeLines(fastaLines, fileConn)
  close(fileConn)
  return(length(fastaLines)/2)
}
