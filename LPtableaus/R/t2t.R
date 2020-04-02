#'@title t2t
#'@description t2t
t2t <- function(frame){
  LP=frame
  n=dim(LP)[2]
  cat("\\begin{table}[h]\n\\begin{tabular}{|")
  for(i in 1:n){
    cat("c|")
  }
  cat("}\\hline\n")
  cat(names(LP)[1])
  for(i in 2:n){
    cat(" & ")
    cat(names(LP)[i])
  }
  cat("\\\\ \\hline\n")
  for(i in 1:dim(LP)[1]){
    cat(LP[i,1])
    for(j in 2:n){
      cat(" & ")
      cat(LP[i,j])
    }
    cat("\\\\ \\hline\n")
  }
  cat("\\end{tabular}\n\\end{table}\n")
}
