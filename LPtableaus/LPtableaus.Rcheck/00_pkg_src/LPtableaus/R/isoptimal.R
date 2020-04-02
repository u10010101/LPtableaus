#' @title LPtableaus
#' @description main functions:LPSolver,LP.Tableaus,Simplex.dual,table2tex.Have fun!
#' @export
#' @return True/False
#' @param LPtable dataframe
Simplex.isoptimal <- function (LPtable,type="max"){
if(type=="max"){
  line <- min(LPtable[!names(LPtable)%in%c("rhs","BV","z")][1,])
  return(line>-1e-6)}
line <- max(LPtable[!names(LPtable)%in%c("rhs","BV","z")][1,])
return(line<1e-6)
}
