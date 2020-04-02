#'@title simplex.ERO
#'@description simplex.ERO
simplex.ERO <- function(frame,rownum,colnum){
  LPtable=frame
  d=rownum
  num=colnum
  baseline=LPtable[d,-length(LPtable[d,])]/LPtable[d,num]
  for (i in 1:dim(LPtable)[1]){
    LPtable[i,-length(LPtable[i,])]=LPtable[i,-length(LPtable[i,])]-baseline*(LPtable[i,num]/baseline[[num]])
  }
  LPtable[d,-length(LPtable[d,])]=baseline
  return(LPtable)
}
