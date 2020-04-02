#'@title Simplex.dual
#'@description Find the standard form of the dual
#'@export
#'@param orgin a list
#'@return a datafame
Simplex.dual <- function(orgin,M=NULL){

  set=c("max","min")
  LP=orgin
  LP[[1]]=set[3-which(set==LP[[1]])]#??ֵ????
  c=orgin[[2]]
  b=orgin[[4]]#rhs
  a=matrix(orgin[[3]],nrow=length(b),byrow=T)
  d=strsplit(orgin[[5]], split = ",")[[1]]
  #?????Ⱥ?
  for(i in 1:length(d)){
    if(d[i]=="="){
      d[i]="<"
      a=rbind(a,a[i,])
      b=c(b,b[i])
      d=c(d,">")
    }
  }
  #?ѷ???Ū?þ???
  if(LP[[1]]=="max"){
    for(i in 1:length(d)){
      if(d[i]=="<"){
        b[i]=-b[i]
        a[i,]=-a[i,]
      }
    }
  }else{
    for(i in 1:length(d)){
      if(d[i]==">"){
        b[i]=-b[i]
        a[i,]=-a[i,]
      }
    }
  }
  LP[[2]]=b
  LP[[4]]=c
  if(LP[[1]]=="max"){#С??
    d="<"
    for(i in 2:length(LP[[4]])){
      d=paste0(d,",<")
    }
  }else{
    d=">"
    for(i in 2:length(LP[[4]])){
      d=paste0(d,",>")
    }
  }
  LP[[5]]=d
  LP[[3]]=as.vector(a)
  re=Simplex.standard(LP,M)
  return(re)
}
