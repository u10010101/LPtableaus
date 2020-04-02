#'@title LP.Tableaus
#'@description If you know basic variables,you can get optimal tableu.
#'@param  varbase a vector
#'@return a dataframe
#'@export
LP.Tableaus <- function(dataframe1,varbase){
  if(typeof(varbase)=="character"){
    v=tolower(varbase)
    u=tolower(names(dataframe1))
    varbase=which(u==v[1])
    for(i in 2:length(v)){
      varbase=c(varbase,which(u==v[i]))}
  }
  name=names(dataframe1)
  maxcol=which(name=="BV")-1
  LP=dataframe1
  row=dim(LP)[1]
  CBV=LP[1,varbase]
  CBV=as.numeric(CBV)*(-1)
  B=LP[-1,varbase]
  B=solve(B)
  b=as.numeric(LP$rhs[-1])
  re=data.frame(z=c(1,seq(0,0,length=row-1)))
  for (i in 2:maxcol){
    if(i %in% varbase){
      re[1,i]=0
      ind=which(varbase==i)
      re[-1,i]=c(seq(0,0,length=ind-1),1,seq(0,0,length=row-ind-1))
    }else{
      re[1,i]=CBV%*%B%*%LP[-1,i]+LP[1,i]
      re[-1,i]=B%*%LP[-1,i]
    }
  }
  re[1,maxcol]=CBV%*%B%*%b
  re[-1,maxcol]=B%*%b
#re[,(maxcol+1):(dim(LP)[2])]=LP[,(maxcol+1):(dim(LP)[2])]
temp="z"
for(i in 1:length(varbase)){
  temp=c(temp,name[varbase[i]])
}
lastcol=data.frame(BV=temp)
re=cbind(re,lastcol)
  names(re)=name
  return(re)
}
