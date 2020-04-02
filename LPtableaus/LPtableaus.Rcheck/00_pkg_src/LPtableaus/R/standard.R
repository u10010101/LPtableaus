#'@title Simplex.standard <- function(orgin,M=NULL)
#'@description find the standard form of a LP
#'@export
#'@examples LP=list("max",c(4,2,3,4,2),c(1,2,1,0,0,1,2,2,0,0,0,0,0,1,1),c(8,8,3),"<,<,<")
#'r=Simplex.standard(LP)
#'@return list
Simplex.standard <- function(orgin,M=NULL){
  options(scipen=200)
  if(class(orgin)!="list"){
    print("not a list")
    return (NULL)
  }
  type=orgin[[1]]
  c=orgin[[2]]
  b=orgin[[4]]
  a=matrix(orgin[[3]],nrow=length(b),byrow=T)
  d=strsplit(orgin[[5]], split = ",")[[1]]
  signset=c("<",">")
  flag="Simplex"

  data=cbind(z=seq(0,0,length=length(b)),data.frame(a,stringsAsFactors = F))
  sn=0
  en=0
  an=0
  BVV=c("z")
  line0=c(1,-c)
  m=c(a,b,c)
  m=m[m>0]
  if(is.null(M)==1){
    M=max(abs(c(a,b,c)))/min(abs(m))
    M=max(100,10**(2+round(log10(M))))
  }
  for(i in 1:length(b)){
    if(b[i]<0){
      data[i,]=-data[i,]
      if(which(signset==d[i])>0){#?ı?????
        d[i]=signset[3-which(signset==d[i])]}
    }

    if(d[i]=="<"){
      data=cbind(data,t=c(seq(0,0,length=i-1),1,seq(0,0,length=length(b)-i)))
      sn=sn+1
      names(data)[length(names(data))]=paste0("s",sn)
      BVV=c(BVV,paste0("s",sn))
      line0=c(line0,0)
    }
    if(d[i]=="="){
      data=cbind(data,t=c(seq(0,0,length=i-1),1,seq(0,0,length=length(b)-i)))
      an=an+1
      names(data)[length(names(data))]=paste0("a",an)
      BVV=c(BVV,paste0("a",an))
      if(type=="max"){line0=c(line0,M)}
      else{line0=c(line0,-M)}
      flag="Big M"
    }
    if(d[i]==">"){#??e??a
      data=cbind(data,t=c(seq(0,0,length=i-1),-1,seq(0,0,length=length(b)-i)))
      en=en+1
      names(data)[length(names(data))]=paste0("e",en)
      data=cbind(data,t=c(seq(0,0,length=i-1),1,seq(0,0,length=length(b)-i)))
      an=an+1
      names(data)[length(names(data))]=paste0("a",an)
      BVV=c(BVV,paste0("a",an))
      if(type=="max"){line0=c(line0,0,M)}
      else{line0=c(line0,0,-M)}
      flag="Big M"
    }
  }
  #rhs
  data=cbind(data,rhs=abs(b))
  #??0??
  data=rbind(c(line0,0),data)
  #????��
  data=cbind(data,BV=BVV)
  row.names(data)=1:dim(data)[1]
  return(list(data=data,type=type,BV=data.frame(BV=BVV),method=data.frame(method=flag,M=M*(flag=="Big M"))))
}
