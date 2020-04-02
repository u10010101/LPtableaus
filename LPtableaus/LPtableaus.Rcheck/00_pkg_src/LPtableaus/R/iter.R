#'@title simplex.iter <- function(LPtable,type,Bland=F)
#'@description main functions:LPSolver,LP.Tableaus,Simplex.dual,table2tex.Have fun!
simplex.iter <- function(LPtable,type,Bland=F){
  c=LPtable[!names(LPtable)%in%c("rhs","BV","z")][1,]
  if (type=="max"){
    if(Bland==0){
      num=which(c==min(c))[1]#
    }else{
      num=which(c<0)[1]
    }
  }
  else{
    if(Bland==0){
      num=which(c==max(c))[1]
    }
    else{
      num=which(c>0)[1]
    }
  }
  num=num+1
  line=which(LPtable[-1,num]>0)+1#????ֵ???Ե??е??±꼯
  if(length(line)==0){#?޽?LP
    return(list(flag=0,new=names(LPtable)[num]))
  }
  a=LPtable$rhs[line]#??????
  b=LPtable[line,num]#????
  d=line[which(a/b==min(a/b))][1]#???????е??±?
  LPtable$BV=as.character(LPtable$BV)
  oldbase=LPtable$BV[d]
  LPtable$BV[d]=names(LPtable)[num]
  #ERO
  LPtable=simplex.ERO(LPtable,d,num)
  return(list(Tableaus=LPtable,new=names(LPtable)[num],out=oldbase,flag=1))
}
