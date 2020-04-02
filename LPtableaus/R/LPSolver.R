#'@description Solve LP and show simplex tableaus!
#'@title LPSolver
#'@export
#'@examples LP=list("max",c(4,2,3,4,2),c(1,2,1,0,0,1,2,2,0,0,0,0,0,1,1),c(8,8,3),"<,<,<")
#'r=LPSolver(LP)
#'@param LP a list or a dataframe
#'@return a list containing much information
LPSolver <- function(LP,maxiter=20,min=F,Bland=F){
  BBB=Bland
  iternum=0
  opt=0
  pro=list()#process

  if(class(LP)=="list") {
    standard=Simplex.standard(LP)
    StandardForm=standard$data#标准形表
    tt=standard$type
    BV=standard$BV
    mm=standard$method[1,1]
  }else{
    if(class(LP)=="data.frame"){
      StandardForm=LP
      ttt=names(LP)
      mm="Simplex"
      for(i in 1:length(ttt)){
        if(substr(ttt[i],1,1)=="a"){
          mm="Big M"
          break
        }
      }
      tt="max"
      if(min==1){
        tt="min"
      }
      BV=LP[,dim(LP)[2],drop=F]
      names(BV)="BV"
    }else{
      return("not a list,not a dataframe")
    }
  }

  inittable=StandardForm
  #对标准形表进行ERO
  for (i in 2:dim(inittable)[1]){#如果这行基变量为a则ERO
    if(substr(as.character(inittable$BV)[i],1,1)=="a"){
      d=which(names(inittable)==as.character(inittable$BV)[i])
      if(i==2){
        inittable=simplex.ERO(StandardForm,i,d)}
      else{inittable=simplex.ERO(inittable,i,d)}
    }
  }
  method=mm
  opt=Simplex.isoptimal(inittable,tt)
  pro$Iter0=inittable
  if(opt==1){
    value=inittable$rhs
    value=data.frame(t(value))
    names(value)=as.character(BV[,1])
    re=list(x=value,s=data.frame(Status="OK",Itertimes=iternum,Method=method),standardForm=StandardForm,table=pro,BV=BV)
    return(re)
  }
  iternum=1
  LPnext=simplex.iter(inittable,tt,Bland = BBB)
  names(BV)="Iter0"
  if(LPnext$flag==0){#无界LP
    exitflag=paste0("Unbounded LP,when ",LPnext$new)
    re=list(s=data.frame(Status=exitflag,Itertimes=iternum,Method=method),standardForm=StandardForm,table=pro,BV=BV)
    return(re)
  }
  pro[[2]]=cbind(round(LPnext$Tableaus[,-dim(LPnext$Tableaus)[2]],8),BV=LPnext$Tableaus[,dim(LPnext$Tableaus)[2]])
  names(pro)[2]="Iter1"
  #基出进
  newbv=as.character(BV[,1])
  newbv[newbv==LPnext$out]=LPnext$new
  BV=cbind(BV,newbv)
  names(BV)[iternum+1]=paste0("Iter",iternum)


  #开始迭代
  while(Simplex.isoptimal(LPnext$Tableaus,tt)==0&iternum<maxiter){
    iternum=iternum+1
    LPnext=simplex.iter(LPnext$Tableaus,tt,Bland = BBB)
    if(LPnext$flag==0){#无界LP
      exitflag=paste0("Unbounded LP,when ",LPnext$new)
      re=list(s=data.frame(Status=exitflag,Itertimes=iternum,Method=method),standardForm=StandardForm,table=pro,BV=BV)
      return(re)
    }
    pro[[iternum+1]] <- cbind(round(LPnext$Tableaus[,-dim(LPnext$Tableaus)[2]],8),BV=LPnext$Tableaus[,dim(LPnext$Tableaus)[2]])
    names(pro)[iternum+1]=paste0("Iter",iternum)
    #基
    newbv=as.character(BV[,iternum])
    newbv[newbv==LPnext$out]=LPnext$new
    BV=cbind(BV,newbv)
    names(BV)[iternum+1]=paste0("Iter",iternum)
  }
  #得到最优表后
  value=round(LPnext$Tableaus$rhs,8)
  value=data.frame(t(value))
  names(value)=newbv#最优点和最优值
  exitflag="OK"
  for(i in 2:dim(value)[2]){
    if(substr(names(value)[i],1,1)=="a"&value[i]>0){
      exitflag="Infeasible LP,ai>0"
      break}
  }
  if(iternum==maxiter){
    exitflag="reached Maximum iterations"
  }
  re=list(x=value,s=data.frame(Status=exitflag,Method=method,Itertimes=iternum),standardForm=StandardForm,table=pro,BV=BV)
  return(re)
}
