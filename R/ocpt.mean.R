ocpt.mean.initialise=function(data,penalty="MBIC",pen.value=0,method="PELT",Q=5,test.stat="Normal",class=TRUE,param.estimates=TRUE,shape=1,minseglen=1){
    checkData(data)
  if(method=="SegNeigh" & minseglen>1){stop("minseglen not yet implemented for SegNeigh method, use PELT instead.")}
  if(minseglen<1){minseglen=1;warning('Minimum segment length for a change in mean is 1, automatically changed to be 1.')}
  #if(!((test.stat=="Normal")||(test.stat=="CUSUM"))){ stop("Invalid test statistic, must be Normal or CUSUM") #}
  if(penalty == "CROPS"){
    stop('Use cpt.mean from changepoint as CROPS is not available with changepoint.online')
  }
  if(penalty =="MBIC"){
      if(test.stat=="Normal"){
      costfunc="mean.norm.mbic"
  } else if(test.stat=="Exponential"){
      costfunc="meanvar.exp.mbic"
  } else if(test.stat=="Gamma"){
      costfunc="meanvar.gamma.mbic"
  }else if(test.stat=="Poisson"){
    costfunc="meanvar.poisson.mbic"
  }
  }else{
      if(test.stat=="Normal"){
          costfunc="mean.norm"
      } else if(test.stat=="Exponential"){
          costfunc="meanvar.exp"
      } else if(test.stat=="Gamma"){
          costfunc="meanvar.gamma"
      }else if(test.stat=="Poisson"){
          costfunc="meanvar.poisson"
      }else{
          stop("Not a valid test statistic. Must be Normal, Exponential, Gamma or Poisson")
      }
  }
  
      if(((method=="AMOC")||(method=="PELT")||(method=="BinSeg")||(method=="Segeigh"))){
          
          mu=mean(data)
          sumstat=cbind(c(0,cumsum(coredata(data))),c(0,cumsum(coredata(data)^2)),cumsum(c(0,(coredata(data)-mu)^2)))
          
          pen.value = penalty_decision(penalty, pen.value, length(data), diffparam=1, asymcheck=costfunc, method="AMOC")
          
          ans=PELT.online.initialise(sumstat,pen=pen.value,cost_func = costfunc, shape = shape, minseglen = minseglen)
          
          return(online.class_input(data, cpttype="mean", method=method, test.stat=test.stat, penalty=penalty, pen.value=ans$penalty, minseglen=minseglen, param.estimates=param.estimates, out=c(0, ans$cptsout),lastchangelike=ans$lastchangelike,lastchangecpts=ans$lastchangecpts,numchangecpts=ans$numchangecpts,checklist=ans$checklist,ndone=ans$ndone,nupdate=ans$nupdate))
      }
      else{
          stop("Invalid Method, must be AMOC, PELT, SegNeigh or BinSeg")
      }
}

ocpt.mean.initialize=function(data,penalty="MBIC",pen.value=0,method="AMOC",Q=5,test.stat="Normal",class=TRUE,param.estimates=TRUE,minseglen=1){
return(ocpt.mean.initialise(data,penalty,pen.value,method,Q,test.stat,class,param.estimates,minseglen))

}

opt.mean.update=function(){
    "Not done yet"
}


