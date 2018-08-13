ocpt.meanvar.initialise=function(data,penalty="MBIC",pen.value=0,Q=5,test.stat="Normal",class=TRUE,param.estimates=TRUE,shape=1,minseglen=2,alpha=1,verbose=FALSE){
    checkData(data)
    if(test.stat=="ECP"){
        ecpans = e.cp3o_delta.online.initialise(Z=data, K=Q, delta=minseglen+1, alpha=alpha,verbose=verbose)
        return(ecpans)
    }
  if(minseglen<2){minseglen=2;warning('Minimum segment length for a change in mean and variance is 2, automatically changed to be 2.')}
  #if(!((test.stat=="Normal")||(test.stat=="CUSUM"))){ stop("Invalid test statistic, must be Normal or CUSUM") #}
  if(penalty == "CROPS"){
    stop('Use cpt.meanvar from changepoint as CROPS is not available with changepoint.online')
  }
  if(penalty =="MBIC"){
      if(test.stat=="Normal"){
      costfunc="meanvar.norm.mbic"
  } else if(test.stat=="Exponential"){
      costfunc="meanvar.exp.mbic"
  } else if(test.stat=="Gamma"){
      costfunc="meanvar.gamma.mbic"
  }else if(test.stat=="Poisson"){
    costfunc="meanvar.poisson.mbic"
  }
  }else{
      if(test.stat=="Normal"){
          costfunc="meanvar.norm"
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
          
          mu=mean(data)
          sumstat=cbind(c(0,cumsum(coredata(data))),c(0,cumsum(coredata(data)^2)),cumsum(c(0,(coredata(data)-mu)^2)))
          
          pen.value = penalty_decision(penalty, pen.value, length(data), diffparam=1, asymcheck=costfunc, method="AMOC")
          method = "PELT"
          ans=PELT.online.initialise(sumstat=sumstat,pen=pen.value,cost_func = costfunc, shape = shape, minseglen = minseglen)
          
          return(online.class_input(sumstat=sumstat, cpttype="mean and variance", method=method, test.stat=test.stat, penalty=penalty, pen.value=ans$penalty, minseglen=minseglen, param.estimates=param.estimates, out=sort(ans$cptsout[ans$cptsout>0]),shape=ans$shape,Q=Q,lastchangelike=ans$lastchangelike,lastchangecpts=ans$lastchangecpts,checklist=ans$checklist,ndone=ans$ndone,nupdate=ans$nupdate,cost_func=ans$cost_func))
}

ocpt.meanvar.initialize=function(data,penalty="MBIC",pen.value=0,Q=5,test.stat="Normal",class=TRUE,param.estimates=TRUE,shape=1,minseglen=2,alpha=1,verbose=FALSE){
return(ocpt.meanvar.initialise(data,penalty,pen.value,Q,test.stat,class,param.estimates,shape,minseglen,alpha,verbose))

}

ocpt.meanvar.update=function(previousanswer,newdata){
    
    checkData(newdata)
    if(class(previousanswer)=="ecp.ocpt"){
        ecpans = e.cp3o_delta.online.update(previousanswer,newdata,K=2*(previousanswer@number))
        return(ecpans)
    }
    if(class(previousanswer@param.est) == "list"){
        param.estimates = TRUE
    }else{
        param.estimates = FALSE
    }
    
    nextans = PELT.online.update(previousanswer=previousanswer,newdata=newdata)
    
    return(online.class_input(sumstat=nextans$sumstat, cpttype="mean and variance", method=previousanswer@method, test.stat=previousanswer@test.stat, penalty=previousanswer@pen.type, pen.value=nextans$penalty, minseglen=nextans$minseglen, param.estimates=param.estimates, out=sort(nextans$cptsout[nextans$cptsout>0]),shape = previousanswer@shape, lastchangelike=nextans$lastchangelike,lastchangecpts=nextans$lastchangecpts,checklist=nextans$checklist,ndone=nextans$ndone,nupdate=nextans$nupdate,cost_func=previousanswer@cost_func))
    
}

