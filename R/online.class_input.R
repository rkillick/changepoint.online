online.class_input <- function(sumstat, cpttype, method, test.stat, penalty, pen.value, minseglen, param.estimates, out=list(), Q=NA, shape=NA, lastchangelike=c(0), lastchangecpts=c(0), checklist=c(0), nchecklist=0, ndone=0, nupdate=length(data),cost_func){
  if(method=="BinSeg" || method=="SegNeigh" || penalty=="CROPS"){
    ans=new("ocpt.range")
  }else{
    ans=new("ocpt")
  }
  
  sumstat(ans)=sumstat;cpttype(ans)=cpttype;method(ans)=method; test.stat(ans)=test.stat;pen.type(ans)=penalty;pen.value(ans)=pen.value;minseglen(ans)=minseglen;ndone(ans)=ndone;nupdate(ans)=nupdate;lastchangelike(ans)=lastchangelike;lastchangecpts(ans)=lastchangecpts;checklist(ans)=checklist;ans@nchecklist=nchecklist;shape(ans)=shape;cost_func(ans)=cost_func;
  if(penalty!="CROPS"){ # crops is only one that doesn't give a single set of cpts
    cpts(ans)=out
    
    if(param.estimates==TRUE){
      if(test.stat == "Gamma"){
      ans=param(ans, shape)
      }else{
      ans=param(ans)
      }
    }
  }
  
  if(method=="PELT"){
      ncpts.max(ans)=Inf
  }
  else if(method=="AMOC"){
    ncpts.max(ans)=1
  }
  else{
    ncpts.max(ans)=Q
  }
  
  if(method=="BinSeg"){
    l=list()
    for(i in 1:(length(out$cps)/2)){
      l[[i]] = out$cps[1,1:i] 
    }
    m = t(sapply(l, '[', 1:max(sapply(l, length))))
    
    cpts.full(ans)=m
    pen.value.full(ans)=out$cps[2,]
  }else if(method=="SegNeigh"){
    cpts.full(ans)=out$cps[-1,]
    pen.value.full(ans)=-diff(out$like.Q)
  }else if(penalty=="CROPS"){
    m = t(sapply(out[[2]], '[', 1:max(sapply(out[[2]], length))))
    
    cpts.full(ans) = m
    pen.value.full(ans) = out[[1]][1,]
    if(test.stat=="Gamma"){param.est(ans)$shape=shape}
  }
  
  return(ans)
}

online.ecp.class_input <- function(number, estimates, GofM, delta, alpha, verbose, csum, dll, dlr, drr, left, right, datalength, functime, width, cpLoc){
    
    ans = new("ecp.ocpt")
    
    number(ans)=number; estimates(ans)=estimates; GofM(ans)=GofM; delta(ans)=delta; alpha(ans)=alpha; verbose(ans)=verbose; csum(ans)=csum; dll(ans)=dll; dlr(ans)=dlr; drr(ans)=drr; left(ans)=left; right(ans)=right; datalength(ans)=datalength; functime(ans)=functime; width(ans)=width; cpLoc(ans)=cpLoc;
    return(ans)
}

