online.class_input <- function(data, ocpttype, method, test.stat, penalty, pen.value, minseglen, param.estimates, out=list(), Q=NA, shape=NA){
  if(method=="BinSeg" || method=="SegNeigh" || penalty=="CROPS"){
    ans=new("ocpt.range")
  }else{
    ans=new("ocpt")
  }
  
  data.set(ans)=data;ocpttype(ans)=ocpttype;method(ans)=method; test.stat(ans)=test.stat;pen.type(ans)=penalty;pen.value(ans)=pen.value;minseglen(ans)=minseglen;
  if(penalty!="CROPS"){ # crops is only one that doesn't give a single set of cpts
    ocpts(ans)=out[[2]]
    
    if(param.estimates==TRUE){
      if(test.stat == "Gamma"){
      ans=param(ans, shape)
      }else{
      ans=param(ans)
      }
    }
  }
  
  if(method=="PELT"){
      nocpts.max(ans)=Inf
  }
  else if(method=="AMOC"){
    nocpts.max(ans)=1
  }
  else{
    nocpts.max(ans)=Q
  }
  
  if(method=="BinSeg"){
    l=list()
    for(i in 1:(length(out$cps)/2)){
      l[[i]] = out$cps[1,1:i] 
    }
    m = t(sapply(l, '[', 1:max(sapply(l, length))))
    
    ocpts.full(ans)=m
    pen.value.full(ans)=out$cps[2,]
  }else if(method=="SegNeigh"){
    ocpts.full(ans)=out$cps[-1,]
    pen.value.full(ans)=-diff(out$like.Q)
  }else if(penalty=="CROPS"){
    m = t(sapply(out[[2]], '[', 1:max(sapply(out[[2]], length))))
    
    ocpts.full(ans) = m
    pen.value.full(ans) = out[[1]][1,]
    if(test.stat=="Gamma"){param.est(ans)$shape=shape}
  }
  
  return(ans)
}
