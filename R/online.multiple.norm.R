online.segneigh.var.norm=function(data,Q=5,pen=0,know.mean=FALSE,mu=NA){
  n=length(data)
  if(n<4){stop('Data must have atleast 4 observations to fit a changepoint model.')}
  
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  if((know.mean==FALSE)&(is.na(mu))){
    mu=mean(data)
  }
  all.seg=matrix(0,ncol=n,nrow=n)
  for(i in 1:n){
    ssq=0
    for(j in i:n){
      m=j-i+1
      ssq=ssq+(data[j]-mu)^2
      if(ssq<=0){sigmasq=0.00000000001/m}
      else{sigmasq=ssq/m}
      all.seg[i,j]=-(m/2)*(log(2*pi)+log(sigmasq)+1)
    }
  }
  like.Q=matrix(0,ncol=n,nrow=Q)
  like.Q[1,]=all.seg[1,]
  cp=matrix(NA,ncol=n,nrow=Q)
  for(q in 2:Q){
    for(j in q:n){
      like=NULL
      if((j-2-q)<0){
        like=-Inf
      }
      else{
        v=(q):(j-2)
        like=like.Q[q-1,v]+all.seg[v+1,j]
      }
      
      like.Q[q,j]= max(like,na.rm=TRUE)
      cp[q,j]=which(like==max(like,na.rm=TRUE))[1]+(q-1)
    }
  }
  cps.Q=matrix(NA,ncol=Q,nrow=Q)
  for(q in 2:Q){
    cps.Q[q,1]=cp[q,n]
    for(i in 1:(q-1)){
      cps.Q[q,(i+1)]=cp[(q-i),cps.Q[q,i]]
    }
  }
  
  op.cps=NULL
  k=0:(Q-1)
  
  for(i in 1:length(pen)){
    criterion=-2*like.Q[,n]+k*pen[i]
    
    op.cps=c(op.cps,which(criterion==min(criterion,na.rm=T))-1)
  }
  if(op.cps==(Q-1)){warning('The number of segments identified is Q, it is advised to increase Q to make sure changepoints have not been missed.')}
  
  if(op.cps==0){ocpts=n}
  else{ocpts=c(sort(cps.Q[op.cps+1,][cps.Q[op.cps+1,]>0]),n)}
  
  return(list(cps=t(apply(cps.Q,1,sort,na.last=TRUE)),ocpts=ocpts,op.ocpts=op.cps,pen=pen,like=criterion[op.cps+1],like.Q=-2*like.Q[,n]))
}


online.segneigh.mean.norm=function(data,Q=5,pen=0){
  n=length(data)
  if(n<2){stop('Data must have atleast 2 observations to fit a changepoint model.')}
  
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  all.seg=matrix(0,ncol=n,nrow=n)
  for(i in 1:n){
    ssq=0
    sumx=0
    for(j in i:n){
      len=j-i+1
      sumx=sumx+data[j]
      ssq=ssq+data[j]^2
      all.seg[i,j]=-0.5*(ssq-(sumx^2)/len)
    }
  }
  like.Q=matrix(0,ncol=n,nrow=Q)
  like.Q[1,]=all.seg[1,]
  cp=matrix(NA,ncol=n,nrow=Q)
  for(q in 2:Q){
    for(j in q:n){
      like=NULL
      v=(q-1):(j-1)
      like=like.Q[q-1,v]+all.seg[v+1,j]
      
      like.Q[q,j]= max(like,na.rm=TRUE)
      cp[q,j]=which(like==max(like,na.rm=TRUE))[1]+(q-2)
    }
  }
  cps.Q=matrix(NA,ncol=Q,nrow=Q)
  for(q in 2:Q){
    cps.Q[q,1]=cp[q,n]
    for(i in 1:(q-1)){
      cps.Q[q,(i+1)]=cp[(q-i),cps.Q[q,i]]
    }
  }
  
  op.cps=NULL
  k=0:(Q-1)
  
  for(i in 1:length(pen)){
    criterion=-2*like.Q[,n]+k*pen[i]
    
    op.cps=c(op.cps,which(criterion==min(criterion,na.rm=T))-1)
  }
  if(op.cps==(Q-1)){warning('The number of segments identified is Q, it is advised to increase Q to make sure changepoints have not been missed.')}
  if(op.cps==0){ocpts=n}
  else{ocpts=c(sort(cps.Q[op.cps+1,][cps.Q[op.cps+1,]>0]),n)}
  
  return(list(cps=t(apply(cps.Q,1,sort,na.last=TRUE)),ocpts=ocpts,op.ocpts=op.cps,pen=pen,like=criterion[op.cps+1],like.Q=-2*like.Q[,n]))
}


online.segneigh.meanvar.norm=function(data,Q=5,pen=0){
  n=length(data)
  if(n<4){stop('Data must have atleast 4 observations to fit a changepoint model.')}
  
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  all.seg=matrix(0,ncol=n,nrow=n)
  for(i in 1:n){
    ssq=0
    sumx=0
    for(j in i:n){
      len=j-i+1
      sumx=sumx+data[j]
      ssq=ssq+data[j]^2
      sigmasq=(1/len)*(ssq-(sumx^2)/len)
      if(sigmasq<=0){sigmasq=0.00000000001}
      all.seg[i,j]=-(len/2)*(log(2*pi)+log(sigmasq)+1)
    }
  }
  like.Q=matrix(0,ncol=n,nrow=Q)
  like.Q[1,]=all.seg[1,]
  cp=matrix(NA,ncol=n,nrow=Q)
  for(q in 2:Q){
    for(j in q:n){
      like=NULL
      if((j-2-q)<0){
        like=-Inf
      }
      else{
        v=(q):(j-2)
        like=like.Q[q-1,v]+all.seg[v+1,j]
      }
      
      like.Q[q,j]= max(like,na.rm=TRUE)
      cp[q,j]=which(like==max(like,na.rm=TRUE))[1]+(q-1)
    }
    
  }
  cps.Q=matrix(NA,ncol=Q,nrow=Q)
  for(q in 2:Q){
    cps.Q[q,1]=cp[q,n]
    for(i in 1:(q-1)){
      cps.Q[q,(i+1)]=cp[(q-i),cps.Q[q,i]]
    }
  }
  
  op.cps=NULL
  k=0:(Q-1)
  
  for(i in 1:length(pen)){
    criterion=-2*like.Q[,n]+k*pen[i]
    
    op.cps=c(op.cps,which(criterion==min(criterion,na.rm=T))-1)
  }
  if(op.cps==(Q-1)){warning('The number of segments identified is Q, it is advised to increase Q to make sure changepoints have not been missed.')}
  
  if(op.cps==0){ocpts=n}
  else{ocpts=c(sort(cps.Q[op.cps+1,][cps.Q[op.cps+1,]>0]),n)}
  
  
  return(list(cps=t(apply(cps.Q,1,sort,na.last=TRUE)),ocpts=ocpts,op.ocpts=op.cps,pen=pen,like=criterion[op.cps+1],like.Q=-2*like.Q[,n]))
}



online.multiple.var.norm=function(data,mul.method="PELT",penalty="MBIC",pen.value=0,Q=5,know.mean=FALSE,mu=NA,class=TRUE,param.estimates=TRUE, minseglen=2){
  if(!((mul.method=="PELT")||(mul.method=="BinSeg")||(mul.method=="SegNeigh"))){
    stop("Multiple Method is not recognised")
  }
  costfunc = "var.norm"
  if(penalty =="MBIC"){
    if(mul.method=="SegNeigh"){
      stop('MBIC penalty not implemented for SegNeigh method, please choose an alternative penalty')
    }
    costfunc = "var.norm.mbic"
  }
  diffparam=1
  if(is.null(dim(data))==TRUE){
    # single dataset
    n=length(data)
    mu=mu[1]
  }
  else{
    n=ncol(data)
  }
  if(n<4){stop('Data must have atleast 4 observations to fit a changepoint model.')}
  if(n<(2*minseglen)){stop('Minimum segment legnth is too large to include a change in this data')}
  
  pen.value = penalty_decision(penalty, pen.value, n, diffparam, asymcheck=costfunc, method=mul.method)

    if(is.null(dim(data))==TRUE){
    # single dataset
    if((know.mean==FALSE)&(is.na(mu))){
      mu=mean(coredata(data))
    }
    out = online.data_input(data=data,method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q,var=mu)
    
    if(class==TRUE){
      out=online.class_input(data, ocpttype="variance", method=mul.method, test.stat="Normal", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out, Q=Q)
      param.est(out)=c(param.est(out),mean=mu)
      return(out)
    }
    else{ return(out[[2]])}
  }
  else{
    rep=nrow(data)
    out=list()
    if(length(mu)!=rep){
      mu=rep(mu,rep)
    }
    
    for(i in 1:rep){
      if((know.mean==FALSE)&(is.na(mu[i]))){
        mu=mean(coredata(data[i,]))
      }
      out[[i]]=online.data_input(data[i,],method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q,var=mu)
    }
    
      ocpts=lapply(out, '[[', 2)
        
    if(class==TRUE){
      ans=list()
      for(i in 1:rep){
        ans[[i]]=online.class_input(data[i,], ocpttype="variance", method=mul.method, test.stat="Normal", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out[[i]], Q=Q)
        param.est(ans[[i]])=c(param.est(ans[[i]]),mean=mu[i])
      }
      return(ans)
    }
    else{return(ocpts)}
  }
}


online.multiple.mean.norm=function(data,mul.method="PELT",penalty="MBIC",pen.value=0,Q=5,class=TRUE,param.estimates=TRUE,minseglen){
  if(!((mul.method=="PELT")||(mul.method=="BinSeg")||(mul.method=="SegNeigh"))){
    stop("Multiple Method is not recognised")
  }
  costfunc = "mean.norm"
  if(penalty=="MBIC"){
    if(mul.method=="SegNeigh"){
      stop('MBIC penalty not implemented for SegNeigh method, please choose an alternative penalty')
    }
    costfunc = "mean.norm.mbic"
  }
  diffparam=1
  if(is.null(dim(data))==TRUE){
    # single dataset
    n=length(data) # still works if data is of class ts
  }
  else{
    n=ncol(data)
  }
  if(n<(2*minseglen)){stop('Minimum segment length is too large to include a change in this data')}
  
  pen.value = penalty_decision(penalty, pen.value, n, diffparam, asymcheck = costfunc, method=mul.method)

    if(is.null(dim(data))==TRUE){
    # single dataset
    out = online.data_input(data=data,method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q)
     
    if(class==TRUE){
      return(online.class_input(data, ocpttype="mean", method=mul.method, test.stat="Normal", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out, Q=Q))
    }
    else{ return(out[[2]])}
  }
  else{
    rep=nrow(data)
    out=list()
    if(class==TRUE){ocpts=list()}
    for(i in 1:rep){
      out[[i]]=online.data_input(data[i,],method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q)
    }
    
    cps=lapply(out, '[[', 2)
    
    if(class==TRUE){
      ans=list()
      for(i in 1:rep){
        ans[[i]]=online.class_input(data[i,], ocpttype="mean", method=mul.method, test.stat="Normal", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out[[i]], Q=Q)
      }
      return(ans)
    }
    else{return(cps)}
  }
}

online.multiple.meanvar.norm=function(data,mul.method="PELT",penalty="MBIC",pen.value=0,Q=5,class=TRUE,param.estimates=TRUE,minseglen){
  if(!((mul.method=="PELT")||(mul.method=="BinSeg")||(mul.method=="SegNeigh"))){
    stop("Multiple Method is not recognised")
  }
  costfunc = "meanvar.norm"
  if(penalty=="MBIC"){
    if(mul.method=="SegNeigh"){
      stop('MBIC penalty not implemented for SegNeigh method, please choose an alternative penalty')
    }
    costfunc = "meanvar.norm.mbic"
  }
  diffparam=2
  if(is.null(dim(data))==TRUE){
    # single dataset
    n=length(data)
  }
  else{
    n=ncol(data)
  }
  if(n<(2*minseglen)){stop('Minimum segment legnth is too large to include a change in this data')}
  
  pen.value = penalty_decision(penalty, pen.value, n, diffparam, asymcheck = costfunc, method=mul.method)

    if(is.null(dim(data))==TRUE){
    # single dataset
    out = online.data_input(data=data,method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q)

    if(class==TRUE){
      return(online.class_input(data, ocpttype="mean and variance", method=mul.method, test.stat="Normal", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out, Q=Q))
    }
    else{ return(out[[2]])}
  }
  else{
    rep=nrow(data)
    out=list()
    for(i in 1:rep){
      out[[i]]=online.data_input(data[i,],method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q)
    }
  
    cps=lapply(out, '[[', 2)
  
    if(class==TRUE){
      ans=list()
      for(i in 1:rep){
        ans[[i]] = online.class_input(data[i,], ocpttype="mean and variance", method=mul.method, test.stat="Normal", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out[[i]], Q=Q)
      }
      return(ans)
    }
    else{return(cps)}
  }
}
