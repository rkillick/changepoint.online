online.segneigh.var.css=function(data,Q=5,pen=0){
  n=length(data)
  if(n<4){stop('Data must have at least 4 observations to fit a changepoint model.')}
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  
  y2=c(0,cumsum(data^2))
  oldmax=1000
  
  test=NULL
  like.Q=matrix(0,ncol=n,nrow=Q)
  cp=matrix(NA,ncol=n,nrow=Q)
  for(q in 2:Q){ # no of segments
    for(j in q:n){
      like=NULL
      v=(q-1):(j-1)
      if(q==2){
        like=abs(sqrt(j/2)*(y2[v+1]/y2[j+1] -v/j))
      }
      else{
        like=like.Q[q-1,v]+abs(sqrt((j-cp[q-1,v])/2)*((y2[v+1]-y2[cp[q-1,v]+1])/(y2[j+1]-y2[cp[q-1,v]+1]) -(v-cp[q-1,v])/(j-cp[q-1,v])))
      }
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
  
  op.cps=0
  flag=0
  for(q in 2:Q){
    criterion=NULL
    ocpttmp=c(0,sort(cps.Q[q,1:(q-1)]),n)
    for(i in 1:(q-1)){
      criterion[i]=abs(sqrt((ocpttmp[i+2]-ocpttmp[i])/2)*((y2[ocpttmp[i+1]+1]-y2[ocpttmp[i]+1])/(y2[ocpttmp[i+2]+1]-y2[ocpttmp[i]+1]) -(ocpttmp[i+1]-ocpttmp[i])/(ocpttmp[i+2]-ocpttmp[i])))
      if(criterion[i]<pen){flag=1}
    }
    if(flag==1){
      break
    }
    op.cps=op.cps+1
  }
  if(op.cps==(Q-1)){warning('The number of segments identified is Q, it is advised to increase Q to make sure changepoints have not been missed.')}
  if(op.cps==0){ocpts=n}
  else{ocpts=c(sort(cps.Q[op.cps+1,][cps.Q[op.cps+1,]>0]),n)}
  
  return(list(cps=t(apply(cps.Q,1,sort,na.last=TRUE)),ocpts=ocpts,op.ocpts=op.cps,pen=pen,like=criterion[op.cps+1],like.Q=like.Q[,n]))
}



online.binseg.var.css=function(data,Q=5,pen=0,minseglen=2){
  n=length(data)
  if(n<4){stop('Data must have atleast 4 observations to fit a changepoint model.')}
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  
  y2=c(0,cumsum(data^2))
  tau=c(0,n)
  ocpt=matrix(0,nrow=2,ncol=Q)
  oldmax=Inf
  
  for(q in 1:Q){
    lambda=rep(0,n-1)
    i=1
    st=tau[1]+1;end=tau[2]
    for(j in 1:(n-1)){
      if(j==end){
        st=end+1;i=i+1;end=tau[i+1]
      }else{
        if(((j-st)>=minseglen)&((end-j)>=minseglen)){
          lambda[j]=sqrt((end-st+1)/2)*((y2[j+1]-y2[st])/(y2[end+1]-y2[st]) -(j-st+1)/(end-st+1))
        }
      }
    }
    k=which.max(abs(lambda))
    ocpt[1,q]=k;ocpt[2,q]=min(oldmax,max(abs(lambda),na.rm=T))
    oldmax=min(oldmax,max(abs(lambda),na.rm=T))
    tau=sort(c(tau,k))
  }
  op.cps=NULL
  p=1:(Q-1)
  for(i in 1:length(pen)){
    criterion=(ocpt[2,])>=pen[i]
    if(sum(criterion)==0){
      op.cps=0
    }
    else{
      op.cps=c(op.cps,max(which((criterion)==TRUE)))
    }
  }
  if(op.cps==Q){warning('The number of changepoints identified is Q, it is advised to increase Q to make sure changepoints have not been missed.')}
  
  if(op.cps==0){ocpts=n}
  else{ocpts=c(sort(ocpt[1,1:op.cps]),n)}
  
  return(list(cps=ocpt,cpts=cpts,op.ocpts=op.cps,pen=pen))
}


online.multiple.var.css=function(data,mul.method="BinSeg",penalty="MBIC",pen.value=0,Q=5,class=TRUE,param.estimates=TRUE,minseglen){
  if(mul.method=="PELT"){ stop("CSS does not satisfy the assumptions of PELT, use SegNeigh or BinSeg instead.") }
  else if(!((mul.method=="BinSeg")||(mul.method=="SegNeigh"))){
    stop("Multiple Method is not recognised")
  }
  if(penalty!="MBIC"){
    costfunc = "var.css"
  }else{
    stop("MBIC penalty is not valid for nonparametric test statistics.")
  }
  diffparam=1
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
    if(mul.method=="BinSeg"){
      			out=online.binseg.var.css(data,Q,pen.value)
    }
    else if(mul.method=="SegNeigh"){
      out=online.segneigh.var.css(data,Q,pen.value)
    }
    if(class==TRUE){
      return(online.class_input(data, ocpttype="variance", method=mul.method, test.stat="CSS", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out, Q=Q))
    }
    else{ return(out)}
  }
  else{
    rep=nrow(data)
    out=list()
    if(class==TRUE){ocpts=list()}
    if(mul.method=="BinSeg"){
      for(i in 1:rep){
  			out=c(out,list(online.binseg.var.css(data[i,],Q,pen.value)))
      }
      if(class==TRUE){ocpts=out}
    }
    else if(mul.method=="SegNeigh"){
      for(i in 1:rep){
        out=c(out,list(online.segneigh.var.css(data[i,],Q,pen.value)))
      }
    }
    if(class==TRUE){
      ans=list()
      for(i in 1:rep){
        ans[[i]]=online.class_input(data[i,], ocpttype="variance", method=mul.method, test.stat="CSS", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out[[i]], Q=Q)
      }
      return(ans)
    }
    else{return(out)}
  }
}






online.segneigh.mean.cusum=function(data,Q=5,pen=0){
  n=length(data)
  if(n<2){stop('Data must have at least 2 observations to fit a changepoint model.')}
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  
  y=c(0,cumsum(data))
  oldmax=1000
  
  test=NULL
  like.Q=matrix(0,ncol=n,nrow=Q)
  cp=matrix(NA,ncol=n,nrow=Q)
  for(q in 2:Q){ # no of segments
    for(j in q:n){
      like=NULL
      v=(q-1):(j-1)
      if(q==2){
        like=abs((y[v+1]-(v/j)*y[j+1])/j)
      }
      else{
        like=like.Q[q-1,v]+abs(((y[v+1]-y[cp[q-1,v]+1])-((v-cp[q-1,v])/(j-cp[q-1,v]))*(y[j+1]-y[cp[q-1,v]+1]))/(j-cp[q-1,v]))
      }
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
  
  op.cps=0
  flag=0
  for(q in 2:Q){
    criterion=NULL
    ocpttmp=c(0,sort(cps.Q[q,1:(q-1)]),n)
    for(i in 1:(q-1)){
      criterion[i]=abs(((y[ocpttmp[i+1]+1]-y[ocpttmp[i]+1])-((ocpttmp[i+1]-ocpttmp[i])/(ocpttmp[i+2]-ocpttmp[i]))*(y[ocpttmp[i+2]+1]-y[ocpttmp[i]+1]))/(ocpttmp[i+2]-ocpttmp[i]))
      if(criterion[i]<pen){flag=1}
    }
    if(flag==1){
      break
    }
    op.cps=op.cps+1
  }
  
  if(op.cps==(Q-1)){warning('The number of segments identified is Q, it is advised to increase Q to make sure changepoints have not been missed.')}
  
  if(op.cps==0){cpts=n}
  else{cpts=c(sort(cps.Q[op.cps+1,][cps.Q[op.cps+1,]>0]),n)}
  
  return(list(cps=t(apply(cps.Q,1,sort,na.last=TRUE)),ocpts=ocpts,op.ocpts=op.cps,pen=pen,like=criterion[op.cps+1],like.Q=like.Q[,n]))
}


online.binseg.mean.cusum=function(data,Q=5,pen=0,minseglen=1){
  n=length(data)
  if(n<2){stop('Data must have atleast 2 observations to fit a changepoint model.')}
  
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  
  y=c(0,cumsum(data))
  tau=c(0,n)
  ocpt=matrix(0,nrow=2,ncol=Q)
  oldmax=Inf
  
  for(q in 1:Q){
    lambda=rep(0,n-1)
    i=1
    st=tau[1]+1;end=tau[2]
    for(j in 1:(n-1)){
      if(j==end){
        st=end+1;i=i+1;end=tau[i+1]
      }else{
        if(((j-st)>=minseglen)&((end-j)>=minseglen)){
          lambda[j]=((y[j+1]-y[st])-((j-st+1)/(end-st+1))*(y[end+1]-y[st]))/(end-st+1)
        }
      }
    }
    k=which.max(abs(lambda))
    ocpt[1,q]=k;ocpt[2,q]=min(oldmax,max(abs(lambda)))
    oldmax=min(oldmax,max(abs(lambda)))
    tau=sort(c(tau,k))
  }
  op.cps=NULL
  p=1:(Q-1)
  for(i in 1:length(pen)){
    criterion=(ocpt[2,])>=pen[i]
    if(sum(criterion)==0){
      op.cps=0
    }
    else{
      op.cps=c(op.cps,max(which((criterion)==TRUE)))
    }
  }
  if(op.cps==Q){warning('The number of changepoints identified is Q, it is advised to increase Q to make sure changepoints have not been missed.')}
  
  if(op.cps==0){ocpts=n}
  else{ocpts=c(sort(ocpt[1,1:op.cps]),n)}
  
  return(list(cps=ocpt,ocpts=ocpts,op.cpts=op.cps,pen=pen))
}


online.multiple.mean.cusum=function(data,mul.method="BinSeg",penalty="Asymptotic",pen.value=0.05,Q=5,class=TRUE,param.estimates=TRUE,minseglen){
  
  if(mul.method=="PELT"){ stop("CUSUM does not satisfy the assumptions of PELT, use SegNeigh or BinSeg instead.") }
  else if(!((mul.method=="BinSeg")||(mul.method=="SegNeigh"))){
    stop("Multiple Method is not recognised")
  }
  if(penalty!="MBIC"){
    costfunc = "mean.cusum"
  }else{
    stop("MBIC penalty is not valid for nonparametric test statistics.")
  }
  diffparam=1
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
    if(mul.method=="BinSeg"){
			out=online.binseg.mean.cusum(coredata(data),Q,pen.value)
    }
    else if(mul.method=="SegNeigh"){
      out=online.segneigh.mean.cusum(coredata(data),Q,pen.value)
    }
    if(class==TRUE){
      return(online.class_input(data, ocpttype="mean", method=mul.method, test.stat="CUSUM", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out, Q=Q))
    }
    else{ return(out)}
  }
  else{
    rep=nrow(data)
    out=list()
    if(class==TRUE){cpts=list()}
    if(mul.method=="BinSeg"){
      for(i in 1:rep){
				out=c(out,list(online.binseg.mean.cusum(data[i,],Q,pen.value)))
      }
      if(class==TRUE){cpts=out}
    }
    else if(mul.method=="SegNeigh"){
      for(i in 1:rep){
        out=c(out,list(online.segneigh.mean.cusum(data[i,],Q,pen.value)))
      }
    }
    if(class==TRUE){
      ans=list()
      for(i in 1:rep){
        ans[[i]]=online.class_input(data[i,], ocpttype="mean", method=mul.method, test.stat="CUSUM", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out, Q=Q)
      }
      return(ans)
    }
    else{return(out)}
  }
}
