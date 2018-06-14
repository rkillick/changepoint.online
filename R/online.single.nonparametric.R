online.single.var.css.calc <-
  function(data,extrainf=TRUE, minseglen){
    singledim=function(data,extrainf=TRUE,minseglen){
      n=length(data)
      y2=c(0,cumsum(data^2))
      taustar=minseglen:(n-minseglen+1)
      tmp=(y2[taustar+1]/y2[n+1])-taustar/n
      
      D=max(abs(tmp),na.rm=T)
      tau=which.max(abs(tmp))
      if(extrainf==TRUE){
        out=c(tau,sqrt(n/2)*D)
        names(out)=c('ocpt','test statistic')
        return(out)
      }
      else{
        return(tau)
      }
    }
    
    
    if(is.null(dim(data))==TRUE){
      # single data set
      ocpt=singledim(data,extrainf,minseglen)
      return(ocpt)
    }
    else{
      rep=nrow(data)
      n=ncol(data)
      ocpt=NULL
      if(extrainf==FALSE){
        for(i in 1:rep){
          ocpt[i]=singledim(data[i,],extrainf,minseglen)
        }
      }
      else{
        ocpt=matrix(0,ncol=2,nrow=rep)
        for(i in 1:rep){
          ocpt[i,]=singledim(data[i,],extrainf,minseglen)
        }
        colnames(ocpt)=c('ocpt','test statistic')
      }
      return(ocpt)
    }
  }


online.single.var.css<-function(data,penalty="MBIC",pen.value=0,class=TRUE,param.estimates=TRUE,minseglen){
  if(length(pen.value)>1){stop('Only one dimensional penalties can be used for CSS')}
  if(penalty=="MBIC"){stop("MBIC penalty is not valid for nonparametric test statistics.")}
  diffparam=1
  if(is.null(dim(data))==TRUE){
    # single dataset
    n=length(data)
  }
  else{
    n=ncol(data)
  }
  if(n<4){stop('Data must have atleast 4 observations to fit a changepoint model.')}
  if(n<(2*minseglen)){stop('Minimum segment legnth is too large to include a change in this data')}
  
  pen.value = penalty_decision(penalty, pen.value, n, diffparam, asymcheck = "var.css", method="AMOC")
  if(is.null(dim(data))==TRUE){
    tmp=online.single.var.css.calc(coredata(data),extrainf=TRUE,minseglen)
    ans=online.decision(tau=tmp[1],null=tmp[2],penalty="Manual",n=n,diffparam=1,pen.value=pen.value)
    if(class==TRUE){
      out=new("ocpt")
      data.set(out)=data; ocpttype(out)="variance"; method(out)="AMOC"; test.stat(out)="CSS"; pen.type(out)=penalty; pen.value(out)=ans$pen;nocpts.max(out)=1
      if(ans$ocpt != n){ocpts(out)=c(ans$ocpt,n)}
      else{ocpts(out)=ans$ocpt}
      if(param.estimates==TRUE){
        out=param(out)
      }
      return(out)
    }
    else{ return(ans$ocpt)}
  }
  else{ 
    tmp=online.single.var.css.calc(data,extrainf=TRUE,minseglen)
    ans=online.decision(tau=tmp[,1],null=tmp[,2],penalty="Manual",n=n,diffparam=1,pen.value=pen.value)
    if(class==TRUE){
      rep=nrow(data)
      out=list()
      for(i in 1:rep){
        out[[i]]=new("ocpt")
        data.set(out[[i]])=ts(data[i,]); ocpttype(out[[i]])="variance"; method(out[[i]])="AMOC"; test.stat(out[[i]])="CSS"; pen.type(out[[i]])=penalty;pen.value(out[[i]])=ans$pen;nocpts.max(out[[i]])=1
        if(ans$ocpt[i] != n){ocpts(out[[i]])=c(ans$ocpt[i],n)}
        else{ocpts(out[[i]])=ans$ocpt[i]}
        if(param.estimates==TRUE){
          out[[i]]=param(out[[i]])
        }
      }
      return(out)
    }
    else{ return(ans$ocpt)}
  }
}










online.single.mean.cusum.calc <-
  function(data,extrainf=TRUE,minseglen){
    singledim=function(data,extrainf=TRUE,minseglen){
      n=length(data)
      ybar=mean(data)
      y=c(0,cumsum(data-ybar))
      y=y/n
      
      M=max(abs(y[minseglen:(n-minseglen+1)]),na.rm=T)
      tau=which.max(abs(y[minseglen:(n-minseglen+1)]))+minseglen-1
      if(extrainf==TRUE){
        out=c(tau,M)
        names(out)=c('ocpt','test statistic')
        return(out)
      }
      else{
        return(tau)
      }
    }
    
    
    if(is.null(dim(data))==TRUE){
      # single data set
      ocpt=singledim(data,extrainf,minseglen)
      return(ocpt)
    }
    else{
      rep=nrow(data)
      n=ncol(data)
      ocpt=NULL
      if(extrainf==FALSE){
        for(i in 1:rep){
          ocpt[i]=singledim(data[i,],extrainf,minseglen)
        }
      }
      else{
        ocpt=matrix(0,ncol=2,nrow=rep)
        for(i in 1:rep){
          ocpt[i,]=singledim(data[i,],extrainf,minseglen)
        }
        colnames(ocpt)=c('ocpt','test statistic')
      }
      return(ocpt)
    }
  }


online.single.mean.cusum<-function(data,penalty="Asymptotic",pen.value=0.05,class=TRUE,param.estimates=TRUE,minseglen){
  if(length(pen.value)>1){stop('Only one dimensional penalties can be used for CUSUM')}
  if(penalty=="MBIC"){stop("MBIC penalty is not valid for nonparametric test statistics.")}

  if(is.null(dim(data))==TRUE){
    # single dataset
    n=length(data)
  }
  else{
    n=ncol(data)
  }
  if(n<2){stop('Data must have atleast 2 observations to fit a changepoint model.')}
  if(n<(2*minseglen)){stop('Minimum segment legnth is too large to include a change in this data')}
  
  pen.value = penalty_decision(penalty, pen.value, n, diffparam=1, asymcheck="mean.cusum", method="AMOC")
  if(is.null(dim(data))==TRUE){
    tmp=online.single.mean.cusum.calc(coredata(data),extrainf=TRUE,minseglen)
    ans=online.decision(tau=tmp[1],null=tmp[2],penalty=penalty,n=n,diffparam=1,pen.value=pen.value)
    if(class==TRUE){
      out=new("ocpt")
      data.set(out)=data; ocpttype(out)="mean"; method(out)="AMOC"; test.stat(out)="CUSUM"; pen.type(out)=penalty; pen.value(out)=ans$pen;nocpts.max(out)=1
      if(ans$ocpt != n){ocpts(out)=c(ans$ocpt,n)}
      else{ocpts(out)=ans$ocpt}
      if(param.estimates==TRUE){
        out=param(out)
      }
      return(out)
    }
    else{ return(ans$ocpt)}
  }
  else{ 
    tmp=online.single.mean.cusum.calc(data,extrainf=TRUE,minseglen)
    ans=online.decision(tau=tmp[,1],null=tmp[,2],penalty=penalty,n=n,diffparam=1,pen.value=pen.value)
    if(class==TRUE){
      rep=nrow(data)
      out=list()
      for(i in 1:rep){
        out[[i]]=new("ocpt")
        data.set(out[[i]])=ts(data[i,]); ocpttype(out[[i]])="mean"; method(out[[i]])="AMOC"; test.stat(out[[i]])="CUSUM"; pen.type(out[[i]])=penalty;pen.value(out[[i]])=ans$pen;nocpts.max(out[[i]])=1
        if(ans$ocpt[i] != n){ocpts(out[[i]])=c(ans$ocpt[i],n)}
        else{ocpts(out[[i]])=ans$ocpt[i]}
        if(param.estimates==TRUE){
          out[[i]]=param(out[[i]])
        }
      }
      return(out)
    }
    else{ return(ans$ocpt)}
  }
}

