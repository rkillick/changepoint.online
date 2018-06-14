CROPS <- function(data, penalty="CROPS", pen.value, costfunc="norm.mean", method="PELT", test.stat="Normal", class=TRUE, param.est=TRUE, minseglen, shape, func){
  if(method != "PELT"){stop('CROPS is a valid penalty choice only if method="PELT", please change your method or your penalty.')}
  mu <- mean(data)
  sumstat=cbind(c(0,cumsum(coredata(data))),c(0,cumsum(coredata(data)^2)),cumsum(c(0,(coredata(data)-mu)^2)))
  
  switch(test.stat,
    "Normal" = {stat = "norm"},
    "Exponential" = {stat = "exp"},
    "Gamma" = {stat = "gamma"},
    "Poisson" = {stat = "poisson"},
    {stop("Only Normal, Exponential, Gamma and Poisson are valid test statistics")}
  )
  
  out = online.range_of_penalties(sumstat=sumstat, cost=costfunc, min_pen=pen.value[1], max_pen=pen.value[2], minseglen=minseglen)
  
  if(func=="var"){
    ocpttype="variance"
  }else if(func=="meanvar"){
    ocpttype="mean and variance"
  }else{
    ocpttype="mean"
  }
  
  if(class==TRUE){
      ans = online.class_input(data=data,ocpttype=ocpttype, method="PELT", test.stat=test.stat, penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.est, out=out,shape=shape)
      if(func=="var"){
        param.est(ans)=c(param.est(ans),mean=mu)
      }
    return(ans)
  }else{return(out)}
}
