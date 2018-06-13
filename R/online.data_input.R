online.data_input <- function(data, method="PELT", pen.value, costfunc, minseglen, Q, var=0, shape=1){
  if(var !=0){
    mu<-var
  }else{
  mu <- mean(data)
  }
 sumstat=cbind(c(0,cumsum(coredata(data))),c(0,cumsum(coredata(data)^2)),cumsum(c(0,(coredata(data)-mu)^2)))
  if(method=="PELT"){
    out=PELT.online(sumstat,pen=pen.value,cost_func = costfunc,minseglen=minseglen, shape=shape)
  }
  else if(method=="BinSeg"){
    out=BINSEG(sumstat,pen=pen.value,cost_func = costfunc,minseglen=minseglen,Q=Q, shape=shape)
  }
  else if(method=="SegNeigh"){
    out=SEGNEIGH.online(data=data, pen.value=pen.value, Q=Q, costfunc=costfunc, var=var, shape=shape)
  }
  return(out)
}
