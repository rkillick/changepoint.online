ocpt.plot=function(data,penalty="MBIC",pen.value=0,Q=5,test.stat="Normal",class=TRUE,param.estimates=TRUE,shape=1,minseglen=2){ #Q= maximum number of changepoints
    n = length(data)
    if(n > Q){
        for(i in (2*Q):n){
            Sys.sleep(0.1)
            plot(x=1:i,y=data[1:i],xlim=c(0,n),ylim=range(data), xlab = i, ylab = "", type ="l")  #type = "l" can be used for line plot
            ansvar=ocpt.meanvar.initialise(data[1:i],penalty=penalty,pen.value=pen.value,Q=Q,test.stat=test.stat,class=class,param.estimates=param.estimates,shape=shape,minseglen=minseglen)
            changepoints <- cpts(ansvar)
            if(length(ansvar@cpts)>0){
                for(i in 1:Q){
                    abline(v=changepoints, col = "blue")
                }
            }
        }
    }
}
