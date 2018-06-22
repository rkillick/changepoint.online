ocpt.plot=function(data,Q=4){ #Q= maximum number of changepoints
    n = length(data)
    if(n > Q){
        for(i in (2*Q):n){
            Sys.sleep(0.1)
            plot(x=1:i,y=data[1:i],xlim=c(0,n),ylim=range(data), xlab = i, ylab = "", type ="l")  #type = "l" can be used for line plot
            ansvar=ocpt.meanvar.initialise(data[1:i],penalty="MBIC",pen.value==2*log(n),method="PELT",Q=5,test.stat="Normal",class=TRUE,param.estimates=TRUE,shape=1,minseglen=2)
            changepoints <- cpts(ansvar)
            abline(h=mean(data[1:i]), col = "red")
            if(length(ansvar@cpts)>0){
                for(i in 1:Q){
                    abline(v=changepoints, col = "blue")
                }
            }
        }
    }
}
