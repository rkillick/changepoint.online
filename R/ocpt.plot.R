ocpt.plot=function(data,Q=4){ #Q= maximum number of changepoints
    n = length(data)
    if(n > Q){
        for(i in (2*Q):n){
            Sys.sleep(0.1)
            plot(x=1:i,y=data[1:i],xlim=c(0,n),ylim=range(data), xlab = i, ylab = "", type ="l")  #type = "l" can be used for line plot
            ansvar=ocpt.meanvar.initialise(data[1:i],method="PELT",Q=Q,penalty="Manual",pen.value=2*log(n))
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
