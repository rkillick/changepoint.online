context("ocpt.var tests")

set.seed(1) # Note: new data sets must be added at the end.
singmeandata <- c(rnorm(100,0,1),rnorm(100,10,1))
mulmeandata <- c(rnorm(100,0,1),rnorm(100,10,1),rnorm(100,20,1),rnorm(100,50,1))
nochangedata <- c(rnorm(200,0,1))

singvardata <- c(rnorm(100,10,1),rnorm(100,10,5))
mulvardata <- c(rnorm(100,20,10),rnorm(100,20,15),rnorm(100,20,20),rnorm(100,20,25))

singmeanvardata <- c(rnorm(50,0,1),rnorm(50,3,10))
mulmeanvardata <- c(rnorm(50,0,1),rnorm(50,5,3),rnorm(50,10,1),rnorm(50,3,10))
mulmeanvarexpdata <- c(rexp(50,1), rexp(50,3), rexp(50,5), rexp(50,7)) #rate values correct
mulmeanvarpoisdata <- c(rpois(50,1), rpois(50,2), rpois(50,3), rpois(50,5)) #lambda values correct?

constantdata <- rep(1, 200)
negativedata <- jitter(rep(-100, 200) )

#NAdata - creates 10 random NA within singmeandata
NAdata <- singvardata
rn <- sample(1:length(singvardata), 10, replace=F)

for(i in rn){
  NAdata[i] <- NA
}
###################

data <- list(singmeanvardata,mulmeanvardata,mulmeanvardata, mulmeanvarexpdata, mulmeanvarpoisdata, nochangedata, constantdata, NAdata, negativedata)

# meandata <- list(singmeandata, mulmeandata, nochangedata)
# vardata <-  list(singvardata, mulvardata, nochangedata)
# meanvardata <-  list(singmeanvardata, mulmeanvardata, nochangedata)

methods <- c("AMOC", "PELT") #might want to change code to convert to uppercase so less likely to break code
#Segneigh taking too long and deprecation, so leaving until very last.
#methods <- c("AMOC")

penalties <- c("None", "SIC", "BIC", "AIC", "Hannan-Quinn", "Asymptotic", "Manual", "MBIC") 

asympenval <- list(1, 0.756, 0.234, 'return', -1, 0) 
manpenval <- list(-1, "boston bob") #don't have varaibles so returns false test

QValues <- list(3, -1, 'jamie', 200000) 
#QValues <- c(3, 5)

testStats <- c("Normal") 

knowmean <- c(FALSE) #need to deal with TRUE values
muValues <- c(NA)

class <- c(TRUE)
param.estimates <- c(TRUE, FALSE)

cropspenval = list(c(2,2.5), c(3,1), c(5,5,6), c("a", "b"), 5, "a")

t = 0 #count for number of iterations

