context("ocpt.meanvar tests")

set.seed(1) #Note: new data sets must be added at the end.
singmeandata <- c(rnorm(100,0,1),rnorm(100,10,1))
mulmeandata <- c(rnorm(100,0,1),rnorm(100,10,1),rnorm(100,20,1),rnorm(100,50,1))
nochangedata <- c(rnorm(200,0,1))

singvardata <- c(rnorm(100,10,1),rnorm(100,10,5))
mulvardata <- c(rnorm(100,20,10),rnorm(100,20,15),rnorm(100,20,20),rnorm(100,20,25))

singmeanvardata <- c(rnorm(50,0,1),rnorm(50,3,10))
mulmeanvardata <- c(rnorm(50,0,1),rnorm(50,5,3),rnorm(50,10,1),rnorm(50,3,10))
mulmeanvarexpdata <- c(rexp(50,1), rexp(50,3), rexp(50,5), rexp(50,7)) #rate values correct
mulmeanvarpoisdata <- c(rpois(50,10), rpois(50,20), rpois(50,15), rpois(50,25)) #lambda values correct?

constantdata <- rep(1, 200)
shortdata <- c(2,4)
negativedata <- jitter(rep(-100, 200) )

#NAdata - creates 10 random NA within singmeandata
NAdata <- singmeanvardata
rn <- sample(1:length(singmeanvardata), 10, replace=F)

for(i in rn){
  NAdata[i] <- NA
}
###################

data <- list(singmeanvardata,mulmeanvardata, mulmeanvarexpdata, mulmeanvarpoisdata, nochangedata, constantdata, NAdata, shortdata, negativedata)

# meandata <- list(singmeandata, mulmeandata, nochangedata)
# vardata <-  list(singvardata, mulvardata, nochangedata)
# meanvardata <-  list(singmeanvardata, mulmeanvardata, nochangedata)

methods <- c("AMOC", "PELT") #might want to change code to convert to uppercase so less likely to break code
#Segneigh taking too long and deprecation, so leaving until very last.
#methods <- c("AMOC")

penalties <- c("SIC", "BIC", "AIC", "Hannan-Quinn", "Asymptotic", "MBIC") #CROPS segfaulting 

asympenval <- list(1, 0.756, 0.234, 'return', -1, 0) #need to add character string and -1 and 0
#manpenval <- list("2+log(n)", "log(n)", "3*n", -1, "diffparam-1") 
manpenval <- list(1)

QValues <- list(3, -1, 'jamie', 200000) 
#QValues <- c(3, 5)

testStats <- c("Normal") 

class <- c(TRUE)
param.estimates <- c(TRUE, FALSE)

shap <- c(1)

cropspenval = list(c(2,2.5), c(3,1), c(5,5,6), c("a", "b"), 5, "a")

t = 0 #count for number of iterations
