context("ocpt.mean.initialise tests")

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
shortdata <- c(2)
negativedata <- jitter(rep(-100, 200))

characterdata <- rep("ert", 200)

#NAdata - creates 10 random NA within singmeandata
NAdata <- singmeandata
rn <- sample(1:length(singmeandata), 10, replace=F)

for(i in rn){
  NAdata[i] <- NA
}
###################
meandata <- list(singmeandata, mulmeandata, nochangedata)
vardata <-  list(singvardata, mulvardata, nochangedata)
meanvardata <-  list(singmeanvardata, mulmeanvardata, nochangedata)

penalties <- c("None", "SIC", "BIC", "AIC", "Hannan-Quinn", "Asymptotic", "Manual", "MBIC", "CROPS") 

asympenval <- list(1, 0.756, 0.234, 'return', -1, 0) #need to add character string and -1 and 0
manpenval <- list("2+log(n)", "log(n)", "3*n", -1, "diffparam-1") #null, alt, tau*2 don't work (comes back with user defind error "your manual cannot be evaluated")

QValues <- list(3, -1, 'jamie', 200000) #data variable needs to be modified - larger than data length and over half data length
#QValues <- c(3, 5)

testStats <- c("Normal","ECP") 
#asym and cusum return user defined "no asymptotic penalty" && "asymptotic penalties not implemented"

class <- c(TRUE, FALSE)
param.estimates <- c(TRUE, FALSE)

minseglen <- c(-1, 10, 20000)

cropspenval = list(c(2,2.5), c(3,1), c(5,5,6), c("a", "b"), 5, "a")

t = 0 #count for number of iterations



  
  