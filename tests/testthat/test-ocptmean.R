context("ocpt.mean function tests")

set.seed(1) # Note: new data sets must be added at the end.
previoussingmeandata <- c(rnorm(100,50,1),rnorm(100,100,1))
previousmulmeandata <- c(rnorm(100,100,1),rnorm(100,70,1),rnorm(100,5,1),rnorm(100,4,1))
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
shortdata <- c(2,2,2)
negativedata <- jitter(rep(-100, 200))

#list of datas
meandata <- list(singmeandata, mulmeandata, nochangedata)
meanvardata <-  list(singmeanvardata, mulmeanvardata, nochangedata)
data <- list(singmeandata, mulmeandata, nochangedata, constantdata, shortdata, negativedata)
previousdata <- list(previoussingmeandata)
updateddata <- list(singmeandata)
ecpdata <- matrix(singmeandata,ncol=1)
penalties <- c("None", "SIC", "BIC", "AIC", "Hannan-Quinn", "Manual", "MBIC") 
penalties.error <- c("Asymptotic","CROPS")

testStats <- c("Normal","ECP") 

#Firt test that results of changepoint.online yield same answers as offline
for(i in data){
  ans.online = ocpt.mean.initialise(i)
  ans.offline = cpt.mean(i,method='PELT')
    expect_equal(cpts(ans.online),cpts(ans.offline))
}

for(i in data){
  english = ocpt.mean.initialise(i)
  american = ocpt.mean.initialize(i)
  expect_identical(english,american)
}

for(i in previousdata){
  for(j in updateddata){
    previousans = ocpt.mean.initialise(i)
    updatedans = ocpt.mean.update(previousans,j)
    ij = c(i,j)
    ans.offline = cpt.mean(ij,method="PELT")
    expect_equal(cpts(updatedans),cpts(ans.offline))
  }
}

#Test ecp and pelt yield same answers (difference of one due to theoretical choice of which data point is considered the changepoint)
expect_identical(ecpdata,as.matrix(singmeandata))
ecpans = ocpt.mean.initialise(ecpdata,test.stat = "ECP")
normans = ocpt.mean.initialise(singmeandata,test.stat = "Normal")
expect_equal(estimates(ecpans)-1,cpts(normans))
#Check correct classes are called
for(i in testStats){
if(i == "ECP"){
  ecpans = ocpt.mean.initialise(ecpdata,test.stat = i)
  expect_identical(class(ecpans)[1],"ecp.ocpt")
}else{
  ans = ocpt.mean.initialise(singmeandata,test.stat = i)
  expect_identical(class(ans)[1],"ocpt")
}
}

#Check all penalties have either solution or it is clearly stated they no longer work
for(p in penalties){
  ans = ocpt.mean.initialise(singmeandata,penalty = p)
}
for(p in penalties.error){
  if(p=="CROPS"){
    expect_that(ocpt.mean.initialise(singmeandata,penalty = p),throws_error("Use cpt.mean from changepoint as CROPS is not available with changepoint.online"))
  }else{
    expect_that(ocpt.mean.initialise(singmeandata,penalty = p),throws_error("Asymptotic penalty values must be > 0 and <= 1"))
  }
}
  
