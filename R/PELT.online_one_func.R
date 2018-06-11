PELT.online = function(sumstat,pen=0, cost_func = "norm.mean", shape = 1, minseglen = 1,lastchangelike, lastchangecpts, numchangecpts){
  # function that uses the PELT method to calculate changes in mean where the segments in the data are assumed to be Normal
  if(is.null(dim(sumstat))){sumstat=as.matrix(sumstat,ncol=1)}
  n = length(sumstat[,1]) - 1
  if(n<2){stop('Data must have at least 2 observations to fit a changepoint model.')}
  
  storage.mode(sumstat) = 'double'
  error=0
  
  if(missing(lastchangelike)) {lastchangelike = array(0,dim = n+1)}
  if(missing(lastchangecpts)) {lastchangecpts = array(0,dim = n+1)}
  if(missing(numchangecpts)) {numchangecpts = array(0,dim = n+1)}
  
  cptsout=rep(0,n) # sets up null vector for changepoint answer
  storage.mode(cptsout)='integer'
  
  answer=list()
  answer[[6]]=1
  on.exit(.C("FreePELT",answer[[6]]))
  
  storage.mode(lastchangelike) = 'double'
  storage.mode(lastchangecpts) = 'integer'
  storage.mode(numchangecpts) = 'integer'

answer=.C('PELT_online',cost_func=cost_func,sumstat=sumstat,ndone=as.integer(n),penalty=as.double(pen),cptsout=cptsout,error=as.integer(error),shape=as.double(shape), minseglen=as.integer(minseglen), lastchangelike=lastchangelike, lastchangecpts=lastchangecpts,numchangecpts=numchangecpts)

if(answer$error>0){
    stop("C code error:",answer$error,call.=F)
}
return(list(lastchangecpts=answer$lastchangecpts,cpts=sort(answer$cptsout[answer$cptsout>0]), lastchangelike=answer$lastchangelike, ncpts=answer$numchangecpts))

}
