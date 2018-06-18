PELT.online = function(sumstat,pen=0, cost_func = "mean.norm", shape = 1, minseglen = 1,lastchangelike, lastchangecpts, numchangecpts,checklist){
  # function that uses the PELT method to calculate changes in mean where the segments in the data are assumed to be Normal
  if(is.null(dim(sumstat))){sumstat=as.matrix(sumstat,ncol=1)}
  
  ndone=1
  nupdate=length(sumstat[,1]) - 1
  
  if(nupdate<2){stop('Data must have at least 2 observations to fit a changepoint model.')}
  storage.mode(sumstat) = 'double'
  
  
  
  
  if(missing(lastchangelike)) {lastchangelike = array(0,dim = nupdate + ndone + 1)}
  storage.mode(lastchangelike) = 'double'
  
  if(missing(lastchangecpts)) {lastchangecpts = array(0,dim = nupdate + ndone + 1)}
  storage.mode(lastchangecpts) = 'integer'
  
  if(missing(numchangecpts)) {numchangecpts = array(0,dim = nupdate + ndone + 1)}
  storage.mode(numchangecpts) = 'integer'
  
  if(missing(checklist)) {checklist = array(0,dim = nupdate + ndone + 1)}
  storage.mode(checklist) = 'integer'
  
  if(missing(checklist)){nchecklist=0}
  else{nchecklist=sum(checklist>0)}
  
  cptsout=rep(0,ndone+nupdate+1) # sets up null vector for changepoint answer
  storage.mode(cptsout)='integer'
  
  answer=list()
  answer[[7]]=1
  on.exit(.C("FreePELT",answer[[7]]))
  
  error=0
  
  
answer=.C('PELT_online',cost_func=cost_func,sumstat=sumstat,ndone=as.integer(ndone),nupdate=as.integer(nupdate),penalty=as.double(pen),cptsout=cptsout,error=as.integer(error),shape=as.double(shape), minseglen=as.integer(minseglen), lastchangelike=lastchangelike, lastchangecpts=lastchangecpts,checklist=checklist,nchecklist=as.integer(nchecklist),numchangecpts=numchangecpts)


if(answer$error>0){
    stop("C code error:",answer$error,call.=F)
}
return(list(lastchangecpts=answer$lastchangecpts,cpts=sort(answer$cptsout[answer$cptsout>0]), lastchangelike=answer$lastchangelike, ncpts=answer$numchangecpts))

}
