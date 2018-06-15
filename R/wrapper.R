PELT.online.initialise=function(sumstat,pen=0,cost_func = "mean.norm", shape = 1, minseglen = 1,lastchangelike, lastchangecpts, numchangecpts,checklist){
  # function that uses the PELT method to calculate changes in mean where the segments in the data are assumed to be Normal
  # initialisation function for online use

  # assumes dyn.load('PELTonline.so') has already been done
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
  names(answer)=c('cost_func','sumstat','ndone','nupdate','penalty','cptsout','error','shape','minseglen','lastchangelike','lastchangecpts','checklist','nchecklist','numchangecpts')

  if(answer$error>0){
    stop("C code error:",answer$error,call.=F)
  }

  return(answer)
}




PELT.online.update=function(previousanswer,newdata){
    # function that uses the PELT method to calculate changes in mean where the segments in the data are assumed to be Normal
    # update function for online use
    # previous answer is of the format spat out by PELT.mean.norm.initialise
     # note for previousanswer[], 1=cost_func 2=sumstat 3=ndone 4=nupdate 5=penalty 6=cptsout 7=error 8=shape 9=minseglen 10=lastchanglike 11=lastchangepts 12=checklist 13=nchecklist 14=numchangecpts
    # new data is a numerical vector of length 1 or more
    
    # assumes dyn.load('PELTonline.so') has already been done
    ndone=previousanswer$ndone+previousanswer$nupdate
    nupdate=length(newdata)
    mu=mean(newdata)
    sumstat=cbind(c(0,cumsum(coredata(newdata))),c(0,cumsum(coredata(newdata)^2)),cumsum(c(0,(coredata(newdata)-mu)^2)))
    storage.mode(sumstat) = 'double'
    
    
    lastchangelike=c(previousanswer[[10]],rep(0,nupdate))
    storage.mode(lastchangelike)='double'
    
    lastchangecpts=c(previousanswer[[11]],rep(0,nupdate))
    storage.mode(lastchangecpts)='integer'
    
    numchangecpts=c(previousanswer[[14]],rep(0,nupdate))
    storage.mode='integer'

    checklist=c(previousanswer[[12]][1:previousanswer[[13]]],rep(0,nupdate))
    storage.mode(checklist)='integer'
    
    nchecklist=sum(checklist>0)
    
    cptsout=rep(0,ndone+nupdate+1) # sets up null vector for changepoint answer
    storage.mode(cptsout)='integer'
    
    error=0
    answer=list()
    answer[[7]]=1
    on.exit(.C("FreePELT",answer[[7]]))
     answer=.C('PELT_online',cost_func=previousanswer$cost_func,sumstat=sumstat,ndone=as.integer(ndone),nupdate=as.integer(nupdate),penalty=as.double(previousanswer$pen),cptsout=cptsout,error=as.integer(error),shape=as.double(previousanswer$shape), minseglen=as.integer(previousanswer$minseglen), lastchangelike=lastchangelike, lastchangecpts=lastchangecpts,checklist=checklist,nchecklist=nchecklist,numchangecpts=numchangecpts)
    if(answer[[7]]>0){
        print("C code error:",answer[[7]])
        stop(call.=F)
    }
    names(answer)=c('cost_func','sumstat','ndone','nupdate','penalty','cptsout','error','shape','minseglen','lastchangelike','lastchangecpts','checklist','nchecklist','numchangecpts')
    return(answer)
}
