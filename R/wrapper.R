PELT.online.initialise=function(sumstat,pen=0,cost_func = "mean.norm", shape = 1, minseglen = 1){
  # function that uses the PELT method to calculate changes in mean where the segments in the data are assumed to be Normal
  # initialisation function for online use

  # assumes dyn.load('PELTonline.so') has already been done
  ndone=0
  nupdate=length(sumstat[,1]) - 1 
  
  lastchangelike = c(rep(0,nupdate + ndone + 1))
  storage.mode(lastchangelike) = 'double'
  
  lastchangecpts = c(rep(0,nupdate + ndone + 1))
  storage.mode(lastchangecpts) = 'integer'
  
  checklist = c(rep(0,nupdate + ndone + 1))
  storage.mode(checklist) = 'integer'
  
  nchecklist=0
  
  cptsout=rep(0,ndone+nupdate+1) # sets up null vector for changepoint answer
  storage.mode(cptsout)='integer'

  answer=list()
  answer[[7]]=1
  on.exit(.C("FreePELT",answer[[7]]))
  
  error=0

  answer=.C('PELT_online',cost_func=as.character(cost_func),sumstat=as.double(sumstat),ndone=as.integer(ndone),nupdate=as.integer(nupdate),penalty=as.double(pen),cptsout=cptsout,error=as.integer(error),shape=as.double(shape), minseglen=as.integer(minseglen), lastchangelike=lastchangelike, lastchangecpts=lastchangecpts,checklist=checklist,nchecklist=as.integer(nchecklist))
  names(answer)=c('cost_func','sumstat','ndone','nupdate','penalty','cptsout','error','shape','minseglen','lastchangelike','lastchangecpts','checklist','nchecklist')

  if(answer$error>0){
    stop("C code error:",answer$error,call.=F)
  }

  return(answer)
}




PELT.online.update=function(previousanswer,newdata){
    # function that uses the PELT method to calculate changes in mean where the segments in the data are assumed to be Normal
    # update function for online use
    # previous answer is of the format spat out by PELT.mean.norm.initialise
     # note for previousanswer[], 1=cost_func 2=sumstat 3=ndone 4=nupdate 5=penalty 6=cptsout 7=error 8=shape 9=minseglen 10=lastchanglike 11=lastchangepts 12=checklist 13=nchecklist
    # new data is a numerical vector of length 1 or more
    
    # assumes dyn.load('PELTonline.so') has already been done
    ndone=previousanswer@ndone+previousanswer@nupdate
    nupdate=length(newdata)
    mu=mean(newdata)
    sumstat=cbind(c(0,cumsum(coredata(newdata))),c(0,cumsum(coredata(newdata)^2)),cumsum(c(0,(coredata(newdata)-mu)^2)))
    storage.mode(sumstat) = 'double'
    
    
    lastchangelike=c(previousanswer@lastchangelike,rep(0,nupdate))
    storage.mode(lastchangelike)='double'
    
    lastchangecpts=c(previousanswer@lastchangecpts,rep(0,nupdate))
    storage.mode(lastchangecpts)='integer'
    
    checklist=c(previousanswer@checklist[1:previousanswer@nchecklist],rep(0,nupdate))
    storage.mode(checklist)='integer'
    
    
    cptsout=rep(0,ndone+nupdate+1) # sets up null vector for changepoint answer
    storage.mode(cptsout)='integer'
    
    error=0
    answer=list()
    answer[[7]]=1
    on.exit(.C("FreePELT",answer[[7]]))
     answer=.C('PELT_online',cost_func=previousanswer@cost_func,sumstat=sumstat,ndone=as.integer(ndone),nupdate=as.integer(nupdate),penalty=as.double(previousanswer@pen.value),cptsout=cptsout,error=as.integer(error),shape=previousanswer@shape, minseglen=as.integer(previousanswer@minseglen), lastchangelike=lastchangelike, lastchangecpts=lastchangecpts,checklist=checklist,nchecklist=as.integer(previousanswer@nchecklist))
    if(answer[[7]]>0){
        print("C code error:",answer[[7]])
        stop(call.=F)
    }
    names(answer)=c('cost_func','sumstat','ndone','nupdate','penalty','cptsout','error','shape','minseglen','lastchangelike','lastchangecpts','checklist','nchecklist')
    return(answer)
}
