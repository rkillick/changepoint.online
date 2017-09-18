PELT.online.initialise=function(data,pen=0, cost_func = "norm.mean", shape = 1, minseglen = 1,lastchangelike, lastchangecpts, numchangecpts,checklist){
  # function that uses the PELT method to calculate changes in mean where the segments in the data are assumed to be Normal
  # initialisation function for online use

  # assumes dyn.load('PELTonline.so') has already been done
  ndone=0
  nupdate=length(data)
  y2=c(0,cumsum((data)^2))
  y=c(0,cumsum(data))

  if(missing(lastchangelike)) {lastchangelike = array(0,dim = nupdate+1)}
  if(missing(lastchangecpts)) {lastchangecpts = array(0,dim = nupdate+1)}
  if(missing(numchangecpts)) {numchangecpts = array(0,dim = nupdate+1)}
  if(missing(checklist)) {checklist = array(0,dim = nupdate+1)}
  
  cptsout=rep(0,ndone+nupdate+1) # sets up null vector for changepoint answer
  storage.mode(cptsout)='integer'

  answer=list()
  answer[[6]]=1
  on.exit(.C("FreePELT",answer[[6]]))
  
  storage.mode(lastchangelike) = 'double'
  storage.mode(lastchangecpts) = 'integer'
  storage.mode(numchangecpts) = 'integer'
  storage.mode(checklist) = 'integer'

  error=0

  # 1=ndone,2=y2,3=y,4=nupdate,5=pen,6=lastchangecpts,7=lastchangelike,8=checklist,9=nchecklist,10=cptsout,11=error
  answer=.C('PELT_online',cost_func, sumstat,as.integer(ndone),as.integer(nupdate),as.double(pen),cptsout,as.integer(error),as.double(shape), as.integer(minseglen), lastchangelike, lastchangecpts,checklist,nchecklist,numchangecpts)
  
  if(answer[[6]]>0){
    stop("C code error:",answer[[6]],call.=F)
  }

  answer=.C('PELT_mean_norm_update',as.integer(ndone),y2,y,as.integer(nupdate),as.double(penalty),lastchangecpts,lastchangelike,checklist,as.integer(0),cptsout,as.integer(error))

  names(answer)=c('ndone','y2','y','nupdate','penalty','last cpts','last like','checklist','nchecklist','cpts','error')
  return(answer)
}



PELT.online.update=function(previousanswer,newdata){
  # function that uses the PELT method to calculate changes in mean where the segments in the data are assumed to be Normal
  # update function for online use
  # previous answer is of the format spat out by PELT.mean.norm.initialise
  # new data is a numerical vector of length 1 or more

  # assumes dyn.load('PELTonline.so') has already been done
  
  ndone=previousanswer[[1]]+previousanswer[[4]]
  nupdate=length(newdata)
  y2=c(previousanswer[[2]],rep(previousanswer[[2]][ndone+1],nupdate)) + c(rep(0,ndone+1),cumsum((newdata)^2))
  y=c(previousanswer[[3]],rep(previousanswer[[3]][ndone+1],nupdate))+ c(rep(0,ndone+1),cumsum(newdata))

  storage.mode(y2)='double'
  storage.mode(y)='double'
  cptsout=rep(0,ndone+nupdate+1) # sets up null vector for changepoint answer
  storage.mode(cptsout)='integer'
  
  lastchangecpts=rbind(previousanswer[[6]],matrix(0,ncol=2,nrow=nupdate))
  storage.mode(lastchangecpts)='integer'
  
  lastchangelike=c(previousanswer[[7]],rep(0,nupdate))
  storage.mode(lastchangelike)='double'
  
  checklist=c(previousanswer[[8]][1:previousanswer[[9]]],rep(0,nupdate))
  storage.mode(checklist)='integer'
  
  error=0
  answer=list()
  answer[[11]]=1
  on.exit(.C("FreePELT",answer[[11]]))
  # 1=ndone,2=y2,3=y,4=nupdate,5=pen,6=lastchangecpts,7=lastchangelike,8=checklist,9=nchecklist,10=cptsout,11=error
  answer=.C('PELT_mean_norm_update',as.integer(ndone),y2,y,as.integer(nupdate),as.double(previousanswer[[5]]),lastchangecpts,lastchangelike,checklist,as.integer(previousanswer[[9]]),cptsout,as.integer(error))
  if(answer[[11]]>0){
    print("C code error:",answer[[11]])
    stop(call.=F)
  }
  names(answer)=c('ndone','y2','y','nupdate','penalty','last cpts','last like','checklist','nchecklist','cpts','error')
  return(answer)
}
