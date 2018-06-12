PELT.online.initialise=function(data,pen=0,cost_func = "norm.mean", shape = 1, minseglen = 1,lastchangelike, lastchangecpts, numchangecpts,checklist){
  # function that uses the PELT method to calculate changes in mean where the segments in the data are assumed to be Normal
  # initialisation function for online use

  # assumes dyn.load('PELTonline.so') has already been done
  ndone=1
  nupdate=length(data) - 1
  mu=mean(data)
  
sumstat=cbind(c(0,cumsum(coredata(data))),c(0,cumsum(coredata(data)^2)),cumsum(c(0,(coredata(data)-mu)^2)))
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



#                                                                               #
#                                                                               #
#                                                                               #
#                                                                               #
#                                                                               #
##########################ORIGINAL UNEDITED######################################
#                                                                               #
#                                                                               #
#                                                                               #
#                                                                               #
#                                                                               #
#PELT.online.update=function(previousanswer,newdata){
    # function that uses the PELT method to calculate changes in mean where the segments in the data are assumed to be Normal
    # update function for online use
    # previous answer is of the format spat out by PELT.mean.norm.initialise
    # new data is a numerical vector of length 1 or more
    
    # assumes dyn.load('PELTonline.so') has already been done
    
#  ndone=previousanswer[[1]]+previousanswer[[4]]
#   nupdate=length(newdata)
#   y2=c(previousanswer[[2]],rep(previousanswer[[2]][ndone+1],nupdate)) + c(rep(0,ndone+1),cumsum((newdata)^2))
#   y=c(previousanswer[[3]],rep(previousanswer[[3]][ndone+1],nupdate))+ c(rep(0,ndone+1),cumsum(newdata))
    
#   storage.mode(y2)='double'
#    storage.mode(y)='double'
#   cptsout=rep(0,ndone+nupdate+1) # sets up null vector for changepoint answer
#   storage.mode(cptsout)='integer'
    
#   lastchangecpts=rbind(previousanswer[[6]],matrix(0,ncol=2,nrow=nupdate))
#   storage.mode(lastchangecpts)='integer'
#
#   lastchangelike=c(previousanswer[[7]],rep(0,nupdate))
#   storage.mode(lastchangelike)='double'
    
#   checklist=c(previousanswer[[8]][1:previousanswer[[9]]],rep(0,nupdate))
#   storage.mode(checklist)='integer'
    
#    error=0
#   answer=list()
#   answer[[11]]=1
#   on.exit(.C("FreePELT",answer[[11]]))
    # 1=ndone,2=y2,3=y,4=nupdate,5=pen,6=lastchangecpts,7=lastchangelike,8=checklist,9=nchecklist,10=cptsout,11=error
#answer=.C('PELT_mean_norm_update',as.integer(ndone),y2,y,as.integer(nupdate),as.double(previousanswer[[5]]),lastchangecpts,lastchangelike,checklist,as.integer(previousanswer[[9]]),cptsout,as.integer(error))
#   if(answer[[11]]>0){
#       print("C code error:",answer[[11]])
#       stop(call.=F)
#   }
#   names(answer)=c('ndone','y2','y','nupdate','penalty','last cpts','last like','checklist','nchecklist','cpts','error')
#   return(answer)
#}
