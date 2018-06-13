setClass("ocpt",slots=list(data.set="ts", ocpttype="character", method="character",     test.stat="character",pen.type="character",pen.value="numeric",minseglen="numeric",ocpts="numeric",nocpts.max="numeric",param.est="list",date="character",version="character"),prototype=prototype(date=date(),version=as(packageVersion("changepoint.online"),'character')))

setClass("ocpt.reg",slots=list(data.set="matrix", ocpttype="character", method="character", test.stat="character",pen.type="character",pen.value="numeric",minseglen="numeric",ocpts="numeric",nocpts.max="numeric",param.est="list",date="character",version="character"),prototype=prototype(ocpttype="regression",date=date(),version=as(packageVersion("changepoint.online"),"character")))

#  setClass("ocpt", representation(), prototype())
#  ocpts is the optimal segementation
#

setClass("ocpt.range",slots=list(ocpts.full="matrix", pen.value.full="numeric"), prototype=prototype(ocpttype="regression",date=date(),version=as(packageVersion("changepoint.online"),"character")), contains="ocpt")
# ocpts.full is the entire matrix
# pen.value.full (beta) values as an extra slot (vector)

# retrival functions for slots
if(!isGeneric("data.set")) {
    if (is.function("data.set")){
        fun <- data.set
    }
    else {fun <- function(object){
        standardGeneric("data.set")
    }
    }
    setGeneric("data.set", fun)
}
setMethod("data.set","ocpt",function(object) coredata(object@data.set))
setMethod("data.set","ocpt.reg",function(object) coredata(object@data.set))

if(!isGeneric("data.set.ts")) {
    if (is.function("data.set.ts")){
        fun <- data.set.ts
    }
    else {fun <- function(object){
        standardGeneric("data.set.ts")
    }
    }
    setGeneric("data.set.ts", fun)
}
setMethod("data.set.ts","ocpt",function(object) object@data.set)

if(!isGeneric("ocpttype")) {
    if (is.function("ocpttype")){
        fun <- ocpttype
    }
    else {fun <- function(object){
        standardGeneric("ocpttype")
    }
    }
    setGeneric("ocpttype", fun)
}
setMethod("ocpttype","ocpt",function(object) object@ocpttype)
setMethod("ocpttype","ocpt.reg",function(object) object@ocpttype)

if(!isGeneric("method")) {
    if (is.function("method")){
        fun <- method
    }
    else {fun <- function(object){
        standardGeneric("method")
    }
    }
    setGeneric("method", fun)
}
setMethod("method","ocpt",function(object) object@method)
setMethod("method","ocpt.reg",function(object) object@method)

# distribution remains for backwards compatability, changed to test.stat version 1.0
if(!isGeneric("distribution")) {
    if (is.function("distribution")){
        fun <- distribution
    }
    else {fun <- function(object){
        standardGeneric("distribution")
    }
    }
    setGeneric("distribution", fun)
}
setMethod("distribution","ocpt",function(object) object@test.stat)
setMethod("distribution","ocpt.reg",function(object) object@test.stat)

if(!isGeneric("test.stat")) {
    if (is.function("test.stat")){
        fun <- test.stat
    }
    else {fun <- function(object){
        standardGeneric("test.stat")
    }
    }
    setGeneric("test.stat", fun)
}
setMethod("test.stat","ocpt",function(object) object@test.stat)
setMethod("test.stat","ocpt.reg",function(object) object@test.stat)

if(!isGeneric("pen.type")) {
    if (is.function("pen.type")){
        fun <- pen.type
    }
    else {fun <- function(object){
        standardGeneric("pen.type")
    }
    }
    setGeneric("pen.type", fun)
}
setMethod("pen.type","ocpt",function(object) object@pen.type)
setMethod("pen.type","ocpt.reg",function(object) object@pen.type)

if(!isGeneric("pen.value")) {
    if (is.function("pen.value")){
        fun <- pen.value
    }
    else {fun <- function(object){
        standardGeneric("pen.value")
    }
    }
    setGeneric("pen.value", fun)
}
setMethod("pen.value","ocpt",function(object) object@pen.value)
setMethod("pen.value","ocpt.reg",function(object) object@pen.value)

if(!isGeneric("pen.value.full")) {
    if (is.function("pen.value.full")){
        fun <- pen.value.full
    }
    else {fun <- function(object){
        standardGeneric("pen.value.full")
    }
    }
    setGeneric("pen.value.full", fun)
}
setMethod("pen.value.full","ocpt.range",function(object) object@pen.value.full)

if(!isGeneric("minseglen")) {
    if (is.function("minseglen")){
        fun <- minseglen
    }
    else {fun <- function(object){
        standardGeneric("minseglen")
    }
    }
    setGeneric("minseglen", fun)
}
setMethod("minseglen","ocpt",function(object) object@minseglen)

if(!isGeneric("ocpts")) {
    if (is.function("ocpts")){
        fun <- ocpts
    }
    else {fun <- function(object){
        standardGeneric("ocpts")
    }
    }
    setGeneric("ocpts", fun)
}
setMethod("ocpts","ocpt",function(object) object@ocpts[-length(object@ocpts)])
setMethod("ocpts","ocpt.reg",function(object) object@ocpts[-length(object@ocpts)])

if(!isGeneric("ocpts.full")) {
    if (is.function("ocpts.full")){
        fun <- ocpts.full
    }
    else {fun <- function(object){
        standardGeneric("ocpts.full")
    }
    }
    setGeneric("ocpts.full", fun)
}
setMethod("ocpts.full","ocpt.range",function(object) object@ocpts.full)

if(!isGeneric("ocpts.ts")) {
    if (is.function("ocpts.ts")){
        fun <- ocpts.ts
    }
    else {fun <- function(object){
        standardGeneric("ocpts.ts")
    }
    }
    setGeneric("ocpts.ts", fun)
}
setMethod("ocpts.ts","ocpt",function(object) index(data.set.ts(object))[ocpts(object)] )

if(!isGeneric("nocpts.max")) {
    if (is.function("nocpts.max")){
        fun <- nocpts.max
    }
    else {fun <- function(object){
        standardGeneric("nocpts.max")
    }
    }
    setGeneric("nocpts.max", fun)
}
setMethod("nocpts.max","ocpt",function(object) object@nocpts.max)
setMethod("nocpts.max","ocpt.reg",function(object) object@nocpts.max)

if(!isGeneric("param.est")) {
    if (is.function("param.est")){
        fun <- param.est
    }
    else {fun <- function(object){
        standardGeneric("param.est")
    }
    }
    setGeneric("param.est", fun)
}
setMethod("param.est","ocpt",function(object) object@param.est)
setMethod("param.est","ocpt.reg",function(object) object@param.est)


setMethod("coef","ocpt",function(object) object@param.est)
setMethod("coef","ocpt.reg",function(object) object@param.est)

# nocpts function
if(!isGeneric("nocpts")) {
    if (is.function("nocpts")){
        fun <- nocpts
    }
    else {fun <- function(object){
        standardGeneric("nocpts")
    }
    }
    setGeneric("nocpts", fun)
}
setMethod("nocpts","ocpt",function(object) length(ocpts(object)))
setMethod("nocpts","ocpt.reg",function(object) length(ocpts(object)))

# seg.len function
if(!isGeneric("seg.len")) {
    if (is.function("seg.len")){
        fun <- seg.len
    }
    else {fun <- function(object){
        standardGeneric("seg.len")
    }
    }
    setGeneric("seg.len", fun)
}
setMethod("seg.len","ocpt",function(object){object@ocpts-c(0,object@ocpts[-length(object@ocpts)])})
setMethod("seg.len","ocpt.reg",function(object){object@ocpts-c(0,object@ocpts[-length(object@ocpts)])})
#i.e. if there is a changepoint in the data, return segment length. If not, return length of the data

# nseg function
if(!isGeneric("nseg")) {
    if (is.function("nseg")){
        fun <- nseg
    }
    else {fun <- function(object){
        standardGeneric("nseg")
    }
    }
    setGeneric("nseg", fun)
}
setMethod("nseg","ocpt",function(object){nocpts(object)+1})
setMethod("nseg","ocpt.reg",function(object){nocpts(object)+1})


# replacement functions for slots
setGeneric("data.set<-", function(object, value) standardGeneric("data.set<-"))
setReplaceMethod("data.set", "ocpt", function(object, value) {
    if(is.ts(value)){object@data.set <- value}else{object@data.set <- ts(value)}
    return(object)
})
setReplaceMethod("data.set", "ocpt.reg", function(object, value) {
    object@data.set <- value
    return(object)
})

setGeneric("ocpttype<-", function(object, value) standardGeneric("ocpttype<-"))
setReplaceMethod("ocpttype", "ocpt", function(object, value) {
    object@ocpttype <- value
    return(object)
})
setReplaceMethod("ocpttype", "ocpt.reg", function(object, value) {
    object@ocpttype <- value
    return(object)
})

setGeneric("method<-", function(object, value) standardGeneric("method<-"))
setReplaceMethod("method", "ocpt", function(object, value) {
    object@method <- value
    return(object)
})
setReplaceMethod("method", "ocpt.reg", function(object, value) {
    object@method <- value
    return(object)
})

# distribution remains for backwards compatability, changed to test.stat version 1.0
setGeneric("distribution<-", function(object, value) standardGeneric("distribution<-"))
setReplaceMethod("distribution", "ocpt", function(object, value) {
    object@test.stat <- value
    return(object)
})
setReplaceMethod("distribution", "ocpt.reg", function(object, value) {
    object@test.stat <- value
    return(object)
})

setGeneric("test.stat<-", function(object, value) standardGeneric("test.stat<-"))
setReplaceMethod("test.stat", "ocpt", function(object, value) {
    object@test.stat <- value
    return(object)
})
setReplaceMethod("test.stat", "ocpt.reg", function(object, value) {
    object@test.stat <- value
    return(object)
})

setGeneric("pen.type<-", function(object, value) standardGeneric("pen.type<-"))
setReplaceMethod("pen.type", "ocpt", function(object, value) {
    object@pen.type <- value
    return(object)
})
setReplaceMethod("pen.type", "ocpt.reg", function(object, value) {
    object@pen.type <- value
    return(object)
})

setGeneric("pen.value<-", function(object, value) standardGeneric("pen.value<-"))
setReplaceMethod("pen.value", "ocpt", function(object, value) {
    object@pen.value <- value
    return(object)
})
setReplaceMethod("pen.value", "ocpt.reg", function(object, value) {
    object@pen.value <- value
    return(object)
})

setGeneric("minseglen<-", function(object, value) standardGeneric("minseglen<-"))
setReplaceMethod("minseglen", "ocpt", function(object, value) {
    object@minseglen <- value
    return(object)
})
setReplaceMethod("minseglen", "ocpt.range", function(object, value) {
    object@minseglen <- value
    return(object)
})
setReplaceMethod("minseglen", "ocpt.reg", function(object, value) {
    object@minseglen <- value
    return(object)
})

setGeneric("ocpts<-", function(object, value) standardGeneric("ocpts<-"))
setReplaceMethod("ocpts", "ocpt", function(object, value) {
    if((ocpttype(object)=="meanar")|(ocpttype(object)=="trendar")){
        n=length(object@data.set) - 1
    }
    else{n=length(object@data.set)}
    
    if(value[length(value)]==n){object@ocpts <- value}
    else{        object@ocpts <- c(value,n)  }
    return(object)
})
setReplaceMethod("ocpts", "ocpt.reg", function(object, value) {
    if(value[length(value)]==nrow(object@data.set)){object@ocpts <- value}
    else{      object@ocpts <- c(value,nrow(object@data.set))  }
    return(object)
})

setGeneric("nocpts.max<-", function(object, value) standardGeneric("nocpts.max<-"))
setReplaceMethod("nocpts.max", "ocpt", function(object, value) {
    object@nocpts.max <- value
    return(object)
})
setReplaceMethod("nocpts.max", "ocpt.reg", function(object, value) {
    object@nocpts.max <- value
    return(object)
})

setGeneric("param.est<-", function(object, value) standardGeneric("param.est<-"))
setReplaceMethod("param.est", "ocpt", function(object, value) {
    object@param.est <- value
    return(object)
})
setReplaceMethod("param.est", "ocpt.reg", function(object, value) {
    object@param.est <- value
    return(object)
})

setGeneric("ocpts.full<-", function(object, value) standardGeneric("ocpts.full<-"))
setReplaceMethod("ocpts.full", "ocpt.range", function(object, value) {
    object@ocpts.full <- value
    return(object)
})
setGeneric("pen.value.full<-", function(object, value) standardGeneric("pen.value.full<-"))
setReplaceMethod("pen.value.full", "ocpt.range", function(object, value) {
    object@pen.value.full <- value
    return(object)
})
#     setGeneric("pen.value.input<-", function(object, value) standardGeneric("pen.value.input<-"))
#     setReplaceMethod("pen.value.input", "ocpt", function(object, value) {
#       object@pen.value.input <- value
#       return(object)
#     })


# parameter functions
setGeneric("param", function(object,...) standardGeneric("param"))
setMethod("param", "ocpt", function(object,shape,...) {
    param.mean=function(object){
        ocpts=c(0,object@ocpts)
        #nseg=length(ocpts)-1
        data=data.set(object)
        tmpmean=NULL
        for(j in 1:nseg(object)){
            tmpmean[j]=mean(data[(ocpts[j]+1):(ocpts[j+1])])
        }
        return(tmpmean)
    }
    param.var=function(object){
        ocpts=c(0,object@ocpts)
        #nseg=length(ocpts)-1
        data=data.set(object)
        seglen=seg.len(object)
        tmpvar=NULL
        for(j in 1:nseg(object)){
            tmpvar[j]=var(data[(ocpts[j]+1):(ocpts[j+1])])
        }
        tmpvar=tmpvar*(seglen-1)/seglen # correctly for the fact that the MLE estimate is /n but the var function is /n-1
        return(tmpvar)
    }
    param.scale=function(object,shape){
        ocpts=c(0,object@ocpts)
        #nseg=length(ocpts)-1
        data=data.set(object)
        y=c(0,cumsum(data))
        tmpscale=NULL
        for(j in 1:nseg(object)){
            tmpscale[j]=(y[(ocpts[j+1]+1)]-y[(ocpts[j]+1)])/((ocpts[j+1]-ocpts[j])*shape)
        }
        return(tmpscale)
    }
    param.trend=function(object){
        ocpts=c(0,object@ocpts)
        seglen=seg.len(object)
        data=data.set(object)
        n=length(data)
        sumstat=cbind(cumsum(c(0,data)),cumsum(c(0,data*c(1:n))))
        ocptsumstat=matrix(sumstat[object@ocpts+1,]-sumstat[c(0,ocpts(object))+1,],ncol=2)
        ocptsumstat[,2]=ocptsumstat[,2]-ocptsumstat[,1]*c(0,ocpts(object)) # i.e. creating newx3
        
        thetaS=(2*ocptsumstat[,1]*(2*seglen + 1) - 6*ocptsumstat[,2]) / (2*seglen*(2*seglen + 1) - 3*seglen*(seglen+1))
        thetaT=(6*ocptsumstat[,2])/((seglen+1)*(2*seglen+1)) + (thetaS * (1-((3*seglen)/((2*seglen)+1))))
        return(cbind(thetaS,thetaT))
    }
    param.meanar=function(object){
        seglen=seg.len(object)
        data=data.set(object)
        n=length(data)-1
        sumstat=cbind(cumsum(c(0,data[-1])),cumsum(c(0,data[-(n+1)])),cumsum(c(0,data[-1]*data[-(n+1)])),cumsum(c(0,data[-1]^2)),cumsum(c(0,data[-(n+1)]^2)))
        ocptsumstat=matrix(sumstat[object@ocpts+1,]-sumstat[c(0,ocpts(object))+1,],ncol=5)
        beta2=(2*seglen*ocptsumstat[,3]-ocptsumstat[,1]*ocptsumstat[,2])/(2*seglen*ocptsumstat[,5]*(1-ocptsumstat[,2]^2));
        beta1=(2*ocptsumstat[,1]-beta2*ocptsumstat[,2])/(2*seglen);
        
        return(cbind(beta1,beta2))
    }
    param.trendar=function(object){
        seglen=seg.len(object)
        data=data.set(object)
        n=length(data)-1
        sumstat=cbind(cumsum(c(0,data[-1])),cumsum(c(0,data[-(n+1)])),cumsum(c(0,data[-1]*data[-(n+1)])),cumsum(c(0,data[-1]*c(1:n))),cumsum(c(0,data[-(n+1)]*c(0:(n-1)))),cumsum(c(0,data[-1]^2)),cumsum(c(0,data[-(n+1)]^2)))
        ocptsumstat=matrix(sumstat[object@ocpts+1,]-sumstat[c(0,ocpts(object))+1,],ncol=7)
        ocptsumstat[,4]=ocptsumstat[,4]-ocptsumstat[,1]*c(0,ocpts(object)) # i.e. creating newx4
        ocptsumstat[,5]=ocptsumstat[,5]-ocptsumstat[,2]*c(0,ocpts(object)) # i.e. creating newx5
        betatop=seglen*(seglen-1)*(seglen*(seglen-1)*ocptsumstat[,3] + 2*(2*seglen+1)*ocptsumstat[,1]*(ocptsumstat[,5]-seglen*ocptsumstat[,2]) + 6*ocptsumstat[,4]*(ocptsumstat[,2]-ocptsumstat[,5]))
        betabottom=seglen*(seglen-1)*ocptsumstat[,7] + 2*(2*seglen+1)*ocptsumstat[,2]*(seglen*ocptsumstat[,2]-ocptsumstat[,5]) + 6*ocptsumstat[,5]*(ocptsumstat[,5]-ocptsumstat[,2]);
        beta=betatop/betabottom;
        thetajpo=(6*(seglen+2)*(ocptsumstat[,4]-beta*ocptsumstat[,5]))/((seglen+1)*(2*seglen+1)) - 2*(ocptsumstat[,1]-beta*ocptsumstat[,2])
        thetaj=(2*(2*seglen+1)*(ocptsumstat[,1]-beta*ocptsumstat[,2])-6*(ocptsumstat[,4]-beta*ocptsumstat[,5]))/(seglen-1)
        
        return(cbind(beta,thetajpo,thetaj))
    }
    if(ocpttype(object)=="mean"){
        param.est(object)<-list(mean=param.mean(object))
    }
    else if(ocpttype(object)=="variance"){
        param.est(object)<-list(variance=param.var(object))
    }
    else if(ocpttype(object)=="mean and variance"){
        if(test.stat(object)=="Normal"){
            param.est(object)<-list(mean=param.mean(object),variance=param.var(object))
        }
        else if(test.stat(object)=="Gamma"){
            param.est(object)<-list(scale=param.scale(object,shape=shape),shape=shape)
        }
        else if(test.stat(object)=="Exponential"){
            param.est(object)<-list(rate=1/param.mean(object))
        }
        else if(test.stat(object)=="Poisson"){
            param.est(object)<-list(lambda=param.mean(object))
        }
        else{
            stop("Unknown test statistic for a change in mean and variance")
        }
    }
    else if(ocpttype(object)=="trend"){
        if(test.stat(object)=="Normal"){
            tmp=param.trend(object)
            param.est(object)<-list(thetaS=tmp[,1],thetaT=tmp[,2])
        }
        else{
            stop("Unknown test statistic for a change in trend")
        }
    }
    else if(ocpttype(object)=="trendar"){
        if(test.stat(object)=="Normal"){
            tmp=param.trendar(object)
            param.est(object)<-list(beta=tmp[,1],thetajpo=tmp[,2],thetaj=tmp[,3])
        }
        else{
            stop("Unknown test statistic for a change in trend+ar")
        }
    }
    else if(ocpttype(object)=="meanar"){
        if(test.stat(object)=="Normal"){
            tmp=param.meanar(object)
            param.est(object)<-list(beta1=tmp[,1],beta2=tmp[,2])
        }
        else{
            stop("Unknown test statistic for a change in mean+ar")
        }
    }
    else{
        stop("Unknown changepoint type, must be 'mean', 'variance', 'mean and variance', 'trend', 'meanar' or 'trendar'.")
    }
    return(object)
})

setMethod("param", "ocpt.range", function(object,nocpts=NA,shape,...) {
    if(is.na(nocpts)){
        ocpts=c(0,object@ocpts)
    }
    else{
        nocpts.full=apply(ocpts.full(object),1,function(x){sum(x>0,na.rm=TRUE)})
        row=try(which(nocpts.full==nocpts),silent=TRUE)
        if(class(row)=='try-error'){
            stop("Your input object doesn't have a segmentation with the requested number of changepoints.")
        }
        ocpts=c(0,ocpts.full(object)[row,1:nocpts],length(data.set(object)))
    }
    
    param.mean=function(object,ocpts){
        nseg=length(ocpts)-1
        data=data.set(object)
        tmpmean=NULL
        for(j in 1:nseg){
            tmpmean[j]=mean(data[(ocpts[j]+1):(ocpts[j+1])])
        }
        return(tmpmean)
    }
    param.var=function(object,ocpts){
        nseg=length(ocpts)-1
        data=data.set(object)
        seglen=seg.len(object)
        tmpvar=NULL
        for(j in 1:nseg){
            tmpvar[j]=var(data[(ocpts[j]+1):(ocpts[j+1])])
        }
        tmpvar=tmpvar*(seglen-1)/seglen
        return(tmpvar)
    }
    param.scale=function(object,ocpts,shape){
        nseg=length(ocpts)-1
        data=data.set(object)
        y=c(0,cumsum(data))
        tmpscale=NULL
        for(j in 1:nseg){
            tmpscale[j]=(y[(ocpts[j+1]+1)]-y[(ocpts[j]+1)])/((ocpts[j+1]-ocpts[j])*shape)
        }
        return(tmpscale)
    }
    param.trend=function(object){
        ocpts=c(0,object@ocpts)
        seglen=seg.len(object)
        data=data.set(object)
        n=length(data)
        sumstat=cbind(cumsum(c(0,data)),cumsum(c(0,data*c(1:n))))
        ocptsumstat=matrix(sumstat[object@ocpts+1,]-sumstat[c(0,ocpts(object))+1,],ncol=2)
        ocptsumstat[,2]=ocptsumstat[,2]-ocptsumstat[,1]*c(0,ocpts(object)) # i.e. creating newx3
        
        thetaS=(2*ocptsumstat[,1]*(2*seglen + 1) - 6*ocptsumstat[,2]) / (2*seglen*(2*seglen + 1) - 3*seglen*(seglen+1))
        thetaT=(6*ocptsumstat[,2])/((seglen+1)*(2*seglen+1)) + (thetaS * (1-((3*seglen)/((2*seglen)+1))))
        return(cbind(thetaS,thetaT))
    }
    param.meanar=function(object){
        seglen=seg.len(object)
        data=data.set(object)
        n=length(data)-1
        sumstat=cbind(cumsum(c(0,data[-1])),cumsum(c(0,data[-(n+1)])),cumsum(c(0,data[-1]*data[-(n+1)])),cumsum(c(0,data[-1]^2)),cumsum(c(0,data[-(n+1)]^2)))
        ocptsumstat=matrix(sumstat[object@ocpts+1,]-sumstat[c(0,ocpts(object))+1,],ncol=5)
        beta2=(2*seglen*ocptsumstat[,3]-ocptsumstat[,1]*ocptsumstat[,2])/(2*seglen*ocptsumstat[,5]*(1-ocptsumstat[,2]^2));
        beta1=(2*ocptsumstat[,1]-beta2*ocptsumstat[,2])/(2*seglen);
        
        return(cbind(beta1,beta2))
    }
    param.trendar=function(object){
        seglen=seg.len(object)
        data=data.set(object)
        n=length(data)-1
        sumstat=cbind(cumsum(c(0,data[-1])),cumsum(c(0,data[-(n+1)])),cumsum(c(0,data[-1]*data[-(n+1)])),cumsum(c(0,data[-1]*c(1:n))),cumsum(c(0,data[-(n+1)]*c(0:(n-1)))))
        ocptsumstat=matrix(sumstat[object@ocpts+1,]-sumstat[c(0,ocpts(object))+1,],ncol=7)
        ocptsumstat[,4]=ocptsumstat[,4]-ocptsumstat[,1]*c(0,ocpts(object)) # i.e. creating newx4
        ocptsumstat[,5]=ocptsumstat[,5]-ocptsumstat[,2]*c(0,ocpts(object)) # i.e. creating newx5
        betatop=seglen*(seglen-1)*(seglen*(seglen-1)*ocptsumstat[,3] + 2*(2*seglen+1)*ocptsumstat[,1]*(ocptsumstat[,5]-seglen*ocptsumstat[,2]) + 6*ocptsumstat[,4]*(ocptsumstat[,2]-ocptsumstat[,5]))
        betabottom=seglen*(seglen-1)*ocptsumstat[,7] + 2*(2*seglen+1)*ocptsumstat[,2]*(seglen*ocptsumstat[,2]-ocptsumstat[,5]) + 6*ocptsumstat[,5]*(ocptsumstat[,5]-ocptsumstat[,2]);
        beta=betatop/betabottom;
        thetajpo=(6*(seglen+2)*(ocptsumstat[,4]-beta*ocptsumstat[,5]))/((seglen+1)*(2*seglen+1)) - 2*(ocptsumstat[,1]-beta*ocptsumstat[,2])
        thetaj=(2*(2*seglen+1)*(ocptsumstat[,1]-beta*ocptsumstat[,2])-6*(ocptsumstat[,4]-beta*ocptsumstat[,5]))/(seglen-1)
        
        return(cbind(beta,thetajpo,thetaj))
    }
    
    if(ocpttype(object)=="mean"){
        param.est<-list(mean=param.mean(object,ocpts))
    }
    else if(ocpttype(object)=="variance"){
        param.est<-list(variance=param.var(object,ocpts))
    }
    else if(ocpttype(object)=="mean and variance"){
        if(test.stat(object)=="Normal"){
            param.est<-list(mean=param.mean(object,ocpts),variance=param.var(object,ocpts))
        }
        else if(test.stat(object)=="Gamma"){
            param.est<-list(scale=param.scale(object,ocpts,shape=shape),shape=shape)
        }
        else if(test.stat(object)=="Exponential"){
            param.est<-list(rate=1/param.mean(object,ocpts))
        }
        else if(test.stat(object)=="Poisson"){
            param.est<-list(lambda=param.mean(object,ocpts))
        }
        else{
            stop("Unknown test statistic for a change in mean and variance")
        }
    }
    else if(ocpttype(object)=="trend"){
        if(test.stat(object)=="Normal"){
            tmp=param.trend(object)
            param.est(object)<-list(thetaS=tmp[,1],thetaT=tmp[,2])
        }
        else{
            stop("Unknown test statistic for a change in trend")
        }
    }
    else if(ocpttype(object)=="trendar"){
        if(test.stat(object)=="Normal"){
            tmp=param.trendar(object)
            param.est(object)<-list(beta=tmp[,1],thetajpo=tmp[,2],thetaj=tmp[,3])
        }
        else{
            stop("Unknown test statistic for a change in trend+ar")
        }
    }
    else if(ocpttype(object)=="meanar"){
        if(test.stat(object)=="Normal"){
            tmp=param.meanar(object)
            param.est(object)<-list(beta1=tmp[,1],beta2=tmp[,2])
        }
        else{
            stop("Unknown test statistic for a change in mean+ar")
        }
    }
    else{
        stop("Unknown changepoint type, must be 'mean', 'variance', 'mean and variance', 'trend', 'meanar' or 'trendar'")
    }
    if(is.na(nocpts)){
        param.est(object)=param.est
        return(object)
    }
    out=new('ocpt.range')
    param.est(out)=param.est
    return(out)
})

setMethod("param", "ocpt.reg", function(object,shape,...) {
    param.norm=function(object){
        ocpts=c(0,object@ocpts)
        #    nseg=length(ocpts)-1 #nseg(object)
        data=data.set(object)
        p=ncol(data)-1
        tmpbeta=matrix(NA,ncol=p,nrow=nseg(object))
        tmpsigma=rep(NA,nseg(object))
        for(j in 1:nseg(object)){
            formula=paste('-1+data[',ocpts[j]+1,':',ocpts[j+1],',2]',sep='')
            if(p>1){
                for(i in 2:p){
                    formula=paste(formula,'+data[',(ocpts[j]+1),':',ocpts[j+1],',',i+1,']',sep='')
                }
            }
            tmpfit=eval(parse(text=paste('lm(data[',(ocpts[j]+1),':',ocpts[j+1],',1]~',formula,')',sep='')))
            tmpbeta[j,]=tmpfit$coefficients
            tmpsigma[j]=var(tmpfit$residuals)
        }
        return(list(beta=tmpbeta,sig2=tmpsigma))
    }
    if(test.stat(object)=="Normal"){
        param.est(object)<-param.norm(object)
    }
    else{
        stop("Unknown test statistic, must be 'Normal'")
    }
    return(object)
})

# summary functions
setMethod("summary","ocpt",function(object){
    cat("Created Using changepoint version",object@version,'\n')
    cat("Changepoint type      : Change in",ocpttype(object),'\n')
    cat("Method of analysis    :",method(object),"\n")
    cat("Test Statistic  :", test.stat(object),"\n")
    cat("Type of penalty       :", pen.type(object), "with value,",pen.value(object),"\n")
    cat("Minimum Segment Length :", minseglen(object),"\n")
    cat("Maximum no. of cpts   :", nocpts.max(object),"\n")
    if(length(ocpts(object))<=20){cat("Changepoint Locations :",ocpts(object),"\n")}
    else{cat("Number of changepoints:", nocpts(object),"\n")}
})

setMethod("summary","ocpt.range",function(object){
    cat("Created Using changepoint version",object@version,'\n')
    cat("Changepoint type      : Change in",ocpttype(object),'\n')
    cat("Method of analysis    :",method(object),"\n")
    cat("Test Statistic  :", test.stat(object),"\n")
    cat("Type of penalty       :", pen.type(object), "with value,",pen.value(object),"\n")
    cat("Minimum Segment Length :", minseglen(object),"\n")
    cat("Maximum no. of cpts   :", nocpts.max(object),"\n")
    if(length(ocpts(object))<=20){cat("Changepoint Locations :",ocpts(object),"\n")}
    else{cat("Number of changepoints:", nocpts(object),"\n")}
    if((nrow(ocpts.full(object))<=5)&(ncol(ocpts.full(object)<=20))){cat("Range of segmentations:\n");print(ocpts.full(object));cat("\n For penalty values:", pen.value.full(object),"\n")}
    else{cat("Number of segmentations recorded:", nrow(ocpts.full(object)), " with between ", sum(ocpts.full(object)[nrow(ocpts.full(object)),]>0,na.rm=T), " and ", sum(ocpts.full(object)[1,]>0,na.rm=T), "changepoints.\n Penalty value ranges from:",min(pen.value.full(object))," to ",max(pen.value.full(object)))}
})

setMethod("summary","ocpt.reg",function(object){
    cat("Created Using changepoint version",object@version,'\n')
    cat("Changepoint type     : Change in",ocpttype(object),'\n')
    cat("Method of analysis   :",method(object),"\n")
    cat("Test Statistic :", test.stat(object),"\n")
    cat("Type of penalty      :", pen.type(object), "with value,",pen.value(object),"\n")
    cat("Maximum no. of cpts   :", nocpts.max(object),"\n")
    if(length(ocpts(object))<=20){cat("Changepoint Locations :",ocpts(object),"\n")}
    else{cat("Number of changepoints:", nocpts(object),"\n")}
})

# show functions
setMethod("show","ocpt",function(object){
    cat("Class 'ocpt' : Changepoint Object\n")
    cat("       ~~   : S4 class containing", length(attributes(object))-1, "slots with names\n")
    cat("             ", names(attributes(object))[1:(length(attributes(object))-1)], "\n\n")
    cat("Created on  :", object@date, "\n\n")
    cat("summary(.)  :\n----------\n")
    summary(object)
})
setMethod("show","ocpt.reg",function(object){
    cat("Class 'ocpt.reg' : Changepoint Regression Object\n")
    cat("       ~~   : S4 class containing", length(attributes(object))-1, "slots with names\n")
    cat("             ", names(attributes(object))[1:(length(attributes(object))-1)], "\n\n")
    cat("Created on  :", object@date, "\n\n")
    cat("summary(.)  :\n----------\n")
    summary(object)
})

# plot functions
setMethod("plot","ocpt",function(x,ocpt.col='red',ocpt.width=1,ocpt.style=1,...){
    if(length(param.est(x))==0){# i.e. parameter.estimates=FALSE in call
        cat('Calculating parameter estimates...')
        object=param(x)
        cat('done.\n')
    }
    plot(data.set.ts(x),...)
    if(ocpttype(x)=="variance"){
        abline(v=index(data.set.ts(x))[ocpts(x)],col=ocpt.col,lwd=ocpt.width,lty=ocpt.style)
    }
    else if(ocpttype(x)=="mean"  ||  ocpttype(x)=="mean and variance"){
        #nseg=length(ocpts(x))+1
        ocpts=c(0,x@ocpts)
        if((test.stat(x)=="Normal")||(test.stat(x)=="CUSUM")){
            means=param.est(x)$mean
        }
        else if(test.stat(x)=="Gamma"){
            means=param.est(x)$scale*param.est(x)$shape
        }
        else if(test.stat(x)=="Exponential"){
            means=1/param.est(x)$rate
        }
        else if(test.stat(x)=="Poisson"){
            means=param.est(x)$lambda
        }
        else{
            stop('Invalid Changepoint test statistic')
        }
        for(i in 1:nseg(x)){
            segments(index(data.set.ts(x))[ocpts[i]+1],means[i],index(data.set.ts(x))[ocpts[i+1]],means[i],ocol=ocpt.col,lwd=ocpt.width,lty=ocpt.style)
        }
    }
    else if(ocpttype(x)=="trend"){
        ocpts=c(0,x@ocpts)
        intercept=rep(param.est(x)$thetaS,x@ocpts-c(0,ocpts(x)))
        slope=rep(param.est(x)$thetaT-param.est(x)$thetaS,x@ocpts-c(0,ocpts(x)))/rep(x@ocpts-c(0,ocpts(x)),x@ocpts-c(0,ocpts(x)))
        ocptn=rep(c(0,ocpts(x)),x@ocpts-c(0,ocpts(x)))
        n=length(data.set(x))
        means=intercept+slope*((1:n)-ocptn)
        for(i in 1:nseg(x)){
            segments(index(data.set.ts(x))[ocpts[i]+1],means[ocpts[i]+1],index(data.set.ts(x))[ocpts[i+1]],means[ocpts[i+1]],col=ocpt.col,lwd=ocpt.width,lty=ocpt.style)
        }
    }
    else{
        stop('Invalid Changepoint Type for plotting.\n Can only plot mean, variance, mean and variance')
    }
})

setMethod("plot","ocpt.range",function(x,nocpts=NA,diagnostic=FALSE,ocpt.col='red',ocpt.width=1,ocpt.style=1,...){
    if(diagnostic==TRUE){
        return(plot(apply(ocpts.full(x),1,function(x){sum(x>0,na.rm=TRUE)}),pen.value.full(x),type='l',xlab='Number of Changepoints',ylab='Difference in Test Statistic',...))
    }
    plot(data.set.ts(x),...)
    if(is.na(nocpts)){
        if(pen.type(x)=="CROPS"){
            stop('CROPS does not supply an optimal set of changepoints, set nocpts to the desired segmentation to plot or use diagnostic=TRUE to identify an appropriate number of changepoints')
        }
        ocpts.to.plot=ocpts(x)
        param.est=x
    }
    else{
        nocpts.full=apply(ocpts.full(x),1,function(x){sum(x>0,na.rm=TRUE)})
        row=which(nocpts.full==nocpts)
        if(length(row)==0){
            stop(paste("Your input object doesn't have a segmentation with the requested number of changepoints.\n Possible nocpts are: "),paste(nocpts.full,collapse=','))
        }
        ocpts.to.plot=ocpts.full(x)[row,1:nocpts]
        if(test.stat(x)=="Gamma"){
            param.est=param(x,nocpts,shape=param.est(x)$shape)
        }
        else{
            param.est=param(x,nocpts)
        }
    }
    if(ocpttype(x)=="variance"){
        abline(v=index(data.set.ts(x))[ocpts.to.plot],col=ocpt.col,lwd=ocpt.width,lty=ocpt.style)
    }
    else if(ocpttype(x)=="mean"  ||  ocpttype(x)=="mean and variance"){
        if((test.stat(x)=="Normal")||(test.stat(x)=="CUSUM")){
            means=param.est(param.est)$mean
        }
        else if(test.stat(x)=="Gamma"){
            means=param.est(param.est)$scale*param.est(param.est)$shape
        }
        else if(test.stat(x)=="Exponential"){
            means=1/param.est(param.est)$rate
        }
        else if(test.stat(x)=="Poisson"){
            means=param.est(param.est)$lambda
        }
        else{
            stop('Invalid Changepoint test statistic')
        }
        nseg=nocpts+1
        ocpts.to.plot=c(0,ocpts.to.plot,length(data.set(x)))
        for(i in 1:nseg){
            segments(index(data.set.ts(x))[ocpts.to.plot[i]+1],means[i],index(data.set.ts(x))[ocpts.to.plot[i+1]],means[i],col=ocpt.col,lwd=ocpt.width,lty=ocpt.style)
        }
    }
    else{
        stop('Invalid Changepoint Type for plotting.\n Can only plot mean, variance, mean and variance')
    }
})

setMethod("plot","ocpt.reg",function(x,ocpt.col='red',ocpt.width=1,ocpt.style=1,...){
    if(length(param.est(x))==0){# i.e. parameter.estimates=FALSE in call
        cat('Calculating parameter estimates...')
        object=param(x)
        cat('done.\n')
    }
    plot(data.set(x)[,1],type='l',...)
    if(test.stat(x)=="Normal"){
        ocpts=c(0,x@ocpts)
        betas=param.est(x)$beta
        for(i in 1:nseg(x)){
            lines((ocpts[i]+1):ocpts[i+1],betas[i,]%*%t(data.set(x)[(ocpts[i]+1):ocpts[i+1],-1]),col=ocpt.col,lwd=ocpt.width,lty=ocpt.style)
        }
    }
    else{
        stop('Invalid Changepoint test statistic')
    }
})

# likelihood functions
setMethod("logLik", "ocpt", function(object) {
    if(length(param.est(object))==0){# i.e. parameter.estimates=FALSE in call
        cat('Calculating parameter estimates...')
        object=param(object)
        cat('done.\n')
    }
    if(test.stat(object)=="Normal"){
        if(ocpttype(object)=="mean"){
            means=rep(param.est(object)$mean,object@ocpts-c(0,ocpts(object)))
            rss=sum((data.set(object)-means)^2)
            n=length(data.set(object))
            like=n*(log(2*pi)+log(rss/n)+1) # -2*loglik
            if(pen.type(object)=="MBIC"){
                like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
            }else{
                like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
            }
        }
        else if(ocpttype(object)=="variance"){
            rss=c(0,cumsum((data.set(object)-param.est(object)$mean)^2))
            ocpts=c(0,object@ocpts)
            n=length(data.set(object))
            seglen=seg.len(object)
            sigmas=(rss[ocpts[-1]+1]-rss[ocpts[-length(ocpts)]+1])/seglen
            like=n*log(2*pi)+sum(seglen*log(sigmas))+n
            if(pen.type(object)=="MBIC"){
                like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(seglen)))
            }else{
                like=c(like,like+(nseg(object)-1)*pen.value(object))
            }
        }
        else if(ocpttype(object)=="mean and variance"){
            means=rep(param.est(object)$mean,object@ocpts-c(0,ocpts(object)))
            rss=sum((data.set(object)-means)^2)
            n=length(data.set(object))
            ocpts=c(0,object@ocpts)
            seglen=seg.len(object)
            sigmas=param.est(object)$variance
            like=n*log(2*pi)+sum(seglen*log(sigmas))+n
            if(pen.type(object)=="MBIC"){
                like=c(like,like+(nseg(object)-2)*pen.value(object)+sum(log(seglen)))
            }else{
                like=c(like,like+(nseg(object)-1)*pen.value(object))
            }
        }
        else if(ocpttype(object)=="trend"){
            intercept=rep(param.est(object)$thetaS,object@ocpts-c(0,ocpts(object)))
            slope=rep(param.est(object)$thetaT-param.est(object)$thetaS,object@copts-c(0,ocpts(object)))/rep(object@ocpts-c(0,ocpts(object)),object@ocpts-c(0,ocpts(object)))
            ocptn=rep(c(0,ocpts(object)),object@ocpts-c(0,ocpts(object)))
            n=length(data.set(object))
            means=intercept+slope*((1:n)-ocptn)
            rss=sum((data.set(object)-means)^2)
            like=n*(log(2*pi)+log(rss/n)+1) # -2*loglik
            if(pen.type(object)=="MBIC"){
                like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
            }else{
                like=c(like,like+(nseg(object)-1)*pen.value(object))
            }
        }
        else if(ocpttype(object)=="trendar"){
            seglen=seg.len(object)
            intercept=rep(param.est(object)$thetaj,seglen)
            slope=rep(param.est(object)$thetajpo-param.est(object)$thetaj,seglen)/rep(seglen,seglen)
            ar=rep(param.est(object)$beta,seglen)
            ocptn=rep(c(0,ocpts(object)),seglen)
            n=length(data.set(object))
            means=NULL;means[1]=0
            for(i in 2:n){means[i]=intercept+slope*((1:n)-ocptn)+ar*means[i-1]}
            means=means[-1]
            rss=sum((data.set(object)[-1]-means)^2)
            like=n*(log(2*pi)+log(rss/n)+1) # -2*loglik
            if(pen.type(object)=="MBIC"){
                like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
            }else{
                like=c(like,like+(nseg(object)-1)*pen.value(object))
            }
        }
        else if(ocpttype(object)=="meanar"){
            seglen=seg.len(object)
            intercept=rep(param.est(object)$beta1,seglen)
            ar=rep(param.est(object)$beta2,seglen)
            ocptn=rep(c(0,ocpts(object)),seglen)
            n=length(data.set(object))
            means[1]=0;for(i in 2:n){means[i]=intercept+ar*means[i-1]}
            means=means[-1]
            rss=sum((data.set(object)[-1]-means)^2)
            like=n*(log(2*pi)+log(rss/n)+1) # -2*loglik
            if(pen.type(object)=="MBIC"){
                like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
            }else{
                like=c(like,like+(nseg(object)-1)*pen.value(object))
            }
        }
        else{
            stop("Unknown changepoint type, must be 'mean', 'variance', 'mean and variance', 'trend', 'meanar' or 'trendar'")
        }
    }
    else if(test.stat(object)=="Gamma"){
        if(ocpttype(object)!="mean and variance"){
            stop("Unknown changepoint type for test.stat='Gamma', must be 'mean and variance'")
        }
        else{
            warning("Not changed to be -2*logLik")
            mll.meanvarg=function(x,n,shape){
                return(n*shape*log(n*shape)-n*shape*log(x))
            }
            y=c(0,cumsum(data.set(object)))
            shape=param.est(object)$shape
            ocpts=c(0,object@ocpts)
            #nseg=length(ocpts)-1
            tmplike=0
            for(j in 1:nseg(object)){
                tmplike=tmplike+mll.meanvarg(y[ocpts[j+1]+1]-y[ocpts[j]+1],ocpts[j+1]-ocpts[j],shape)
            }
            if(pen.type(object)=="MBIC"){
                like=c(tmplike, tmplike+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
            }else{
                like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
            }
        }
    }
    else if(test.stat(object)=="Exponential"){
        if(ocpttype(object)!="mean and variance"){
            stop("Unknown changepoint type for test.stat='Exponential', must be 'mean and variance'")
        }
        else{
            warning("Not changed to be -2*logLik")
            mll.meanvare=function(x,n){
                return(n*log(n)-n*log(x))
            }
            y=c(0,cumsum(data.set(object)))
            ocpts=c(0,object@ocpts)
            #nseg=length(ocpts)-1
            tmplike=0
            for(j in 1:nseg(object)){
                tmplike=tmplike+mll.meanvare(y[ocpts[j+1]+1]-y[ocpts[j]+1],ocpts[j+1]-ocpts[j])
            }
            if(pen.type(object)=="MBIC"){
                like=c(tmplike, tmplike+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
            }else{
                like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
            }
        }
    }
    else if(test.stat(object)=="Poisson"){
        if(ocpttype(object)!="mean and variance"){
            stop("Unknown changepoint type for test.stat='Poisson', must be 'mean and variance'")
        }
        else{
            warning("Not changed to be -2*logLik")
            mll.meanvarp=function(x,n){
                return(x*log(x)-x*log(n))
            }
            y=c(0,cumsum(data.set(object)))
            ocpts=c(0,object@ocpts)
            #nseg=length(ocpts)-1
            tmplike=0
            for(j in 1:nseg(object)){
                tmplike=tmplike+mll.meanvarp(y[ocpts[j+1]+1]-y[ocpts[j]+1],ocpts[j+1]-ocpts[j])
            }
            if(pen.type(object)=="MBIC"){
                like=c(tmplike, tmplike+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
            }else{
                like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
            }
        }
    }
    else{stop("logLik is only valid for distributional assumptions, not CUSUM or CSS")}
    names(like)=c("-like","-likepen")
    return(like)
})

setMethod("logLik", "ocpt.range", function(object,nocpts=NA) {
    warning("Not changed to be -2*logLik")
    if(is.na(nocpts)){
        if(pen.type(object)=="CROPS"){
            stop('CROPS does not supply an optimal set of changepoints, set nocpts argument to the desired segmentation to plot or use diagnostic=TRUE to identify an appropriate number of changepoints')
        }
        ocpts=c(0,object@ocpts)
        pen.value=pen.value(object)
    }
    else{
        nocpts.full=apply(ocpts.full(object),1,function(x){sum(x>0,na.rm=TRUE)})
        row=which(nocpts.full==nocpts)
        if(length(row)==0){
            stop(paste("Your input object doesn't have a segmentation with the requested number of changepoints.\n Possible nocpts are: "),paste(nocpts.full,collapse=','))
        }
        ocpts=c(0,ocpts.full(object)[row,1:nocpts],length(data.set(object)))
        pen.value=pen.value.full(object)[row]
    }
    nseg=length(ocpts)-1
    
    if(test.stat(object)=="Normal"){
        if(ocpttype(object)=="mean"){
            mll.mean=function(x2,x,n){
                return( x2-(x^2)/n)
            }
            y2=c(0,cumsum(data.set(object)^2))
            y=c(0,cumsum(data.set(object)))
            tmplike=0
            for(j in 1:nseg){
                tmplike=tmplike+mll.mean(y2[ocpts[j+1]+1]-y2[ocpts[j]+1],y[ocpts[j+1]+1]-y[ocpts[j]+1],ocpts[j+1]-ocpts[j])
            }
            ##c(tmplike, tmplike+(nseg-2)*pen.value(object)+sum(log(ocpts[-1]-ocpts[-(nseg+1)])))
            if(pen.type(object)=="MBIC"){
                like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(seg.len(object))))
            }else{
                like=c(tmplike,tmplike+(nseg-1)*pen.value)
            }
            names(like)=c("-like","-likepen")
        }
        else if(ocpttype(object)=="variance"){
            mll.var=function(x,n){
                neg=x<=0
                x[neg==TRUE]=0.00000000001
                return( n*(log(2*pi)+log(x/n)+1))
            }
            y2=c(0,cumsum((data.set(object)-param.est(object)$mean)^2))
            tmplike=0
            for(j in 1:nseg){
                tmplike=tmplike+mll.var(y2[ocpts[j+1]+1]-y2[ocpts[j]+1],ocpts[j+1]-ocpts[j])
            }
            if(pen.type(object)=="MBIC"){
                like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(seg.len(object))))
            }else{
                like=c(tmplike,tmplike+(nseg-1)*pen.value)
            }
            names(like)=c("-like","-likepen")
        }
        else if(ocpttype(object)=="mean and variance"){
            mll.meanvar=function(x2,x,n){
                sigmasq=(1/n)*(x2-(x^2)/n)
                neg=sigmasq<=0
                sigmasq[neg==TRUE]=0.00000000001
                return( n*(log(2*pi)+log(sigmasq)+1))
            }
            y2=c(0,cumsum(data.set(object)^2))
            y=c(0,cumsum(data.set(object)))
            tmplike=0
            for(j in 1:nseg){
                tmplike=tmplike+mll.meanvar(y2[ocpts[j+1]+1]-y2[ocpts[j]+1],y[ocpts[j+1]+1]-y[ocpts[j]+1],ocpts[j+1]-ocpts[j])
            }
            if(pen.type(object)=="MBIC"){
                like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(seg.len(object))))
            }else{
                like=c(tmplike,tmplike+(nseg-1)*pen.value)
            }
            names(like)=c("-like","-likepen")
        }
        else{
            stop("Unknown changepoint type, must be 'mean', 'variance' or 'mean and variance'")
        }
    }
    else if(test.stat(object)=="Gamma"){
        if(ocpttype(object)!="mean and variance"){
            stop("Unknown changepoint type for test.stat='Gamma', must be 'mean and variance'")
        }
        else{
            mll.meanvarg=function(x,n,shape){
                return(n*shape*log(n*shape)-n*shape*log(x))
            }
            y=c(0,cumsum(data.set(object)))
            shape=param.est(object)$shape
            ocpts=c(0,object@ocpts)
            #nseg=length(ocpts)-1
            tmplike=0
            for(j in 1:nseg){
                tmplike=tmplike+mll.meanvarg(y[ocpts[j+1]+1]-y[ocpts[j]+1],ocpts[j+1]-ocpts[j],shape)
            }
            if(pen.type(object)=="MBIC"){
                like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(seg.len(object))))
            }else{
                like=c(tmplike,tmplike+(nseg-1)*pen.value)
            }
            names(like)=c("-like","-likepen")
        }
    }
    else if(test.stat(object)=="Exponential"){
        if(ocpttype(object)!="mean and variance"){
            stop("Unknown changepoint type for test.stat='Exponential', must be 'mean and variance'")
        }
        else{
            mll.meanvare=function(x,n){
                return(n*log(n)-n*log(x))
            }
            y=c(0,cumsum(data.set(object)))
            ocpts=c(0,object@ocpts)
            #nseg=length(ocpts)-1
            tmplike=0
            for(j in 1:nseg){
                tmplike=tmplike+mll.meanvare(y[ocpts[j+1]+1]-y[ocpts[j]+1],ocpts[j+1]-ocpts[j])
            }
            if(pen.type(object)=="MBIC"){
                like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(seg.len(object))))
            }else{
                like=c(tmplike,tmplike+(nseg-1)*pen.value)
            }
            names(like)=c("-like","-likepen")
        }
    }
    else if(test.stat(object)=="Poisson"){
        if(ocpttype(object)!="mean and variance"){
            stop("Unknown changepoint type for test.stat='Poisson', must be 'mean and variance'")
        }
        else{
            mll.meanvarp=function(x,n){
                return(x*log(x)-x*log(n))
            }
            y=c(0,cumsum(data.set(object)))
            ocpts=c(0,object@ocpts)
            #nseg=length(ocpts)-1
            tmplike=0
            for(j in 1:nseg){
                tmplike=tmplike+mll.meanvarp(y[ocpts[j+1]+1]-y[ocpts[j]+1],ocpts[j+1]-ocpts[j])
            }
            if(pen.type(object)=="MBIC"){
                like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(seg.len(object))))
            }else{
                like=c(tmplike,tmplike+(nseg-1)*pen.value)
            }
            names(like)=c("-like","-likepen")
        }
    }
    else{stop("logLik is only valid for distributional assumptions, not CUSUM or CSS")}
    return(like)
})

setMethod("logLik", "ocpt.reg", function(object) {
    if(length(param.est(object))==0){# i.e. parameter.estimates=FALSE in call
        cat('Calculating parameter estimates...')
        object=param(object)
        cat('done.\n')
    }
    if(test.stat(object)=="Normal"){
        ocpts=c(0,object@ocpts)
        seglen=seg.len(object)
        data=data.set(object)
        beta=param.est(object)$beta
        sigmas=param.est(object)$sig2
        rss=NULL
        for(i in 1:length(seglen)){
            rss[i]=sum((data[(ocpts[i]+1):ocpts[i+1],1]-data[(ocpts[i]+1):ocpts[i+1],-1]%*%beta[i,])^2)
        }
        like=sum(seglen*log(2*pi*sigmas))+sum(rss/sigmas)
        if(pen.type(object)=="MBIC"){
            like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
        }else{
            like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
        }
    }
    else{stop("logLik is only valid for Normal distributional assumption.")}
    return(like)
})

setGeneric("likelihood", function(object) standardGeneric("likelihood"))
setMethod("likelihood", "ocpt", function(object) {
    return(logLik(object))
})

# acf functions
setGeneric("acf", function(object,...) standardGeneric("acf"))
setMethod("acf", "ocpt", function(object,lag.max=NULL,...) {
    ocpts=c(0,object@ocpts)
    nseg=nseg(object)
    data=data.set(object)
    for(i in 1:nseg){
        stats::acf(data[(ocpts[i]+1):ocpts[i+1]],main=paste("Series part:",(ocpts[i]+1),":",ocpts[i+1]),...)
    }
})

setMethod("acf", "ocpt.reg", function(object,lag.max=NULL,...) {
    ocpts=c(0,object@ocpts)
    nseg=nseg(object)
    data=data.set(object)[,1]
    for(i in 1:nseg){
        stats::acf(data[(ocpts[i]+1):ocpts[i+1]],main=paste("Series part:",(ocpts[i]+1),"-",ocpts[i+1]),...)
    }
    })
