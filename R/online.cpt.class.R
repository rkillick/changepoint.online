setClass("ocpt",slots=list(data.set="ts", cpttype="character", method="character",     test.stat="character",pen.type="character",pen.value="numeric",minseglen="numeric",cpts="numeric",ncpts.max="numeric",param.est="list",date="character",version="character",lastchangelike="numeric",lastchangecpts="numeric",numchangecpts="numeric",checklist="numeric",ndone="numeric",nupdate="numeric",cost_func="character",shape="numeric"),prototype=prototype(cpttype="Not Set",date=date(),version=as(packageVersion("changepoint.online"),'character')))

setClass("ocpt.reg",slots=list(data.set="matrix", cpttype="character", method="character", test.stat="character",pen.type="character",pen.value="numeric",minseglen="numeric",cpts="numeric",ncpts.max="numeric",param.est="list",date="character",version="character",lastchangelike="numeric",lastchangecpts="numeric",numchangecpts="numeric",checklist="numeric",ndone="numeric",nupdate="numeric",cost_func="character",shape="numeric"),prototype=prototype(cpttype="Not Set",date=date(),version=as(packageVersion("changepoint.online"),'character')))

#  setClass("ocpt", representation(), prototype())
#  cpts is the optimal segementation
#

setClass("ocpt.range",slots=list(cpts.full="matrix", pen.value.full="numeric"), prototype=prototype(), contains="ocpt")
# cpts.full is the entire matrix
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

if(!isGeneric("cpttype")) {
    if (is.function("cpttype")){
        fun <- cpttype
    }
    else {fun <- function(object){
        standardGeneric("cpttype")
    }
    }
    setGeneric("cpttype", fun)
}
setMethod("cpttype","ocpt",function(object) object@cpttype)
setMethod("cpttype","ocpt.reg",function(object) object@cpttype)

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

if(!isGeneric("cpts")) {
    if (is.function("cpts")){
        fun <- cpts
    }
    else {fun <- function(object){
        standardGeneric("cpts")
    }
    }
    setGeneric("cpts", fun)
}
setMethod("cpts","ocpt",function(object) object@cpts[-length(object@cpts)])
setMethod("cpts","ocpt.reg",function(object) object@cpts[-length(object@cpts)])

if(!isGeneric("cpts.full")) {
    if (is.function("cpts.full")){
        fun <- cpts.full
    }
    else {fun <- function(object){
        standardGeneric("cpts.full")
    }
    }
    setGeneric("cpts.full", fun)
}
setMethod("cpts.full","ocpt.range",function(object) object@cpts.full)

if(!isGeneric("cpts.ts")) {
    if (is.function("cpts.ts")){
        fun <- cpts.ts
    }
    else {fun <- function(object){
        standardGeneric("cpts.ts")
    }
    }
    setGeneric("cpts.ts", fun)
}
setMethod("cpts.ts","ocpt",function(object) index(data.set.ts(object))[cpts(object)] )

if(!isGeneric("ncpts.max")) {
    if (is.function("ncpts.max")){
        fun <- ncpts.max
    }
    else {fun <- function(object){
        standardGeneric("ncpts.max")
    }
    }
    setGeneric("ncpts.max", fun)
}
setMethod("ncpts.max","ocpt",function(object) object@ncpts.max)
setMethod("ncpts.max","ocpt.reg",function(object) object@ncpts.max)

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

# ncpts function
if(!isGeneric("ncpts")) {
    if (is.function("ncpts")){
        fun <- ncpts
    }
    else {fun <- function(object){
        standardGeneric("ncpts")
    }
    }
    setGeneric("ncpts", fun)
}
setMethod("ncpts","ocpt",function(object) length(cpts(object)))
setMethod("ncpts","ocpt.reg",function(object) length(cpts(object)))

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
setMethod("seg.len","ocpt",function(object){object@cpts-c(0,object@cpts[-length(object@cpts)])})
setMethod("seg.len","ocpt.reg",function(object){object@cpts-c(0,object@cpts[-length(object@cpts)])})
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
setMethod("nseg","ocpt",function(object){ncpts(object)+1})
setMethod("nseg","ocpt.reg",function(object){ncpts(object)+1})

#lastchangelike function
if(!isGeneric("lastchangelike")) {
    if (is.function("lastchangelike")){
        fun <- lastchangelike
    }
    else {fun <- function(object){
        standardGeneric("lastchangelike")
    }
    }
    setGeneric("lastchangelike", fun)
}
setMethod("lastchangelike","ocpt",function(object) object@lastchangelike)
setMethod("lastchangelike","ocpt.reg",function(object) object@lastchangelike)

#lastchangecpts function
if(!isGeneric("lastchangecpts")) {
    if (is.function("lastchangecpts")){
        fun <- lastchangecpts
    }
    else {fun <- function(object){
        standardGeneric("lastchangecpts")
    }
    }
    setGeneric("lastchangecpts", fun)
}
setMethod("lastchangecpts","ocpt",function(object) object@lastchangecpts)
setMethod("lastchangecpts","ocpt.reg",function(object) object@lastchangecpts)

#numchangecpts function
if(!isGeneric("numchangecpts")) {
    if (is.function("numchangecpts")){
        fun <- numchangecpts
    }
    else {fun <- function(object){
        standardGeneric("numchangecpts")
    }
    }
    setGeneric("numchangecpts", fun)
}
setMethod("numchangecpts","ocpt",function(object) object@numchangecpts)
setMethod("numchangecpts","ocpt.reg",function(object) object@numchangecpts)

#checklist function
if(!isGeneric("checklist")) {
    if (is.function("checklist")){
        fun <- checklist
    }
    else {fun <- function(object){
        standardGeneric("checklist")
    }
    }
    setGeneric("checklist", fun)
}
setMethod("checklist","ocpt",function(object) object@checklist)
setMethod("checklist","ocpt.reg",function(object) object@checklist)

#ndone
if(!isGeneric("ndone")) {
    if (is.function("ndone")){
        fun <- ndone
    }
    else {fun <- function(object){
        standardGeneric("ndone")
    }
    }
    setGeneric("ndone", fun)
}
setMethod("ndone","ocpt",function(object) object@ndone)
setMethod("ndone","ocpt.reg",function(object) object@ndone)

#nupdate
if(!isGeneric("nupdate")) {
    if (is.function("nupdate")){
        fun <- nupdate
    }
    else {fun <- function(object){
        standardGeneric("nupdate")
    }
    }
    setGeneric("nupdate", fun)
}
setMethod("nupdate","ocpt",function(object) object@nupdate)
setMethod("nupdate","ocpt.reg",function(object) object@nupdate)

#cost_func
if(!isGeneric("cost_func")) {
    if (is.function("cost_func")){
        fun <- ndone
    }
    else {fun <- function(object){
        standardGeneric("cost_func")
    }
    }
    setGeneric("cost_func", fun)
}
setMethod("cost_func","ocpt",function(object) object@cost_func)
setMethod("cost_func","ocpt.reg",function(object) object@cost_func)

#shape
if(!isGeneric("shape")) {
    if (is.function("shape")){
        fun <- shape
    }
    else {fun <- function(object){
        standardGeneric("shape")
    }
    }
    setGeneric("shape", fun)
}
setMethod("shape","ocpt",function(object) object@shape)
setMethod("shape","ocpt.reg",function(object) object@shape)


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

setGeneric("cpttype<-", function(object, value) standardGeneric("cpttype<-"))
setReplaceMethod("cpttype", "ocpt", function(object, value) {
    object@cpttype <- value
    return(object)
})
setReplaceMethod("cpttype", "ocpt.reg", function(object, value) {
    object@cpttype <- value
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

setGeneric("cpts<-", function(object, value) standardGeneric("cpts<-"))
setReplaceMethod("cpts", "ocpt", function(object, value) {
    if((cpttype(object)=="meanar")|(cpttype(object)=="trendar")){
        n=length(object@data.set) - 1
    }
    else{n=length(object@data.set)}
    
    if(value[length(value)]==n){object@cpts <- value}
    else{        object@cpts <- c(value,n)  }
    return(object)
})
setReplaceMethod("cpts", "ocpt.reg", function(object, value) {
    if(value[length(value)]==nrow(object@data.set)){object@cpts <- value}
    else{      object@cpts <- c(value,nrow(object@data.set))  }
    return(object)
})

setGeneric("ncpts.max<-", function(object, value) standardGeneric("ncpts.max<-"))
setReplaceMethod("ncpts.max", "ocpt", function(object, value) {
    object@ncpts.max <- value
    return(object)
})
setReplaceMethod("ncpts.max", "ocpt.reg", function(object, value) {
    object@ncpts.max <- value
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

setGeneric("cpts.full<-", function(object, value) standardGeneric("cpts.full<-"))
setReplaceMethod("cpts.full", "ocpt.range", function(object, value) {
    object@cpts.full <- value
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


#lastchangelike
setGeneric("lastchangelike<-", function(object, value) standardGeneric("lastchangelike<-"))
setReplaceMethod("lastchangelike", "ocpt", function(object, value) {
    object@lastchangelike <- value
    return(object)
})
setReplaceMethod("lastchangelike", "ocpt.reg", function(object, value) {
    object@lastchangelike <- value
    return(object)
})

#lastchangecpts
setGeneric("lastchangecpts<-", function(object, value) standardGeneric("lastchangecpts<-"))
setReplaceMethod("lastchangecpts", "ocpt", function(object, value) {
    object@lastchangecpts <- value
    return(object)
})
setReplaceMethod("lastchangecpts", "ocpt.reg", function(object, value) {
    object@lastchangecpts <- value
    return(object)
})

#numchangecpts
setGeneric("numchangecpts<-", function(object, value) standardGeneric("numchangecpts<-"))
setReplaceMethod("numchangecpts", "ocpt", function(object, value) {
    object@numchangecpts <- value
    return(object)
})
setReplaceMethod("numchangecpts", "ocpt.reg", function(object, value) {
    object@numchangecpts <- value
    return(object)
})

#checklist
setGeneric("checklist<-", function(object, value) standardGeneric("checklist<-"))
setReplaceMethod("checklist", "ocpt", function(object, value) {
    object@checklist <- value
    return(object)
})
setReplaceMethod("checklist", "ocpt.reg", function(object, value) {
    object@checklist <- value
    return(object)
})

#ndone
setGeneric("ndone<-", function(object, value) standardGeneric("ndone<-"))
setReplaceMethod("ndone", "ocpt", function(object, value) {
    object@ndone <- value
    return(object)
})
setReplaceMethod("ndone", "ocpt.reg", function(object, value) {
    object@ndone <- value
    return(object)
})

#nupdate
setGeneric("nupdate<-", function(object, value) standardGeneric("nupdate<-"))
setReplaceMethod("nupdate", "ocpt", function(object, value) {
    object@nupdate <- value
    return(object)
})
setReplaceMethod("nupdate", "ocpt.reg", function(object, value) {
    object@nupdate <- value
    return(object)
})

#cost_func
setGeneric("cost_func<-", function(object, value) standardGeneric("cost_func<-"))
setReplaceMethod("cost_func", "ocpt", function(object, value) {
    object@cost_func <- value
    return(object)
})
setReplaceMethod("cost_func", "ocpt.reg", function(object, value) {
    object@cost_func <- value
    return(object)
})

#shape
setGeneric("shape<-", function(object, value) standardGeneric("shape<-"))
setReplaceMethod("shape", "ocpt", function(object, value) {
    object@shape <- value
    return(object)
})
setReplaceMethod("shape", "ocpt.reg", function(object, value) {
    object@shape <- value
    return(object)
})

# parameter functions
setGeneric("param", function(object,...) standardGeneric("param"))
setMethod("param", "ocpt", function(object,shape,...) {
    param.mean=function(object){
        cpts=c(0,object@cpts)
        #nseg=length(cpts)-1
        data=data.set(object)
        tmpmean=NULL
        for(j in 1:nseg(object)){
            tmpmean[j]=mean(data[(cpts[j]+1):(cpts[j+1])])
        }
        return(tmpmean)
    }
    param.var=function(object){
        cpts=c(0,object@cpts)
        #nseg=length(cpts)-1
        data=data.set(object)
        seglen=seg.len(object)
        tmpvar=NULL
        for(j in 1:nseg(object)){
            tmpvar[j]=var(data[(cpts[j]+1):(cpts[j+1])])
        }
        tmpvar=tmpvar*(seglen-1)/seglen # correctly for the fact that the MLE estimate is /n but the var function is /n-1
        return(tmpvar)
    }
    param.scale=function(object,shape){
        cpts=c(0,object@cpts)
        #nseg=length(cpts)-1
        data=data.set(object)
        y=c(0,cumsum(data))
        tmpscale=NULL
        for(j in 1:nseg(object)){
            tmpscale[j]=(y[(cpts[j+1]+1)]-y[(cpts[j]+1)])/((cpts[j+1]-cpts[j])*shape)
        }
        return(tmpscale)
    }
    param.trend=function(object){
        cpts=c(0,object@cpts)
        seglen=seg.len(object)
        data=data.set(object)
        n=length(data)
        sumstat=cbind(cumsum(c(0,data)),cumsum(c(0,data*c(1:n))))
        cptsumstat=matrix(sumstat[object@cpts+1,]-sumstat[c(0,cpts(object))+1,],ncol=2)
        cptsumstat[,2]=cptsumstat[,2]-cptsumstat[,1]*c(0,cpts(object)) # i.e. creating newx3
        
        thetaS=(2*cptsumstat[,1]*(2*seglen + 1) - 6*cptsumstat[,2]) / (2*seglen*(2*seglen + 1) - 3*seglen*(seglen+1))
        thetaT=(6*cptsumstat[,2])/((seglen+1)*(2*seglen+1)) + (thetaS * (1-((3*seglen)/((2*seglen)+1))))
        return(cbind(thetaS,thetaT))
    }
    param.meanar=function(object){
        seglen=seg.len(object)
        data=data.set(object)
        n=length(data)-1
        sumstat=cbind(cumsum(c(0,data[-1])),cumsum(c(0,data[-(n+1)])),cumsum(c(0,data[-1]*data[-(n+1)])),cumsum(c(0,data[-1]^2)),cumsum(c(0,data[-(n+1)]^2)))
        cptsumstat=matrix(sumstat[object@cpts+1,]-sumstat[c(0,cpts(object))+1,],ncol=5)
        beta2=(2*seglen*cptsumstat[,3]-cptsumstat[,1]*cptsumstat[,2])/(2*seglen*cptsumstat[,5]*(1-cptsumstat[,2]^2));
        beta1=(2*cptsumstat[,1]-beta2*cptsumstat[,2])/(2*seglen);
        
        return(cbind(beta1,beta2))
    }
    param.trendar=function(object){
        seglen=seg.len(object)
        data=data.set(object)
        n=length(data)-1
        sumstat=cbind(cumsum(c(0,data[-1])),cumsum(c(0,data[-(n+1)])),cumsum(c(0,data[-1]*data[-(n+1)])),cumsum(c(0,data[-1]*c(1:n))),cumsum(c(0,data[-(n+1)]*c(0:(n-1)))),cumsum(c(0,data[-1]^2)),cumsum(c(0,data[-(n+1)]^2)))
        cptsumstat=matrix(sumstat[object@cpts+1,]-sumstat[c(0,cpts(object))+1,],ncol=7)
        cptsumstat[,4]=cptsumstat[,4]-cptsumstat[,1]*c(0,cpts(object)) # i.e. creating newx4
        cptsumstat[,5]=cptsumstat[,5]-cptsumstat[,2]*c(0,cpts(object)) # i.e. creating newx5
        betatop=seglen*(seglen-1)*(seglen*(seglen-1)*cptsumstat[,3] + 2*(2*seglen+1)*cptsumstat[,1]*(cptsumstat[,5]-seglen*cptsumstat[,2]) + 6*cptsumstat[,4]*(cptsumstat[,2]-cptsumstat[,5]))
        betabottom=seglen*(seglen-1)*cptsumstat[,7] + 2*(2*seglen+1)*cptsumstat[,2]*(seglen*cptsumstat[,2]-cptsumstat[,5]) + 6*cptsumstat[,5]*(cptsumstat[,5]-cptsumstat[,2]);
        beta=betatop/betabottom;
        thetajpo=(6*(seglen+2)*(cptsumstat[,4]-beta*cptsumstat[,5]))/((seglen+1)*(2*seglen+1)) - 2*(cptsumstat[,1]-beta*cptsumstat[,2])
        thetaj=(2*(2*seglen+1)*(cptsumstat[,1]-beta*cptsumstat[,2])-6*(cptsumstat[,4]-beta*cptsumstat[,5]))/(seglen-1)
        
        return(cbind(beta,thetajpo,thetaj))
    }
    if(cpttype(object)=="mean"){
        param.est(object)<-list(mean=param.mean(object))
    }
    else if(cpttype(object)=="variance"){
        param.est(object)<-list(variance=param.var(object))
    }
    else if(cpttype(object)=="mean and variance"){
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
    else if(cpttype(object)=="trend"){
        if(test.stat(object)=="Normal"){
            tmp=param.trend(object)
            param.est(object)<-list(thetaS=tmp[,1],thetaT=tmp[,2])
        }
        else{
            stop("Unknown test statistic for a change in trend")
        }
    }
    else if(cpttype(object)=="trendar"){
        if(test.stat(object)=="Normal"){
            tmp=param.trendar(object)
            param.est(object)<-list(beta=tmp[,1],thetajpo=tmp[,2],thetaj=tmp[,3])
        }
        else{
            stop("Unknown test statistic for a change in trend+ar")
        }
    }
    else if(cpttype(object)=="meanar"){
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

setMethod("param", "ocpt.range", function(object,ncpts=NA,shape,...) {
    if(is.na(ncpts)){
        cpts=c(0,object@cpts)
    }
    else{
        ncpts.full=apply(cpts.full(object),1,function(x){sum(x>0,na.rm=TRUE)})
        row=try(which(ncpts.full==ncpts),silent=TRUE)
        if(class(row)=='try-error'){
            stop("Your input object doesn't have a segmentation with the requested number of changepoints.")
        }
        cpts=c(0,cpts.full(object)[row,1:ncpts],as.integer(ndone(object)+nupdate(object)))
    }
    
    param.mean=function(object,cpts){
        nseg=length(cpts)-1
        data=data.set(object)
        tmpmean=NULL
        for(j in 1:nseg){
            tmpmean[j]=mean(data[(cpts[j]+1):(cpts[j+1])])
        }
        return(tmpmean)
    }
    param.var=function(object,cpts){
        nseg=length(cpts)-1
        data=data.set(object)
        seglen=seg.len(object)
        tmpvar=NULL
        for(j in 1:nseg){
            tmpvar[j]=var(data[(cpts[j]+1):(cpts[j+1])])
        }
        tmpvar=tmpvar*(seglen-1)/seglen
        return(tmpvar)
    }
    param.scale=function(object,cpts,shape){
        nseg=length(cpts)-1
        data=data.set(object)
        y=c(0,cumsum(data))
        tmpscale=NULL
        for(j in 1:nseg){
            tmpscale[j]=(y[(cpts[j+1]+1)]-y[(cpts[j]+1)])/((cpts[j+1]-cpts[j])*shape)
        }
        return(tmpscale)
    }
    param.trend=function(object){
        cpts=c(0,object@cpts)
        seglen=seg.len(object)
        data=data.set(object)
        n=length(data)
        sumstat=cbind(cumsum(c(0,data)),cumsum(c(0,data*c(1:n))))
        cptsumstat=matrix(sumstat[object@cpts+1,]-sumstat[c(0,cpts(object))+1,],ncol=2)
        cptsumstat[,2]=cptsumstat[,2]-cptsumstat[,1]*c(0,cpts(object)) # i.e. creating newx3
        
        thetaS=(2*cptsumstat[,1]*(2*seglen + 1) - 6*cptsumstat[,2]) / (2*seglen*(2*seglen + 1) - 3*seglen*(seglen+1))
        thetaT=(6*cptsumstat[,2])/((seglen+1)*(2*seglen+1)) + (thetaS * (1-((3*seglen)/((2*seglen)+1))))
        return(cbind(thetaS,thetaT))
    }
    param.meanar=function(object){
        seglen=seg.len(object)
        data=data.set(object)
        n=length(data)-1
        sumstat=cbind(cumsum(c(0,data[-1])),cumsum(c(0,data[-(n+1)])),cumsum(c(0,data[-1]*data[-(n+1)])),cumsum(c(0,data[-1]^2)),cumsum(c(0,data[-(n+1)]^2)))
        cptsumstat=matrix(sumstat[object@cpts+1,]-sumstat[c(0,cpts(object))+1,],ncol=5)
        beta2=(2*seglen*cptsumstat[,3]-cptsumstat[,1]*cptsumstat[,2])/(2*seglen*cptsumstat[,5]*(1-cptsumstat[,2]^2));
        beta1=(2*cptsumstat[,1]-beta2*cptsumstat[,2])/(2*seglen);
        
        return(cbind(beta1,beta2))
    }
    param.trendar=function(object){
        seglen=seg.len(object)
        data=data.set(object)
        n=length(data)-1
        sumstat=cbind(cumsum(c(0,data[-1])),cumsum(c(0,data[-(n+1)])),cumsum(c(0,data[-1]*data[-(n+1)])),cumsum(c(0,data[-1]*c(1:n))),cumsum(c(0,data[-(n+1)]*c(0:(n-1)))))
        cptsumstat=matrix(sumstat[object@cpts+1,]-sumstat[c(0,cpts(object))+1,],ncol=7)
        cptsumstat[,4]=cptsumstat[,4]-cptsumstat[,1]*c(0,cpts(object)) # i.e. creating newx4
        cptsumstat[,5]=cptsumstat[,5]-cptsumstat[,2]*c(0,cpts(object)) # i.e. creating newx5
        betatop=seglen*(seglen-1)*(seglen*(seglen-1)*cptsumstat[,3] + 2*(2*seglen+1)*cptsumstat[,1]*(cptsumstat[,5]-seglen*cptsumstat[,2]) + 6*cptsumstat[,4]*(cptsumstat[,2]-cptsumstat[,5]))
        betabottom=seglen*(seglen-1)*cptsumstat[,7] + 2*(2*seglen+1)*cptsumstat[,2]*(seglen*cptsumstat[,2]-cptsumstat[,5]) + 6*cptsumstat[,5]*(cptsumstat[,5]-cptsumstat[,2]);
        beta=betatop/betabottom;
        thetajpo=(6*(seglen+2)*(cptsumstat[,4]-beta*cptsumstat[,5]))/((seglen+1)*(2*seglen+1)) - 2*(cptsumstat[,1]-beta*cptsumstat[,2])
        thetaj=(2*(2*seglen+1)*(cptsumstat[,1]-beta*cptsumstat[,2])-6*(cptsumstat[,4]-beta*cptsumstat[,5]))/(seglen-1)
        
        return(cbind(beta,thetajpo,thetaj))
    }
    
    if(cpttype(object)=="mean"){
        param.est<-list(mean=param.mean(object,cpts))
    }
    else if(cpttype(object)=="variance"){
        param.est<-list(variance=param.var(object,cpts))
    }
    else if(cpttype(object)=="mean and variance"){
        if(test.stat(object)=="Normal"){
            param.est<-list(mean=param.mean(object,cpts),variance=param.var(object,cpts))
        }
        else if(test.stat(object)=="Gamma"){
            param.est<-list(scale=param.scale(object,cpts,shape=shape),shape=shape)
        }
        else if(test.stat(object)=="Exponential"){
            param.est<-list(rate=1/param.mean(object,cpts))
        }
        else if(test.stat(object)=="Poisson"){
            param.est<-list(lambda=param.mean(object,cpts))
        }
        else{
            stop("Unknown test statistic for a change in mean and variance")
        }
    }
    else if(cpttype(object)=="trend"){
        if(test.stat(object)=="Normal"){
            tmp=param.trend(object)
            param.est(object)<-list(thetaS=tmp[,1],thetaT=tmp[,2])
        }
        else{
            stop("Unknown test statistic for a change in trend")
        }
    }
    else if(cpttype(object)=="trendar"){
        if(test.stat(object)=="Normal"){
            tmp=param.trendar(object)
            param.est(object)<-list(beta=tmp[,1],thetajpo=tmp[,2],thetaj=tmp[,3])
        }
        else{
            stop("Unknown test statistic for a change in trend+ar")
        }
    }
    else if(cpttype(object)=="meanar"){
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
    if(is.na(ncpts)){
        param.est(object)=param.est
        return(object)
    }
    out=new('ocpt.range')
    param.est(out)=param.est
    return(out)
})

setMethod("param", "ocpt.reg", function(object,shape,...) {
    param.norm=function(object){
        cpts=c(0,object@cpts)
        #    nseg=length(cpts)-1 #nseg(object)
        data=data.set(object)
        p=ncol(data)-1
        tmpbeta=matrix(NA,ncol=p,nrow=nseg(object))
        tmpsigma=rep(NA,nseg(object))
        for(j in 1:nseg(object)){
            formula=paste('-1+data[',cpts[j]+1,':',cpts[j+1],',2]',sep='')
            if(p>1){
                for(i in 2:p){
                    formula=paste(formula,'+data[',(cpts[j]+1),':',cpts[j+1],',',i+1,']',sep='')
                }
            }
            tmpfit=eval(parse(text=paste('lm(data[',(cpts[j]+1),':',cpts[j+1],',1]~',formula,')',sep='')))
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
    cat("Created Using changepoint.online version",object@version,'\n')
    cat("Changepoint type      : Change in",cpttype(object),'\n')
    cat("Method of analysis    :",method(object),"\n")
    cat("Test Statistic  :", test.stat(object),"\n")
    cat("Type of penalty       :", pen.type(object), "with value,",pen.value(object),"\n")
    cat("Minimum Segment Length :", minseglen(object),"\n")
    cat("Maximum no. of cpts   :", ncpts.max(object),"\n")
    if(length(cpts(object))<=20){cat("Changepoint Locations :",cpts(object),"\n")}
    else{cat("Number of changepoints:", ncpts(object),"\n")}
    cat("ndone              :", ndone(object), "\n")
    cat("nupdate            :", nupdate(object), "\n")
})

setMethod("summary","ocpt.range",function(object){
    cat("Created Using changepoint.online version",object@version,'\n')
    cat("Changepoint type      : Change in",cpttype(object),'\n')
    cat("Method of analysis    :",method(object),"\n")
    cat("Test Statistic  :", test.stat(object),"\n")
    cat("Type of penalty       :", pen.type(object), "with value,",pen.value(object),"\n")
    cat("Minimum Segment Length :", minseglen(object),"\n")
    cat("Maximum no. of cpts   :", ncpts.max(object),"\n")
    if(length(cpts(object))<=20){cat("Changepoint Locations :",cpts(object),"\n")}
    else{cat("Number of changepoints:", ncpts(object),"\n")}
    if((nrow(cpts.full(object))<=5)&(ncol(cpts.full(object)<=20))){cat("Range of segmentations:\n");print(cpts.full(object));cat("\n For penalty values:", pen.value.full(object),"\n")}
    else{cat("Number of segmentations recorded:", nrow(cpts.full(object)), " with between ", sum(cpts.full(object)[nrow(cpts.full(object)),]>0,na.rm=T), " and ", sum(cpts.full(object)[1,]>0,na.rm=T), "changepoints.\n Penalty value ranges from:",min(pen.value.full(object))," to ",max(pen.value.full(object)))}
    cat("ndone              :", ndone(object), "\n")
    cat("nupdate            :", nupdate(object), "\n")
})

setMethod("summary","ocpt.reg",function(object){
    cat("Created Using changepoint.online version",object@version,'\n')
    cat("Changepoint type     : Change in",cpttype(object),'\n')
    cat("Method of analysis   :",method(object),"\n")
    cat("Test Statistic :", test.stat(object),"\n")
    cat("Type of penalty      :", pen.type(object), "with value,",pen.value(object),"\n")
    cat("Maximum no. of cpts   :", ncpts.max(object),"\n")
    if(length(cpts(object))<=20){cat("Changepoint Locations :",cpts(object),"\n")}
    else{cat("Number of changepoints:", ncpts(object),"\n")}
    cat("ndone              :", ndone(object), "\n")
    cat("nupdate            :", nupdate(object), "\n")
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
setMethod("plot","ocpt",function(x,cpt.col='red',cpt.width=1,cpt.style=1,window=NA,...){
    if(length(param.est(x))==0){# i.e. parameter.estimates=FALSE in call
        cat('Calculating parameter estimates...')
        object=param(x)
        cat('done.\n')
    }
    if(is.na(window)){
        plot(data.set.ts(x),...)
    }
    else{
        if(window>(ndone(x)+nupdate(x))){
            window=ndone(x)+nupdate(x)
        }
        start=ndone(x)+nupdate(x)-window
        plot(data.set.ts(x)[start:(ndone(x)+nupdate(x))],...)
    }
    if(cpttype(x)=="variance"){
        abline(v=index(data.set.ts(x))[cpts(x)],col=cpt.col,lwd=cpt.width,lty=cpt.style)
    }
    else if(cpttype(x)=="mean"  ||  cpttype(x)=="mean and variance"){
        #nseg=length(cpts(x))+1
        cpts=c(0,x@cpts)
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
            segments(index(data.set.ts(x))[cpts[i]+1],means[i],index(data.set.ts(x))[cpts[i+1]],means[i],col=cpt.col,lwd=cpt.width,lty=cpt.style)
        }
    }
    else if(cpttype(x)=="trend"){
        cpts=c(0,x@cpts)
        intercept=rep(param.est(x)$thetaS,x@cpts-c(0,cpts(x)))
        slope=rep(param.est(x)$thetaT-param.est(x)$thetaS,x@cpts-c(0,cpts(x)))/rep(x@cpts-c(0,cpts(x)),x@cpts-c(0,cpts(x)))
        cptn=rep(c(0,cpts(x)),x@cpts-c(0,cpts(x)))
        n=length(data.set(x))
        means=intercept+slope*((1:n)-cptn)
        for(i in 1:nseg(x)){
            segments(index(data.set.ts(x))[cpts[i]+1],means[cpts[i]+1],index(data.set.ts(x))[cpts[i+1]],means[cpts[i+1]],col=cpt.col,lwd=cpt.width,lty=cpt.style)
        }
    }
    else{
        stop('Invalid Changepoint Type for plotting.\n Can only plot mean, variance, mean and variance')
    }
})

setMethod("plot","ocpt.range",function(x,ncpts=NA,diagnostic=FALSE,cpt.col='red',cpt.width=1,cpt.style=1,...){
    if(diagnostic==TRUE){
        return(plot(apply(cpts.full(x),1,function(x){sum(x>0,na.rm=TRUE)}),pen.value.full(x),type='l',xlab='Number of Changepoints',ylab='Difference in Test Statistic',...))
    }
    plot(data.set.ts(x),...)
    if(is.na(ncpts)){
        if(pen.type(x)=="CROPS"){
            stop('CROPS does not supply an optimal set of changepoints, set ncpts to the desired segmentation to plot or use diagnostic=TRUE to identify an appropriate number of changepoints')
        }
        cpts.to.plot=cpts(x)
        param.est=x
    }
    else{
        ncpts.full=apply(cpts.full(x),1,function(x){sum(x>0,na.rm=TRUE)})
        row=which(ncpts.full==ncpts)
        if(length(row)==0){
            stop(paste("Your input object doesn't have a segmentation with the requested number of changepoints.\n Possible ncpts are: "),paste(ncpts.full,collapse=','))
        }
        cpts.to.plot=cpts.full(x)[row,1:ncpts]
        if(test.stat(x)=="Gamma"){
            param.est=param(x,ncpts,shape=param.est(x)$shape)
        }
        else{
            param.est=param(x,ncpts)
        }
    }
    if(cpttype(x)=="variance"){
        abline(v=index(data.set.ts(x))[cpts.to.plot],col=cpt.col,lwd=cpt.width,lty=cpt.style)
    }
    else if(cpttype(x)=="mean"  ||  cpttype(x)=="mean and variance"){
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
        nseg=ncpts+1
        cpts.to.plot=c(0,cpts.to.plot,length(data.set(x)))
        for(i in 1:nseg){
            segments(index(data.set.ts(x))[cpts.to.plot[i]+1],means[i],index(data.set.ts(x))[cpts.to.plot[i+1]],means[i],col=cpt.col,lwd=cpt.width,lty=cpt.style)
        }
    }
    else{
        stop('Invalid Changepoint Type for plotting.\n Can only plot mean, variance, mean and variance')
    }
})

setMethod("plot","ocpt.reg",function(x,cpt.col='red',cpt.width=1,cpt.style=1,...){
    if(length(param.est(x))==0){# i.e. parameter.estimates=FALSE in call
        cat('Calculating parameter estimates...')
        object=param(x)
        cat('done.\n')
    }
    plot(data.set(x)[,1],type='l',...)
    if(test.stat(x)=="Normal"){
        cpts=c(0,x@cpts)
        betas=param.est(x)$beta
        for(i in 1:nseg(x)){
            lines((cpts[i]+1):cpts[i+1],betas[i,]%*%t(data.set(x)[(cpts[i]+1):cpts[i+1],-1]),col=cpt.col,lwd=cpt.width,lty=cpt.style)
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
        if(cpttype(object)=="mean"){
            means=rep(param.est(object)$mean,object@cpts-c(0,cpts(object)))
            rss=sum((data.set(object)-means)^2)
            n=length(data.set(object))
            like=n*(log(2*pi)+log(rss/n)+1) # -2*loglik
            if(pen.type(object)=="MBIC"){
                like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
            }else{
                like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
            }
        }
        else if(cpttype(object)=="variance"){
            rss=c(0,cumsum((data.set(object)-param.est(object)$mean)^2))
            cpts=c(0,object@cpts)
            n=length(data.set(object))
            seglen=seg.len(object)
            sigmas=(rss[cpts[-1]+1]-rss[cpts[-length(cpts)]+1])/seglen
            like=n*log(2*pi)+sum(seglen*log(sigmas))+n
            if(pen.type(object)=="MBIC"){
                like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(seglen)))
            }else{
                like=c(like,like+(nseg(object)-1)*pen.value(object))
            }
        }
        else if(cpttype(object)=="mean and variance"){
            means=rep(param.est(object)$mean,object@cpts-c(0,cpts(object)))
            rss=sum((data.set(object)-means)^2)
            n=length(data.set(object))
            cpts=c(0,object@cpts)
            seglen=seg.len(object)
            sigmas=param.est(object)$variance
            like=n*log(2*pi)+sum(seglen*log(sigmas))+n
            if(pen.type(object)=="MBIC"){
                like=c(like,like+(nseg(object)-2)*pen.value(object)+sum(log(seglen)))
            }else{
                like=c(like,like+(nseg(object)-1)*pen.value(object))
            }
        }
        else if(cpttype(object)=="trend"){
            intercept=rep(param.est(object)$thetaS,object@cpts-c(0,cpts(object)))
            slope=rep(param.est(object)$thetaT-param.est(object)$thetaS,object@cpts-c(0,cpts(object)))/rep(object@cpts-c(0,cpts(object)),object@cpts-c(0,cpts(object)))
            cptn=rep(c(0,cpts(object)),object@cpts-c(0,cpts(object)))
            n=length(data.set(object))
            means=intercept+slope*((1:n)-cptn)
            rss=sum((data.set(object)-means)^2)
            like=n*(log(2*pi)+log(rss/n)+1) # -2*loglik
            if(pen.type(object)=="MBIC"){
                like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
            }else{
                like=c(like,like+(nseg(object)-1)*pen.value(object))
            }
        }
        else if(cpttype(object)=="trendar"){
            seglen=seg.len(object)
            intercept=rep(param.est(object)$thetaj,seglen)
            slope=rep(param.est(object)$thetajpo-param.est(object)$thetaj,seglen)/rep(seglen,seglen)
            ar=rep(param.est(object)$beta,seglen)
            cptn=rep(c(0,cpts(object)),seglen)
            n=length(data.set(object))
            means=NULL;means[1]=0
            for(i in 2:n){means[i]=intercept+slope*((1:n)-cptn)+ar*means[i-1]}
            means=means[-1]
            rss=sum((data.set(object)[-1]-means)^2)
            like=n*(log(2*pi)+log(rss/n)+1) # -2*loglik
            if(pen.type(object)=="MBIC"){
                like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
            }else{
                like=c(like,like+(nseg(object)-1)*pen.value(object))
            }
        }
        else if(cpttype(object)=="meanar"){
            seglen=seg.len(object)
            intercept=rep(param.est(object)$beta1,seglen)
            ar=rep(param.est(object)$beta2,seglen)
            cptn=rep(c(0,cpts(object)),seglen)
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
        if(cpttype(object)!="mean and variance"){
            stop("Unknown changepoint type for test.stat='Gamma', must be 'mean and variance'")
        }
        else{
            warning("Not changed to be -2*logLik")
            mll.meanvarg=function(x,n,shape){
                return(n*shape*log(n*shape)-n*shape*log(x))
            }
            y=c(0,cumsum(data.set(object)))
            shape=param.est(object)$shape
            cpts=c(0,object@cpts)
            #nseg=length(cpts)-1
            tmplike=0
            for(j in 1:nseg(object)){
                tmplike=tmplike+mll.meanvarg(y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j],shape)
            }
            if(pen.type(object)=="MBIC"){
                like=c(tmplike, tmplike+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
            }else{
                like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
            }
        }
    }
    else if(test.stat(object)=="Exponential"){
        if(cpttype(object)!="mean and variance"){
            stop("Unknown changepoint type for test.stat='Exponential', must be 'mean and variance'")
        }
        else{
            warning("Not changed to be -2*logLik")
            mll.meanvare=function(x,n){
                return(n*log(n)-n*log(x))
            }
            y=c(0,cumsum(data.set(object)))
            cpts=c(0,object@cpts)
            #nseg=length(cpts)-1
            tmplike=0
            for(j in 1:nseg(object)){
                tmplike=tmplike+mll.meanvare(y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j])
            }
            if(pen.type(object)=="MBIC"){
                like=c(tmplike, tmplike+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
            }else{
                like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
            }
        }
    }
    else if(test.stat(object)=="Poisson"){
        if(cpttype(object)!="mean and variance"){
            stop("Unknown changepoint type for test.stat='Poisson', must be 'mean and variance'")
        }
        else{
            warning("Not changed to be -2*logLik")
            mll.meanvarp=function(x,n){
                return(x*log(x)-x*log(n))
            }
            y=c(0,cumsum(data.set(object)))
            cpts=c(0,object@cpts)
            #nseg=length(cpts)-1
            tmplike=0
            for(j in 1:nseg(object)){
                tmplike=tmplike+mll.meanvarp(y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j])
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

setMethod("logLik", "ocpt.range", function(object,ncpts=NA) {
    warning("Not changed to be -2*logLik")
    if(is.na(ncpts)){
        if(pen.type(object)=="CROPS"){
            stop('CROPS does not supply an optimal set of changepoints, set ncpts argument to the desired segmentation to plot or use diagnostic=TRUE to identify an appropriate number of changepoints')
        }
        cpts=c(0,object@cpts)
        pen.value=pen.value(object)
    }
    else{
        ncpts.full=apply(cpts.full(object),1,function(x){sum(x>0,na.rm=TRUE)})
        row=which(ncpts.full==ncpts)
        if(length(row)==0){
            stop(paste("Your input object doesn't have a segmentation with the requested number of changepoints.\n Possible ncpts are: "),paste(ncpts.full,collapse=','))
        }
        cpts=c(0,cpts.full(object)[row,1:ncpts],as.integer(ndone(object)+nupdate(object)))
        pen.value=pen.value.full(object)[row]
    }
    nseg=length(cpts)-1
    
    if(test.stat(object)=="Normal"){
        if(cpttype(object)=="mean"){
            mll.mean=function(x2,x,n){
                return( x2-(x^2)/n)
            }
            y2=c(0,cumsum(data.set(object)^2))
            y=c(0,cumsum(data.set(object)))
            tmplike=0
            for(j in 1:nseg){
                tmplike=tmplike+mll.mean(y2[cpts[j+1]+1]-y2[cpts[j]+1],y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j])
            }
            ##c(tmplike, tmplike+(nseg-2)*pen.value(object)+sum(log(cpts[-1]-cpts[-(nseg+1)])))
            if(pen.type(object)=="MBIC"){
                like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(seg.len(object))))
            }else{
                like=c(tmplike,tmplike+(nseg-1)*pen.value)
            }
            names(like)=c("-like","-likepen")
        }
        else if(cpttype(object)=="variance"){
            mll.var=function(x,n){
                neg=x<=0
                x[neg==TRUE]=0.00000000001
                return( n*(log(2*pi)+log(x/n)+1))
            }
            y2=c(0,cumsum((data.set(object)-param.est(object)$mean)^2))
            tmplike=0
            for(j in 1:nseg){
                tmplike=tmplike+mll.var(y2[cpts[j+1]+1]-y2[cpts[j]+1],cpts[j+1]-cpts[j])
            }
            if(pen.type(object)=="MBIC"){
                like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(seg.len(object))))
            }else{
                like=c(tmplike,tmplike+(nseg-1)*pen.value)
            }
            names(like)=c("-like","-likepen")
        }
        else if(cpttype(object)=="mean and variance"){
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
                tmplike=tmplike+mll.meanvar(y2[cpts[j+1]+1]-y2[cpts[j]+1],y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j])
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
        if(cpttype(object)!="mean and variance"){
            stop("Unknown changepoint type for test.stat='Gamma', must be 'mean and variance'")
        }
        else{
            mll.meanvarg=function(x,n,shape){
                return(n*shape*log(n*shape)-n*shape*log(x))
            }
            y=c(0,cumsum(data.set(object)))
            shape=param.est(object)$shape
            cpts=c(0,object@cpts)
            #nseg=length(cpts)-1
            tmplike=0
            for(j in 1:nseg){
                tmplike=tmplike+mll.meanvarg(y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j],shape)
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
        if(cpttype(object)!="mean and variance"){
            stop("Unknown changepoint type for test.stat='Exponential', must be 'mean and variance'")
        }
        else{
            mll.meanvare=function(x,n){
                return(n*log(n)-n*log(x))
            }
            y=c(0,cumsum(data.set(object)))
            cpts=c(0,object@cpts)
            #nseg=length(cpts)-1
            tmplike=0
            for(j in 1:nseg){
                tmplike=tmplike+mll.meanvare(y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j])
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
        if(cpttype(object)!="mean and variance"){
            stop("Unknown changepoint type for test.stat='Poisson', must be 'mean and variance'")
        }
        else{
            mll.meanvarp=function(x,n){
                return(x*log(x)-x*log(n))
            }
            y=c(0,cumsum(data.set(object)))
            cpts=c(0,object@cpts)
            #nseg=length(cpts)-1
            tmplike=0
            for(j in 1:nseg){
                tmplike=tmplike+mll.meanvarp(y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j])
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
        cpts=c(0,object@cpts)
        seglen=seg.len(object)
        data=data.set(object)
        beta=param.est(object)$beta
        sigmas=param.est(object)$sig2
        rss=NULL
        for(i in 1:length(seglen)){
            rss[i]=sum((data[(cpts[i]+1):cpts[i+1],1]-data[(cpts[i]+1):cpts[i+1],-1]%*%beta[i,])^2)
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
    cpts=c(0,object@cpts)
    nseg=nseg(object)
    data=data.set(object)
    for(i in 1:nseg){
        stats::acf(data[(cpts[i]+1):cpts[i+1]],main=paste("Series part:",(cpts[i]+1),":",cpts[i+1]),...)
    }
})

setMethod("acf", "ocpt.reg", function(object,lag.max=NULL,...) {
    cpts=c(0,object@cpts)
    nseg=nseg(object)
    data=data.set(object)[,1]
    for(i in 1:nseg){
        stats::acf(data[(cpts[i]+1):cpts[i+1]],main=paste("Series part:",(cpts[i]+1),"-",cpts[i+1]),...)
    }
    })

setClass("ecp.ocpt",slots=list(number="numeric",estimates="numeric",GofM="numeric",delta="numeric",alpha="numeric",verbose="logical",csum="numeric",dll="numeric",dlr="numeric",drr="numeric",left="matrix",right="matrix",datalength="numeric",functime="numeric",width="numeric",cpLoc="list"),contains="ocpt")

####functions
if(!isGeneric("number")) {
    if (is.function("number")){
        fun <- number
    }
    else {fun <- function(object){
        standardGeneric("number")
    }
    }
    setGeneric("number", fun)
}
setMethod("number","ecp.ocpt",function(object) object@number)

if(!isGeneric("estimates")) {
    if (is.function("estimates")){
        fun <- estimates
    }
    else {fun <- function(object){
        standardGeneric("estimates")
    }
    }
    setGeneric("estimates", fun)
}
setMethod("estimates","ecp.ocpt",function(object) object@estimates)

if(!isGeneric("GofM")) {
    if (is.function("GofM")){
        fun <- GofM
    }
    else {fun <- function(object){
        standardGeneric("GofM")
    }
    }
    setGeneric("GofM", fun)
}
setMethod("GofM","ecp.ocpt",function(object) object@GofM)

if(!isGeneric("delta")) {
    if (is.function("delta")){
        fun <- delta
    }
    else {fun <- function(object){
        standardGeneric("delta")
    }
    }
    setGeneric("delta", fun)
}
setMethod("delta","ecp.ocpt",function(object) object@delta)

if(!isGeneric("alpha")) {
    if (is.function("alpha")){
        fun <- alpha
    }
    else {fun <- function(object){
        standardGeneric("alpha")
    }
    }
    setGeneric("alpha", fun)
}
setMethod("alpha","ecp.ocpt",function(object) object@alpha)

if(!isGeneric("verbose")) {
    if (is.function("verbose")){
        fun <- verbose
    }
    else {fun <- function(object){
        standardGeneric("verbose")
    }
    }
    setGeneric("verbose", fun)
}
setMethod("verbose","ecp.ocpt",function(object) object@verbose)

if(!isGeneric("csum")) {
    if (is.function("csum")){
        fun <- csum
    }
    else {fun <- function(object){
        standardGeneric("csum")
    }
    }
    setGeneric("csum", fun)
}
setMethod("csum","ecp.ocpt",function(object) object@csum)

if(!isGeneric("dll")) {
    if (is.function("dll")){
        fun <- dll
    }
    else {fun <- function(object){
        standardGeneric("dll")
    }
    }
    setGeneric("dll", fun)
}
setMethod("dll","ecp.ocpt",function(object) object@dll)

if(!isGeneric("dlr")) {
    if (is.function("dlr")){
        fun <- dlr
    }
    else {fun <- function(object){
        standardGeneric("dlr")
    }
    }
    setGeneric("dlr", fun)
}
setMethod("dlr","ecp.ocpt",function(object) object@dlr)

if(!isGeneric("drr")) {
    if (is.function("drr")){
        fun <- drr
    }
    else {fun <- function(object){
        standardGeneric("drr")
    }
    }
    setGeneric("drr", fun)
}
setMethod("drr","ecp.ocpt",function(object) object@drr)

if(!isGeneric("left")) {
    if (is.function("left")){
        fun <- left
    }
    else {fun <- function(object){
        standardGeneric("left")
    }
    }
    setGeneric("left", fun)
}
setMethod("left","ecp.ocpt",function(object) object@left)

if(!isGeneric("right")) {
    if (is.function("right")){
        fun <- right
    }
    else {fun <- function(object){
        standardGeneric("right")
    }
    }
    setGeneric("right", fun)
}
setMethod("right","ecp.ocpt",function(object) object@right)

if(!isGeneric("datalength")) {
    if (is.function("datalength")){
        fun <- datalength
    }
    else {fun <- function(object){
        standardGeneric("datalength")
    }
    }
    setGeneric("datalength", fun)
}
setMethod("datalength","ecp.ocpt",function(object) object@datalength)

if(!isGeneric("functime")) {
    if (is.function("functime")){
        fun <- functime
    }
    else {fun <- function(object){
        standardGeneric("functime")
    }
    }
    setGeneric("functime", fun)
}
setMethod("functime","ecp.ocpt",function(object) object@functime)

if(!isGeneric("width")) {
    if (is.function("width")){
        fun <- width
    }
    else {fun <- function(object){
        standardGeneric("width")
    }
    }
    setGeneric("width", fun)
}
setMethod("width","ecp.ocpt",function(object) object@width)

if(!isGeneric("cpLoc")) {
    if (is.function("cpLoc")){
        fun <- cpLoc
    }
    else {fun <- function(object){
        standardGeneric("cpLoc")
    }
    }
    setGeneric("cpLoc", fun)
}
setMethod("cpLoc","ecp.ocpt",function(object) object@cpLoc)

####replacement
setGeneric("number<-", function(object, value) standardGeneric("number<-"))
setReplaceMethod("number", "ecp.ocpt", function(object, value) {
    object@number <- value
    return(object)
})
setGeneric("estimates<-", function(object, value) standardGeneric("estimates<-"))
setReplaceMethod("estimates", "ecp.ocpt", function(object, value) {
    object@estimates <- value
    return(object)
})
setGeneric("GofM<-", function(object, value) standardGeneric("GofM<-"))
setReplaceMethod("GofM", "ecp.ocpt", function(object, value) {
    object@GofM <- value
    return(object)
})
setGeneric("delta<-", function(object, value) standardGeneric("delta<-"))
setReplaceMethod("delta", "ecp.ocpt", function(object, value) {
    object@delta <- value
    return(object)
})
setGeneric("alpha<-", function(object, value) standardGeneric("alpha<-"))
setReplaceMethod("alpha", "ecp.ocpt", function(object, value) {
    object@alpha <- value
    return(object)
})
setGeneric("verbose<-", function(object, value) standardGeneric("verbose<-"))
setReplaceMethod("verbose", "ecp.ocpt", function(object, value) {
    object@verbose <- value
    return(object)
})
setGeneric("csum<-", function(object, value) standardGeneric("csum<-"))
setReplaceMethod("csum", "ecp.ocpt", function(object, value) {
    object@csum <- value
    return(object)
})
setGeneric("dll<-", function(object, value) standardGeneric("dll<-"))
setReplaceMethod("dll", "ecp.ocpt", function(object, value) {
    object@dll <- value
    return(object)
})
setGeneric("dlr<-", function(object, value) standardGeneric("dlr<-"))
setReplaceMethod("dlr", "ecp.ocpt", function(object, value) {
    object@dlr <- value
    return(object)
})
setGeneric("drr<-", function(object, value) standardGeneric("drr<-"))
setReplaceMethod("drr", "ecp.ocpt", function(object, value) {
    object@drr <- value
    return(object)
})
setGeneric("left<-", function(object, value) standardGeneric("left<-"))
setReplaceMethod("left", "ecp.ocpt", function(object, value) {
    object@left <- value
    return(object)
})
setGeneric("right<-", function(object, value) standardGeneric("right<-"))
setReplaceMethod("right", "ecp.ocpt", function(object, value) {
    object@right <- value
    return(object)
})
setGeneric("datalength<-", function(object, value) standardGeneric("datalength<-"))
setReplaceMethod("datalength", "ecp.ocpt", function(object, value) {
    object@datalength <- value
    return(object)
})
setGeneric("functime<-", function(object, value) standardGeneric("functime<-"))
setReplaceMethod("functime", "ecp.ocpt", function(object, value) {
    object@functime <- value
    return(object)
})
setGeneric("width<-", function(object, value) standardGeneric("width<-"))
setReplaceMethod("width", "ecp.ocpt", function(object, value) {
    object@width <- value
    return(object)
})
setGeneric("cpLoc<-", function(object, value) standardGeneric("cpLoc<-"))
setReplaceMethod("cpLoc", "ecp.ocpt", function(object, value) {
    object@cpLoc <- value
    return(object)
})
#summary
# summary functions
setMethod("summary","ecp.ocpt",function(object){
    cat("Number of Changepoints   :", number(object),"\n")
    cat("Estimate Locations       :",estimates(object),"\n")
    cat("Goodness of Fit Model    :",GofM(object),"\n")
    cat("Delta                    :", delta(object),"\n")
    cat("Alpha                    :", alpha(object),"\n")
    cat("Verbose                  :", verbose(object),"\n")
    cat("Number of Data points    :", datalength(object),"\n")
    cat("Calculation Time         :", functime(object), "\n")
})

# show functions
setMethod("show","ecp.ocpt",function(object){
    cat("Class 'ocpt' : Changepoint Object\n")
    cat("summary(.)  :\n----------\n")
    summary(object)
})


