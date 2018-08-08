setClass("ecp.ocpt",slots=list(number="numeric",estimates="numeric",GofM="numeric",delta="numeric",alpha="numeric",verbose="logical",csum="numeric",dll="numeric",dlr="numeric",drr="numeric",left="matrix",right="matrix",datalength="numeric",functime="numeric",width="numeric",cpLoc="list"))

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
