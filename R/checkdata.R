checkData = function(data){
    if(!is.numeric(data)){
        stop("Only numeric data allowed")
    }
    if(anyNA(data)){stop("Missing value: NA is not allowed in the data as changepoint methods are only sensible for regularly spaced data.")}
}
