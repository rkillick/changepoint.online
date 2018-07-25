	#########################################################################
	#			    Fast ECP (eFast)    			#
	#########################################################################

e.cp3o_delta.online.initialise = function(Z, K=1, delta=29, alpha=1, verbose=FALSE){
	#Argument checking
	if(!is.matrix(Z))
		stop("Z must be an n x d matrix.")
	if(alpha <= 0 || alpha > 2)
		stop("alpha must be in the interval (0,2].")
	if(delta < 2)
		stop("delta must be a positive integer greater than 1.")
	if(K < 1 || K > floor(nrow(Z)/(delta+1)))
		stop("K is not in an acceptable range.")
	#Force K and delta to be integers
	delta = as.integer(delta)
    K = as.integer(K)
    newlength = as.integer(length(Z[,1]))
    cSum = as.vector(c(rep(0,newlength)))
    oldlength = as.integer(0)
    DLL = as.vector(c(rep(0,newlength)))
    DRR = as.vector(c(rep(0,newlength)))
    DLR = as.vector(c(rep(0,newlength)))
    Left = as.matrix(matrix(c(rep(0, 2*newlength)), ncol = 2))
    Right = as.matrix(matrix(c(rep(0, 2*newlength)), ncol = 2))

	#Call C++ code that implements the method and store result in res
	#Also keep track of time
	t1 = proc.time()
	res = eFastC_delta.online(Z, K, delta, alpha, verbose, oldlength, newlength, cSum, DLL, DRR, DLR, Left, Right)
	t2 = proc.time()
    res$length = newlength
	res$time = as.numeric((t2-t1)[3])
	#Correct for the fact that C++ is zero based
	return(res)
}

e.cp3o_delta.online.initialize = function(Z, K=1, delta=29, alpha=1, verbose=FALSE){
  e.cp3o_delta.online.initialise(Z, K, delta, alpha, verbose)
}

e.cp3o_delta.online.update = function(previousanswer, newdata, K=1){
    #Argument checking
    if(!is.matrix(newdata))
    stop("new data must be an n x d matrix.")
    if(previousanswer$alpha <= 0 || previousanswer$alpha > 2)
    stop("alpha must be in the interval (0,2].")
    if(previousanswer$delta < 2)
    stop("delta must be a positive integer greater than 1.")
    if(K < 1 || K > floor(nrow(newdata)/(previousanswer$delta+1))){
    stop("K is not in an acceptable range.")
    }
    #Force K and delta to be integers
    Z = newdata
    K = as.integer(K)
    delta = as.integer(previousanswer$delta)
    alpha = as.numeric(previousanswer$alpha)
    verbose = as.logical(previousanswer$verbose)
    oldlength = as.integer(previousanswer$length)
    newlength = length(newdata[,1])
    cSum = as.vector(c(previousanswer$csum,rep(0,newlength)))
    DLL = as.vector(c(previousanswer$dll,rep(0,newlength)))
    DRR = as.vector(c(previousanswer$drr,rep(0,newlength)))
    DLR = as.vector(c(previousanswer$dlr,rep(0,newlength)))
    matrixlr = matrix(c(rep(0, 2*newlength)), ncol = 2)
    Left = as.matrix(rbind(previousanswer$left, matrixlr))
    Right = as.matrix(rbind(previousanswer$right, matrixlr))
    #Call C++ code that implements the method and store result in res
    #Also keep track of time
    t1 = proc.time()
    res = eFastC_delta.online(Z, K, delta, alpha, verbose, oldlength, newlength, cSum, DLL, DRR, DLR, Left, Right)
    t2 = proc.time()
    res$length = oldlength + newlength
    res$time = as.numeric((t2-t1)[3])
    #Correct for the fact that C++ is zero based
    return(res)
}

