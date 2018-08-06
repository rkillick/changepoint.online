online.ecp.class_input <- function(number, estimates, GofM, delta, alpha, verbose, csum, dll, dlr, drr, left, right, datalength, time, width, cpLoc){
  
  ans = new("ecp.ocpt")
  
  number(ans)=number; estimates(ans)=estimates; GofM(ans)=GofM; delta(ans)=delta; alpha(ans)=alpha; verbose(ans)=verbose; csum(ans)=csum; dll(ans)=dll; dlr(ans)=dlr; drr(ans)=drr; left(ans)=left; right(ans)=right; datalength(ans)=datalength; time(ans)=time; width(ans)=width; cpLoc(ans)=cpLoc;
  return(ans)
}
