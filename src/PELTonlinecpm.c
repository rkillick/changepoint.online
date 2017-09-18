#include <R.h> 
#include <Rmath.h>
#include <Rinternals.h> // RK addition
#include <R_ext/RS.h>	// RK addition
#include <R_ext/Lapack.h> // RK addition
#include <R_ext/BLAS.h> // RK addition
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#define SWAP(a,b)   { int t; t=a; a=b; b=t; }  // Macro for swapping

static double *tmplike;
static int *tmpt;

void FreePELT(error)
	int *error; /* Error code from PELT C function, non-zero => error */	
	{
	if(*error==0){
	  free((void *)tmplike);
	  free((void *)tmpt);
	}
}

void PELT_mean_norm_update(ndone,y2,y,nupdate,pen,lastchangecpts,lastchangelike,checklist,nchecklist,cptsout,error)
	int *ndone;			/* Length of the time series analyzed so far */
  double *y2;    /* Summary statistic for the time series (ndone+nupdate) */
	double *y;		/*Summary statistics for the time series (ndone+nupdate) */
	int *nupdate;			/* Length of the time series to be analyzed in this update (i.e. total length is ndone+nupdate) */
  double *pen;  /* Penalty used to decide if a changepoint is significant */
  int *lastchangecpts;    /* Vector of identified last changepoint locations up to n (to be added to), length 2*(ndone+nupdate) */
  double *lastchangelike;    /* Vector of likelihoods for the data up to n (to be added to), length (ndone+nupdate) */
  int *checklist;    /* Vector of locations of the potential last changepoint for next iteration (to be updated), max length=(ndone+nupdate) */
  int *nchecklist;    /* Number in the checklist currently (to be updated) */
  int *cptsout;   /* Vector of locations of the identified changepoints up to (ndone+nupdate) */
  int *error;   /* 0 by default, nonzero indicates error in code */
  {
	// R code does know.mean and fills mu if necessary
	// must have atleast 1 observations for initialisation, i.e. if ndone=0 then nupdate>=1

	double minout;

  //double tmplike[*n];
  double *tmplike;
  tmplike = (double *)calloc((*nupdate+*nchecklist+1),sizeof(double));
  if (tmplike==NULL)   {
    *error = 4;
    goto err4;
  }

	//int tmpt[*n];
  int *tmpt;
  tmpt = (int *)calloc((*nupdate+*nchecklist+1),sizeof(int));
  if (tmpt==NULL)   {
    *error = 5;
    goto err5;
  }

	int tstar,i,whichout,nchecktmp;

	double mll_mean();
	void min_which();
	
	if(*ndone==0){
  	*(lastchangelike+0)= -*pen;
  	*(lastchangecpts+0)=0; *(lastchangecpts+*ndone+*nupdate+0)=0;
  	*(lastchangelike+1)=mll_mean(*(y+1),*(y2+1),1);
  	*(lastchangecpts+1)=0; *(lastchangecpts+*ndone+*nupdate+1)=1;

   	*nchecklist=2;
  	*(checklist+0)=0;
  	*(checklist+1)=1;
  	*ndone=1;
		*nupdate=*nupdate-1;
  }

	for(tstar=*ndone+1;tstar<(*ndone+*nupdate+1);tstar++){
    R_CheckUserInterrupt(); /* checks if user has interrupted the R session and quits if true */

		for(i=0;i< *nchecklist;i++){
			tmplike[i]=*(lastchangelike+*(checklist+i)) + mll_mean(*(y+tstar)-*(y+*(checklist+i)),*(y2+tstar)-*(y2+*(checklist+i)),tstar-*(checklist+i))+*pen;
		}
		min_which(tmplike,*nchecklist,&minout,&whichout); /*updates minout and whichout with min and which element */
		*(lastchangelike+tstar)=minout;
		*(lastchangecpts+tstar)=*(checklist+whichout); *(lastchangecpts+*ndone+*nupdate+tstar)=tstar;		

		/* Update checklist for next iteration, first element is next tau */
		nchecktmp=0;
		for(i=0;i< *nchecklist;i++){
			if(tmplike[i]<= (*(lastchangelike+tstar)+*pen)){
				*(checklist+nchecktmp)=*(checklist+i);
				nchecktmp+=1;
			}
		}
		*(checklist+nchecktmp)=tstar;  // atleast 1 obs per seg
		nchecktmp+=1;
		*nchecklist=nchecktmp;
	} // end taustar
	
	// put final set of changepoints together
	int ncpts=0;
	int last=*ndone+*nupdate;
	while(last!=0){
		*(cptsout+ncpts)=*(lastchangecpts+*ndone+*nupdate+last);
		last=*(lastchangecpts+last);
		ncpts+=1;
	}

  free(tmpt);
err5:  free(tmplike);
err4:  return;
}

void cpmMLEStudent1(double *S, double *S2, int *nS, int *n, double *Ds)
{
	// RK modified cpmMLEStudent function from cpm package to do 1 cpt location
	// removed nb argument as was set to 0 in all relevant calls
	// W summary statistic from cpm is (n-1)*Var, I have switched it to use S and S2 as summary 
	// statistics and so this function now calculates W first

	double W; // for storing the calculated summary statistic
	W= *S2 - ((*S^2)/ *nS); // same as cpm@windowStatistic$W

	int i,j;
	double temp,E,sigma;	//sigma is standard deiation of test stasitic
	sigma = *nS -2;	//degrees of freedom
	sigma = (sqrt(sigma/(sigma-2)));

	// the following does a cpt at i
	for (i = 1; i < *nS-2; i++) {
		j = i+1; 		
		temp = *n*S[i] - j*S[*nS-1];
		E = temp*temp / (*n*j*(*n-j));
		Ds[i] = sqrt( (*n-2)*E / (W[*nW-1]-E)) / sigma;
		//Rprintf("%d: %f \n ",i,Ts[i]);	
	}
}

void cpmMLEStudent(double *S, int *nS, double *W, int *nW, int *n, int *nb, double *Ds)
{
	int i,j;
	double temp,E,sigma;	//sigma is standard deiation of test stasitic
	sigma = (*nb + *nS) -2;	//degrees of freedom
	sigma = (sqrt(sigma/(sigma-2)));


	for (i = 1; i < *nS-2; i++) {
		j = i+1; 		
		temp = *n*S[i] - j*S[*nS-1];
		E = temp*temp / (*n*j*(*n-j));
		Ds[i] = sqrt( (*n-2)*E / (W[*nW-1]-E)) / sigma;
		//Rprintf("%d: %f \n ",i,Ts[i]);	
	}
}


void cpmMLEMW(double *X, int *nX, int *N, int *nN, int *ranks, int *nranks, double *Ds)
{
	int i;
	double n0,n1,R1,U,mu,sd;
    int n = N[*nN-1];
    
	double *cumsums;
	cumsums = malloc(*nranks * sizeof(double));
	
	cumsums[0] =  ranks[0];
	
	for (i = 1 ; i < *nranks; i++) {
		cumsums[i] = cumsums[i-1] + ranks[i];
	}

	for (i = 1; i < (*nX-2) ;i++) {
		n0 = i+1;
        n1 = n-n0;
		R1 = cumsums[i];
		
		U = R1 - ( n0*(n0+1)/2) ;		
				
		mu = n0*n1/2;			
		sd = sqrt(n0*n1*(n0+n1+1)/12);
		//Rprintf("%d: %f %f %f\n ",i,U,mu,sd);
		Ds[i] = (U-mu)/sd;		
	}
	free(cumsums);
}


