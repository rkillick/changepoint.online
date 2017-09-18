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

void PELT_online(cost_func,sumstat,ndone,nupdate,pen,cptsout,error,shape,minseglen,lastchangelike,lastchangecpts,checklist,nchecklist,numchangecpts)
  char **cost_func;
  double *sumstat;    /* Summary statistics for the time series (ndone+nupdate) */
  int *ndone;			/* Length of the time series analyzed so far */
  int *nupdate;			/* Length of the time series to be analyzed in this update (i.e. total length is ndone+nupdate) */
  double *pen;  /* Penalty used to decide if a changepoint is significant */
  int *cptsout;   /* Vector of locations of the identified changepoints up to (ndone+nupdate) */
  int *error;   /* 0 by default, nonzero indicates error in code */
  double *shape; // only used when cost_func is the gamma likelihood 
  int *minseglen; //minimum segment length 
  double *lastchangelike;    /* Vector of likelihoods for the data up to n (to be added to), length (ndone+nupdate) */
  int *lastchangecpts;    /* Vector of identified last changepoint locations up to n (to be added to), length 2*(ndone+nupdate) */
  int *checklist;    /* Vector of locations of the potential last changepoint for next iteration (to be updated), max length=(ndone+nupdate) */
  int *nchecklist;    /* Number in the checklist currently (to be updated) */
  int *numchangecpts; //stores the current number of changepoints 
  {
	// R code does know.mean and fills mu if necessary
	// must have atleast 1 observations for initialisation, i.e. if ndone=0 then nupdate>=1

   double (*costfunction)();
  double mll_var(); 
double mll_mean(); 
double mll_meanvar(); 
double mll_meanvar_exp(); 
double mll_meanvar_gamma(); 
double mll_meanvar_poisson(); 
double mbic_var(); 
double mbic_mean(); 
double mbic_meanvar(); 
double mbic_meanvar_exp(); 
double mbic_meanvar_gamma(); 
double mbic_meanvar_poisson(); 
   
   if (strcmp(*cost_func,"var.norm")==0){
   costfunction = &mll_var;
   }
   else if (strcmp(*cost_func,"mean.norm")==0){
   costfunction = &mll_mean;
   }  
    else if (strcmp(*cost_func,"meanvar.norm")==0){
  costfunction = &mll_meanvar;
   }
   else if (strcmp(*cost_func,"meanvar.exp")==0){
  costfunction = &mll_meanvar_exp;
  }
   else if (strcmp(*cost_func,"meanvar.gamma")==0){
  costfunction = &mll_meanvar_gamma;
  }
   else if (strcmp(*cost_func,"meanvar.poisson")==0){
  costfunction = &mll_meanvar_poisson;
  }
   else if (strcmp(*cost_func,"mean.norm.mbic")==0){
  costfunction = &mbic_mean;
  }
 else if (strcmp(*cost_func,"var.norm.mbic")==0){
  costfunction = &mbic_var;
  }
 else if (strcmp(*cost_func,"meanvar.norm.mbic")==0){
  costfunction = &mbic_meanvar;
}
 else if (strcmp(*cost_func,"meanvar.exp.mbic")==0){
  costfunction = &mbic_meanvar_exp;
}
 else if (strcmp(*cost_func,"meanvar.gamma.mbic")==0){
  costfunction = &mbic_meanvar_gamma;
}
 else if (strcmp(*cost_func,"meanvar.poisson.mbic")==0){
costfunction = &mbic_meanvar_poisson;
} 


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

	int n=*ndone+*nupdate;
	int tstar,i,j,whichout,nchecktmp;

	void min_which();
	
  if(*ndone==0){
	   int min=2*(*minseglen);
	   if(min>*nupdate){min=*nupdate;} // you might have less updates than 2*minseglen thus you can't add a change yet!
	   for(j=*minseglen;j<min;j++){
	    lastchangelike[j] = costfunction(*(sumstat+j),*(sumstat + n + 1 + j),*(sumstat + n + n + 2 + j),j, *shape); 
	    // lastchangelike[j] = mll_mean(n, sumstat, j, 0, j, *shape);
	  }
  
	  for(j=*minseglen;j<min;j++){ 
	    lastchangecpts[j] = 0;
	  }
  
	   for(j=*minseglen;j<min;j++){ 
	    numchangecpts[j] =1;
	  }

 	 *ndone=min;
 	 if(min>*nupdate){return;} // i.e. you can't add a change
	 nchecklist=2;
 	 checklist[0]=0;
 	 checklist[1]=*minseglen;
	 *nupdate=*nupdate-*minseglen;
  }
 
  for(tstar=*ndone+1;tstar<(*ndone+*nupdate+1);tstar++){
    R_CheckUserInterrupt(); /* checks if user has interrupted the R session and quits if true */

		for(i=0;i< *nchecklist;i++){
			tmplike[i]=lastchangelike[checklist[i]] + costfunction(*(sumstat+tstar)-*(sumstat+checklist[i]),*(sumstat + n + 1 +tstar)-*(sumstat + n + 1 +checklist[i]),*(sumstat + n + n + 2 +tstar)-*(sumstat + n + n + 2 +checklist[i]), tstar-checklist[i], *shape)+*pen;
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


double mll_mean(double x, double x2, int n){
	return(x2-(x*x)/n);
}


void PELT_meanvar_norm_update(ndone,y2,y,nupdate,pen,lastchangecpts,lastchangelike,checklist,nchecklist,cptsout,error)
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
	// must have atleast 3 observations for initialisation, i.e. if ndone=0 then nupdate>=3

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

	double mll_meanvar();
	void min_which();
	
	if(*ndone==0){
  	*(lastchangelike+0)= -*pen;
  	*(lastchangecpts+0)=0; *(lastchangecpts+*ndone+*nupdate+0)=0;
  	*(lastchangelike+1)=mll_meanvar(*(y+1),*(y2+1),1);
  	*(lastchangecpts+1)=0; *(lastchangecpts+*ndone+*nupdate+1)=1;
  	*(lastchangelike+2)=mll_meanvar(*(y+2),*(y2+2),2);
  	*(lastchangecpts+2)=0; *(lastchangecpts+*ndone+*nupdate+2)=2;
  	*(lastchangelike+3)=mll_meanvar(*(y+3),*(y2+3),3);
  	*(lastchangecpts+3)=0; *(lastchangecpts+*ndone+*nupdate+3)=3;

   	*nchecklist=2;
  	*(checklist+0)=0;
  	*(checklist+1)=2;
  	*ndone=3;
		*nupdate=*nupdate-3;
  }

	for(tstar=*ndone+1;tstar<(*ndone+*nupdate+1);tstar++){
    R_CheckUserInterrupt(); /* checks if user has interrupted the R session and quits if true */

		for(i=0;i< *nchecklist;i++){
			tmplike[i]=*(lastchangelike+*(checklist+i)) + mll_meanvar(*(y+tstar)-*(y+*(checklist+i)),*(y2+tstar)-*(y2+*(checklist+i)),tstar-*(checklist+i))+*pen;
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
		*(checklist+nchecktmp)=tstar-1;  // atleast 2 obs per seg
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

double mll_meanvar(double x, double x2, int n){
	double sigsq=(x2-((x*x)/n))/n;
	if(sigsq<=0){sigsq=0.00000000001;}
	return(n*(log(2*M_PI)+log(sigsq)+1)); /* M_PI is in Rmath.h  */
}

void PELT_var_norm_update(ndone,y2,nupdate,pen,lastchangecpts,lastchangelike,checklist,nchecklist,cptsout,error)
	int *ndone;			/* Length of the time series analyzed so far */
  double *y2;    /* Summary statistic for the time series (ndone+nupdate) */
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
	// must have atleast 3 observations for initialisation, i.e. if ndone=0 then nupdate>=3

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

	double mll_var();
	void min_which();
	
	if(*ndone==0){
  	*(lastchangelike+0)= -*pen;
  	*(lastchangecpts+0)=0; *(lastchangecpts+*ndone+*nupdate+0)=0;
  	*(lastchangelike+1)=mll_var(*(y2+1),1);
  	*(lastchangecpts+1)=0; *(lastchangecpts+*ndone+*nupdate+1)=1;
  	*(lastchangelike+2)=mll_var(*(y2+2),2);
  	*(lastchangecpts+2)=0; *(lastchangecpts+*ndone+*nupdate+2)=2;
  	*(lastchangelike+3)=mll_var(*(y2+3),3);
  	*(lastchangecpts+3)=0; *(lastchangecpts+*ndone+*nupdate+3)=3;

   	*nchecklist=2;
  	*(checklist+0)=0;
  	*(checklist+1)=2;
  	*ndone=3;
		*nupdate=*nupdate-3;
  }

	for(tstar=*ndone+1;tstar<(*ndone+*nupdate+1);tstar++){
    R_CheckUserInterrupt(); /* checks if user has interrupted the R session and quits if true */

		for(i=0;i< *nchecklist;i++){
			tmplike[i]=*(lastchangelike+*(checklist+i)) + mll_var(*(y2+tstar)-*(y2+*(checklist+i)),tstar-*(checklist+i))+*pen;
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
		*(checklist+nchecktmp)=tstar-1;  // atleast 2 obs per seg
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

//void PELT_var_norm_update(ndone,y2,nupdate,pen,lastchangecpts,lastchangelike,checklist,nchecklist,cptsout,error)
//	int *ndone;		/* Length of the time series analyzed so far */
//  double *y2;    /* Summary statistic for the time series (ndone+nupdate) */
//	int *nupdate;			/* Length of the time series to be analyzed in thsi update (i.e. total length is ndone+nupdate */
//  double *pen;  /* Penalty used to decide if a changepoint is significant */
//	int *lastchangecpts;		/* Vector of identified last changepoint locations up to n (to be added to), length 2*(ndone+nupdate) */
//	double *lastchangelike;		/* Vector of likelihoods for hte data up to n (to be added to), length (ndone+nupdate) */
//	int *checklist;		/* Vectors of locations of the potential last hcangepoint for next iteration (to be updated), max length=(ndone+nupdate) */
//	int *nchecklist; /* Number in the checklist currently (to be updated) */
//  int *cptsout;    /* Vector of identified changepoint locations up to (ndone+nupdate) */
//  int *error;   /* 0 by default, nonzero indicates error in code */
//  {
	// R code does know.mean and fills mu if necessary
	// Must have atleast 1 observation for initialisation, i.e. if ndone=0 then nupdate>=3


//	double minout;

  //double tmplike[*n];
//  double *tmplike;
//  tmplike = (double *)calloc((*nupdate+*nchecklist+1),sizeof(double));
//  if (tmplike==NULL)   {
//    *error = 4;
//    goto err4;
//  }

	//int tmpt[*n];
//  int *tmpt;
//  tmpt = (int *)calloc((*nupdate+*nchecklist+1),sizeof(int));
//  if (tmpt==NULL)   {
//    *error = 5;
//    goto err5;
//  }

//	int tstar,i,whichout,nchecktmp;

//	double mll_var();
//	void min_which();

//	if(*ndone==0){  
//		*(lastchangelike+0)= -*pen; /* null (last changepoint at 0) */
//		*(lastchangecpts+0)=0; *(lastchangecpts+*ndone+*nupdate+0)=0;	
//		*(lastchangelike+1)=mll_var(*(y2+1),1);
//		*(lastchangecpts+1)=0; *(lastchangecpts+*ndone+*nupdate+1)=1;
//		*(lastchangelike+2)=mll_var(*(y2+2),2);
//		*(lastchangecpts+2)=0; *(lastchangecpts+*ndone+*nupdate+2)=2;
//		*(lastchangelike+3)=mll_var(*(y2+3),3);
//		*(lastchangecpts+3)=0; *(lastchangecpts+*ndone+*nupdate+3)=3;

//		*nchecklist=2;
//		*(checklist+0)=0;
//		*(checklist+1)=2;
//		*ndone=3;
//		*nupdate=*nupdate-3;
//	}

//	for(tstar=*ndone+1;tstar<(*ndone+*nupdate+1);tstar++){
//    R_CheckUserInterrupt(); /* checks if user has interrupted the R session and quits if true */

//    for(i=0;i< *nchecklist;i++){
//			tmplike[i]=*(lastchangelike+*(checklist+i)) + mll_var(*(y2+tstar)-*(y2+*(checklist+i)),tstar-*(checklist+i))+*pen;
//		}
//		min_which(tmplike,*nchecklist,&minout,&whichout); /*updates minout and whichout with min and which element */
//		*(lastchangelike+tstar)=minout;
//		*(lastchangecpts+tstar)=*(checklist+whichout); *(lastchangecpts+*ndone+*nupdate+tstar)=tstar;

		/* Update checklist for next iteration, last element is next tau */
//		nchecktmp=0;
//		for(i=0;i< *nchecklist;i++){
//			if(tmplike[i]<= (*(lastchangelike+tstar)+*pen)){
//				*(checklist+nchecktmp)= *(checklist+i);
//				nchecktmp+=1;
//			}
//		}
//		*(checklist+nchecktmp)=tstar-1;  // atleast 2 obs per seg
//		nchecktmp+=1;
//		*nchecklist=nchecktmp;
//	} // end taustar
	
	// put final set of changepoints together
//	int ncpts=0;
//	int last=*ndone+*nupdate;
//	while(last!=0){
//		*(cptsout+ncpts)= *(lastchangecpts+*ndone+*nupdate+last);
//		last= *(lastchangecpts+last);
//		ncpts+=1;
//	}
//  free(tmpt);
//err5:  free(tmplike);
//err4:  return;
//}

double mll_var(double x, int n){
	if(x<=0){x=0.00000000001;}
	return(n*(log(2*M_PI)+log(x/n)+1)); /* M_PI is in Rmath.h  */
}

void PELT_mean_mv_norm_update(p,ndone,y2,y,nupdate,pen,lastchangecpts,lastchangelike,checklist,nchecklist,cptsout,error)
  int *p;        /* Number of dimensions */
  int *ndone;			/* Length of the time series analyzed so far */
  double *y2;    /* Summary statistic for the time series (ndone+nupdate)xp */
	double *y;		/*Summary statistics for the time series (ndone+nupdate)xp */
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
  int N=*ndone+*nupdate;

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

	double mll_mean_mv();
	void min_which();
	
	if(*ndone==0){
  	*(lastchangelike+0)= -*p * *pen;
  	*(lastchangecpts+0)=0; *(lastchangecpts+N+0)=0;
  	*(lastchangelike+1)=mll_mean_mv((y+1),(y2+1),1,N+1,*p);
  	*(lastchangecpts+1)=0; *(lastchangecpts+N+1)=1;

   	*nchecklist=2;
  	*(checklist+0)=0;
  	*(checklist+1)=1;
  	*ndone=1;
		*nupdate=*nupdate-1;
  }

	for(tstar=*ndone+1;tstar<(N+1);tstar++){
    R_CheckUserInterrupt(); /* checks if user has interrupted the R session and quits if true */

		for(i=0;i< *nchecklist;i++){
			tmplike[i]=*(lastchangelike+*(checklist+i)) + mll_mean_mv((y+tstar),(y2+tstar),tstar-*(checklist+i),N+1,*p)+(*p * *pen);
		}
		min_which(tmplike,*nchecklist,&minout,&whichout); /*updates minout and whichout with min and which element */
		*(lastchangelike+tstar)=minout;
		*(lastchangecpts+tstar)=*(checklist+whichout); *(lastchangecpts+N+tstar)=tstar;		

		/* Update checklist for next iteration, first element is next tau */
		nchecktmp=0;
		for(i=0;i< *nchecklist;i++){
			if(tmplike[i]<= (*(lastchangelike+tstar)+(*p * *pen))){
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
	int last=N;
	while(last!=0){
		*(cptsout+ncpts)=*(lastchangecpts+N+last);
		last=*(lastchangecpts+last);
		ncpts+=1;
	}

  free(tmpt);
err5:  free(tmplike);
err4:  return;
}

double mll_mean_mv(double *x, double *x2, int n, int N, int p){
  /* &x/&x2 is the reference for the end of the data segment, e.g. tstar in the PELT algorithm
     then n is the length of the segment, i.e. tstar-n is the checklist point in the PELT algorithm
     then we need N to be the nrows in the matrix so we can move from one stream to another
  */
  int i,tstar;
  double sum=0;
  for(i=0;i<p;i++){ /* summing over p dimensions as they are considered independent */
    sum+=(*(x2+i*N)-*(x2-n+i*N)) - (((*(x+i*N)-*(x-n+i*N))*(*(x+i*N)-*(x-n+i*N))) / n);
//printf("p=%d,%f,%f \n",p,*(x2+i*N)-*(x2-n+i*N),(*(x+i*N)-*(x-n+i*N)));
  } /* just doing (x2-(x*x)/n) but calculating x2 and x using the cumulative vectors */
  return(sum);
}








void min_which(double *array,int n,double *minout,int *whichout){
	/* Function to find minimum of an array with n elements that is put in min */
	*minout=*array;
	*whichout=0;
	int i;
	for(i=1;i<n;i++){
		if(*(array+i)< *minout){
			*minout= *(array+i);
			*whichout=i;
		}
	}
}

void max_which(double *array,int n,double *maxout,int *whichout){
	/* Function to find maximum of an array with n elements that is put in max */
	*maxout=*array;
	*whichout=0;
	int i;
	for(i=1;i<n;i++){
		if(*(array+i)> *maxout){
			*maxout= *(array+i);
			*whichout=i;
		}
	}
}

void order_vec( int a[], int n ){   
	int i, j;
	for(i = 0; i < n; i++){         // Make a pass through the array for each element
	  for(j = 1; j < (n-i); j++){			// Go through the array beginning to end
			if(a[j-1] > a[j])       // If the the first number is greater, swap it 
				SWAP(a[j-1],a[j]);   
		}
	}
}

