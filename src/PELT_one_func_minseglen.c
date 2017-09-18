#include <R.h> 
#include <Rmath.h>
#include <Rinternals.h> // RK addition
#include <R_ext/RS.h>  // RK addition
#include <R_ext/Lapack.h> // RK addition
#include <R_ext/BLAS.h> // RK addition
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
//#include "cost_general_functions.c"
#define SWAP(a,b)   { int t; t=a; a=b; b=t; }  // Macro for swapping

//static int *lastchangecpts;
//static double *lastchangelike;
static int *checklist;
static double *tmplike;
static int *tmpt;

void FreePELT(error)
	int *error; /* Error code from PELT C function, non-zero => error */	
	{
	if(*error==0){
	//	free((void *)lastchangecpts);
	 // free((void *)lastchangelike);
	  free((void *)checklist);
	  free((void *)tmplike);
	  free((void *)tmpt);
	}
}

void PELT(cost_func, sumstat,n,pen,cptsout,error, shape, minseglen, lastchangelike, lastchangecpts, numchangecpts)
  char **cost_func;
  double *sumstat;    /* Summary statistic for the time series */
	int *n;			/* Length of the time series */
  double *pen;  /* Penalty used to decide if a changepoint is significant */
  int *cptsout;    /* Vector of identified changepoint locations */
  int *error;   /* 0 by default, nonzero indicates error in code */
  double *shape; // only used when cost_func is the gamma likelihood 
  int *minseglen; //minimum segment length 
  double *lastchangelike; // stores likelihood up to that time using optimal changepoint locations up to that time 
  int *lastchangecpts; // stores last changepoint locations 
  int *numchangecpts; //stores the current number of changepoints 
  {
	// R code does know.mean and fills mu if necessary

   double (*costfunction)();
double mll_trend_norm(); 
double mbic_trend_norm();
double mll_trendar_norm();
double mbic_trendar_norm();
double mll_meanar_norm();
double mbic_meanar_norm();

 if (strcmp(*cost_func,"trend.norm")==0){
costfunction = &mll_trend_norm;
} 
 else if (strcmp(*cost_func,"trend.norm.mbic")==0){
costfunction = &mbic_trend_norm;
} 
 else if (strcmp(*cost_func,"trendar.norm")==0){
   costfunction = &mll_trendar_norm;
} 
 else if (strcmp(*cost_func,"trendar.norm.mbic")==0){
   costfunction = &mbic_trendar_norm;
 } 
 else if (strcmp(*cost_func,"meanar.norm")==0){
   costfunction = &mll_meanar_norm;
 } 
 else if (strcmp(*cost_func,"meanar.norm.mbic")==0){
   costfunction = &mbic_meanar_norm;
 } 
 
  int *checklist;
  checklist = (int *)calloc(*n+1,sizeof(int));
  if (checklist==NULL)   {
    *error = 1;
    goto err1;
  }
  
  int nchecklist;
  double minout;

  double *tmplike;
  tmplike = (double *)calloc(*n+1,sizeof(double));
  if (tmplike==NULL)   {
    *error = 2;
    goto err2;
  }
  
  int *tmpt;
  tmpt = (int *)calloc(*n+1,sizeof(int));
  if (tmpt==NULL)   {
    *error = 3;
    goto err3;
  }
  
  int tstar,i,whichout,nchecktmp;
  
  void min_which();
  
  lastchangelike[0]= -*pen;
  lastchangecpts[0]=0; 
  numchangecpts[0]=0;

  int j; 
  for(j=*minseglen;j<(2*(*minseglen));j++){
    lastchangelike[j] = costfunction(*(sumstat+j),*(sumstat + *n + 1 + j),*(sumstat + (*n)*2 + 2 + j),*(sumstat + (*n)*3 + 3 + j),*(sumstat + (*n)*4 + 4 + j),*(sumstat + (*n)*5 + 5 + j),*(sumstat + (*n)*6 + 6 + j),j,0, *shape); 
  }
  
  for(j=*minseglen;j<(2*(*minseglen));j++){ 
    lastchangecpts[j] = 0;
  }
  
   for(j=*minseglen;j<(2*(*minseglen));j++){ 
    numchangecpts[j] =1;
  }
  
  
  nchecklist=2;
  checklist[0]=0;
  checklist[1]=*minseglen;
  
  for(tstar=2*(*minseglen);tstar<(*n+1);tstar++){
    R_CheckUserInterrupt(); /* checks if user has interrupted the R session and quits if true */

    if ((lastchangelike[tstar]) == 0){ 
      for(i=0;i<(nchecklist);i++){
        tmplike[i]=lastchangelike[checklist[i]] + costfunction(*(sumstat+tstar)-*(sumstat+checklist[i]),*(sumstat + *n + 1 +tstar)-*(sumstat + *n + 1 +checklist[i]),*(sumstat + (*n)*2 + 2 +tstar)-*(sumstat + (*n)*2 + 2 +checklist[i]),*(sumstat + (*n)*3 + 3 +tstar)-*(sumstat + (*n)*3 + 3 +checklist[i]),*(sumstat + (*n)*4 + 4 +tstar)-*(sumstat + (*n)*4 + 4 +checklist[i]),*(sumstat + (*n)*5 + 5 +tstar)-*(sumstat + (*n)*5 + 5 +checklist[i]),*(sumstat + (*n)*6 + 6 +tstar)-*(sumstat + (*n)*6 + 6 +checklist[i]), tstar,checklist[i], *shape)+*pen;
      }
    
    min_which(tmplike,nchecklist,&minout,&whichout); /*updates minout and whichout with min and which element */
    lastchangelike[tstar]=minout;
    lastchangecpts[tstar]=checklist[whichout]; 
    numchangecpts[tstar]=numchangecpts[lastchangecpts[tstar]]+1;
    /* Update checklist for next iteration, first element is next tau */
      nchecktmp=0;
    for(i=0;i<nchecklist;i++){
      if(tmplike[i]<= (lastchangelike[tstar]+*pen)){
        *(checklist+nchecktmp)=checklist[i];
        nchecktmp+=1;
      }
     }
     nchecklist = nchecktmp;
    }
    
    
   *(checklist+nchecklist)=tstar-(*minseglen-1);// atleast 1 obs per seg
     nchecklist+=1;
  /*  nchecklist=nchecktmp;*/
  
  } // end taustar
  
  // put final set of changepoints together
  int ncpts=0;
  int last=*n;
  while(last!=0){
     *(cptsout + ncpts) = last; 
    last=lastchangecpts[last];
    ncpts+=1;
  }
  free(tmpt);
  err3:  free(tmplike);
  err2:  free(checklist);
 // err3:  free(lastchangelike);
 // err2:  free(lastchangecpts);
  err1:  return;
}


