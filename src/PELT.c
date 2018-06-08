#include <R.h>
#include <Rmath.h>
#include <Rinternals.h> // RK addition
#include <R_ext/RS.h>    // RK addition
#include <R_ext/Lapack.h> // RK addition
#include <R_ext/BLAS.h> // RK addition
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#define SWAP(a,b)   { int t; t=a; a=b; b=t; }  // Macro for swapping

//static int *checklist;
static double *tmplike;
static int *tmpt;

void FreePELT(error)
int *error; /* Error code from PELT C function, non-zero => error */
{
    if(*error==0){
       // free((void *)checklist);
        free((void *)tmplike);
        free((void *)tmpt);
    }
}

void PELT_online(cost_func, sumstat, ndone, nupdate, pen, cptsout, error, shape, minseglen, lastchangelike, lastchangecpts, checklist, nchecklist, numchangecpts)
char **cost_func;
double *sumstat;    /* Summary statistics for the time series (ndone+nupdate) */
int *ndone;            /* Length of the time series analyzed so far */
int *nupdate;            /* Length of the time series to be analysed in this update (i.e. total length is ndone+nupdate) */
double *pen;  /* Penalty used to decide if a changepoint is significant */
int *cptsout;   /* Vector of locations of the identified changepoints up to (ndone+nupdate) */
int *error;   /* 0 by default, nonzero indicates error in code */
double *shape; // only used when cost_func is the gamma likelihood
int *minseglen; //minimum segment length
double *lastchangelike;    /* Vector of likelihoods for the data up to n (to be added to), length (ndone+nupdate) */
double *lastchangecpts;    /* Vector of identified last changepoint locations up to n (to be added to), length 2*(ndone+nupdate) */
int *checklist;    /* Vector of locations of the potential last changepoint for next iteration (to be updated), max length=(ndone+nupdate) */
int *nchecklist;    /* Number in the checklist currently (to be updated) */
int *numchangecpts; //stores the current number of changepoints
{
    // R code does know.mean and fills mu if necessary
    // must have at least 1 observations for initialisation, i.e. if ndone=0 then nupdate>=1
    
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
    
    double *tmplike;
    tmplike = (double *)calloc((*nupdate+*nchecklist+1),sizeof(double));
    if (tmplike==NULL)   {
        *error = 4;
        goto err4;
    }
    
    
    int *tmpt;
    tmpt = (int *)calloc((*nupdate+*nchecklist+1),sizeof(int));
    if (tmpt==NULL)   {
        *error = 5;
        goto err5;
    }
    
    int n=*ndone+*nupdate;
    int tstar;
    int i,j,whichout,nchecktmp;
    
    void min_which();
    int min = 2*(*minseglen);
    if(*ndone==0){
        
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
        *nchecklist=2;
        checklist[0]=0;
        checklist[1]=*minseglen;
        *nupdate=*nupdate-*minseglen;
    }
    
    for(tstar=*ndone+1;tstar<(n+1);tstar++){
        R_CheckUserInterrupt(); /* checks if user has interrupted the R session and quits if true */
        
        for(i=0;i< *nchecklist;i++){
            tmplike[i]=lastchangelike[checklist[i]] + costfunction(*(sumstat+tstar)-*(sumstat+checklist[i]),*(sumstat + n + 1 +tstar)-*(sumstat + n + 1 +checklist[i]),*(sumstat + n + n + 2 +tstar)-*(sumstat + n + n + 2 +checklist[i]), tstar-checklist[i], *shape)+*pen;
        }
        min_which(tmplike,*nchecklist,&minout,&whichout); /*updates minout and whichout with min and which element */
        *(lastchangelike+tstar)=minout;
        *(lastchangecpts+tstar)=*(checklist+whichout); *(lastchangecpts+n+tstar)=tstar;
        
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
