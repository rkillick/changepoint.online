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

#define SWAP(a,b)   { int t; t=a; a=b; b=t; }  // Macro for swapping

// Cost functions  

double mll_trend_norm(double x, double x2, double x3, double x4, double x5, double x6, double x7, int tstar,int checklist, double shape){
  double n=tstar-checklist;
  double newx3=x3-x*checklist;
  double thetaS=(2*x*(2*n + 1) - 6*newx3) / (2*n*(2*n + 1) - 3*n*(n+1));
  double thetaT=(6*newx3)/((n+1)*(2*n+1)) + (thetaS * (1-((3*n)/((2*n)+1))));
  double out= n*log(2*M_PI)+x2 - 2*thetaS*x - 2*((thetaT-thetaS)/n)*newx3 + n*thetaS*thetaS + thetaS*(thetaT-thetaS)*(n+1) + (thetaT - thetaS)*(thetaT-thetaS) * (n+1)*(2*n+1)/(6*n);
  if(isfinite(out)){return(out);}
  else{return(log(2*M_PI));}
}

double mbic_trend_norm(double x, double x2, double x3, double x4, double x5,double x6, double x7, int tstar,int checklist, double shape){
  double n=tstar-checklist;
  double newx3=x3-x*checklist;
  double thetaS=(2*x*(2*n + 1) - 6*newx3) / (2*n*(2*n + 1) - 3*n*(n+1));
  double thetaT=(6*newx3)/((n+1)*(2*n+1)) + (thetaS * (1-((3*n)/((2*n)+1))));
  double out= n*log(2*M_PI)+x2 - 2*thetaS*x - 2*((thetaT-thetaS)/n)*newx3 + n*thetaS*thetaS + thetaS*(thetaT-thetaS)*(n+1) + (thetaT - thetaS)*(thetaT-thetaS) * (n+1)*(2*n+1)/(6*n);
  if(!isfinite(out)){return(log(2*M_PI)+log(n));}
  else{return(out+log(n));}
}

double mll_meanar_norm(double x1, double x2, double x3, double x4, double x5, double x6, double x7, int tstar,int checklist, double shape){
  double n=tstar-checklist;
  double beta2=(n*x3-x1*x2)/(n*x7*(1-x2*x2));
  double beta1=(x1-beta2*x2)/n;
  double out= n*log(2*M_PI)+x6+n*beta1*beta1-2*beta1*x1+beta2*beta2*x7-2*beta2*x3+2*beta1*beta2*x2;
  if(isfinite(out)){return(out);}
  else{return(log(2*M_PI));}
}

double mbic_meanar_norm(double x1, double x2, double x3, double x4, double x5, double x6, double x7, int tstar,int checklist, double shape){
  double n=tstar-checklist;
  double beta2=(n*x3-x1*x2)/(n*x7*(1-x2*x2));
  double beta1=(x1-beta2*x2)/n;
  double out= n*log(2*M_PI)+x6+n*beta1*beta1-2*beta1*x1+beta2*beta2*x7-2*beta2*x3+2*beta1*beta2*x2;
  if(isfinite(out)){return(out+log(n));}
  else{return(log(2*M_PI)+log(n));}
}

double mll_trendar_norm(double x1, double x2, double x3, double x4, double x5, double x6, double x7,int tstar,int checklist, double shape){
  double n=tstar-checklist;
  double newx4=x4-x1*checklist;
  double newx5=x5-x2*checklist;
  double betatop=pow(n,2)*(n-1)*(n+1)*x3 + 2*pow(n,2)*(n+1)*newx5*((2*n+1)*x1-3*newx4)+2*((2*n+1)*x1-3*newx4)*((n-1)*newx5-n*(n+1)*x2);
  double betabottom=pow(n,2)*(n-1)*(n+1)*x7 - 2*(n+1)*x2*(n*(2*n+1)*x2-(2*n-1)*(pow(n,2)+n+1)*newx5) - 6*(n+1)*newx5*(pow(n-1,2)-n*x2);
  double beta=betatop/betabottom;  
  double thetajpo=(6*(newx4-beta*newx5))/(n*(n+1)) - (2*(x1-beta*x2))/n;
  double thetaj=(2*(2*n+1)*(x1-beta*x2)-6*(newx4-beta*newx5))/(n*(n-1));
  double out=n*log(2*M_PI)+x6+pow(beta,2)*x7-2*beta*x3+thetaj*beta*(2*x2-(2/n)*newx5)+thetajpo*beta*(2/n)*newx5+pow(thetaj,2)*(-1+((n+1)*(2*n+1))/(6*n))+(pow(thetajpo,2)*(n+1)*(2*n+1))/(6*n)+thetaj*((2/n)*newx4-2*x1)-thetajpo*(2/n)*newx4+thetaj*thetajpo*(n+1-((n+1)*(2*n+1))/(3*n));
  if(isfinite(out)){return(out);}
  else{return(n*log(2*M_PI));}
}

double mbic_trendar_norm(double x1, double x2, double x3, double x4, double x5, double x6, double x7,int tstar,int checklist, double shape){
  double n=tstar-checklist;
  double newx4=x4-x1*checklist;
  double newx5=x5-x2*checklist;
  double betatop=pow(n,2)*(n-1)*(n+1)*x3 + 2*pow(n,2)*(n+1)*newx5*((2*n+1)*x1-3*newx4)+2*((2*n+1)*x1-3*newx4)*((n-1)*newx5-n*(n+1)*x2);
  double betabottom=pow(n,2)*(n-1)*(n+1)*x7 - 2*(n+1)*x2*(n*(2*n+1)*x2-(2*n-1)*(pow(n,2)+n+1)*newx5) - 6*(n+1)*newx5*(pow(n-1,2)-n*x2);
  double beta=betatop/betabottom;  
  double thetajpo=(6*(newx4-beta*newx5))/(n*(n+1)) - (2*(x1-beta*x2))/n;
  double thetaj=(2*(2*n+1)*(x1-beta*x2)-6*(newx4-beta*newx5))/(n*(n-1));
  double out=n*log(2*M_PI)+x6+pow(beta,2)*x7-2*beta*x3+thetaj*beta*(2*x2-(2/n)*newx5)+thetajpo*beta*(2/n)*newx5+pow(thetaj,2)*(-1+((n+1)*(2*n+1))/(6*n))+(pow(thetajpo,2)*(n+1)*(2*n+1))/(6*n)+thetaj*((2/n)*newx4-2*x1)-thetajpo*(2/n)*newx4+thetaj*thetajpo*(n+1-((n+1)*(2*n+1))/(3*n));
  if(isfinite(out)){return(out+log(n));}
  else{return(n*log(2*M_PI)+log(n));}
}

void max_which(double *array,int n,double *maxout,int *whichout){
  // Function to find maximum of an array with n elements that is put in max 
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

void min_which(double *array,int n,double *minout,int *whichout){
  // Function to find minimum of an array with n elements that is put in min 
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

void order_vec( int a[], int n ){   
  int i, j;
  for(i = 0; i < n; i++){         // Make a pass through the array for each element
                                  for(j = 1; j < (n-i); j++){  		// Go through the array beginning to end
                                                                 if(a[j-1] > a[j])       // If the the first number is greater, swap it 
                                                                 SWAP(a[j-1],a[j]);   
                                  }
  }
}

