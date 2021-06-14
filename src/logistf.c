#include <math.h>
#include <R.h>
#include <Rdefines.h>
#include "memory.h"
#include "Rmath.h"
#include "veclib.h"

void logistffit_revised(double *x, int *y, int *n_l, int *k_l, 
                double *weight, double *offset, 
                double *beta,
                int *colfit, int *ncolfit_l, int *firth_l, 
                int *maxit, double *maxstep, int *maxhs,
                double *lconv, double *gconv, double *xconv, double* tau, 
                // output: 
                double *fisher_cov,		// k x k
                double *Ustar,				// k
                double *pi,						// n
                double *Hdiag,				// n
                double *loglik,				// 1
                int *evals,
                int *iter,
                double *convergence		// 3
)
{
  long n = (long)*n_l, k = (long)*k_l, firth = (long)*firth_l, ncolfit = (long)*ncolfit_l;
  double wi, logdet;
  long i, j, halfs;
  double loglik_old, loglik_change = 5.0;
  
  double *xt;
  double *xw2;
  double *xw2t;
  double *tmp;
  double *beta_old;
  double *w;
  double *xw2_reduced_augmented;
  double *xw2_reduced_augmented_t;
  double *fisher_cov_reduced_augmented;
  double *cov_augmented;
  double *delta;
  int *selcol;
  
  // memory allocations
  if (NULL == (xt = (double *) R_alloc(n * k, sizeof(double)))){ error("no memory available\n");}
  if (NULL == (xw2 = (double *) R_alloc(k * n, sizeof(double)))){ error("no memory available\n");}
  if (NULL == (xw2t = (double *) R_alloc(n * k, sizeof(double)))){ error("no memory available\n");}
  if (NULL == (tmp = (double *) R_alloc(n * k, sizeof(double)))){ error("no memory available\n");}
  if (NULL == (beta_old = (double *) R_alloc(k, sizeof(double)))){ error("no memory available\n");}
  if (NULL == (w = (double *) R_alloc(n, sizeof(double)))){ error("no memory available\n");}
  if (NULL == (xw2_reduced_augmented = (double *) R_alloc(3*n*ncolfit, sizeof(double)))){ error("no memory available\n");}
  if (NULL == (xw2_reduced_augmented_t = (double *) R_alloc(3*n*ncolfit, sizeof(double)))){ error("no memory available\n");}
  if (NULL == (fisher_cov_reduced_augmented = (double *) R_alloc(ncolfit*ncolfit, sizeof(double)))){ error("no memory available\n");}
  if (NULL == (cov_augmented = (double *) R_alloc(k*k, sizeof(double)))){ error("no memory available\n");}
  if (NULL == (delta = (double *) R_alloc(k, sizeof(double)))){ error("no memory available\n");}
  if (NULL == (selcol = (int *) R_alloc(ncolfit, sizeof(double)))){ error("no memory available\n");}
  
  //Initialise delta: 
  for(i=0; i < k; i++) {
    delta[i] = 0.0;
  }
  
  // which columns to select based on the columns to fit:
  for(i=0; i < ncolfit; i++){
    selcol[i] = colfit[i] - 1;
  }
    
  // Calculate initial likelihood
  trans(x, xt, n, k);
  XtY(xt, beta, pi, k, n, 1);	//init of pred prob
  for(i = 0; i < n; i++){
    pi[i] = 1.0 / (1.0 + exp( - pi[i] - offset[i]));	
  }
  
  //Calculation of hat matrix diagonal; needed for loglik calculation on augmented dataset and in first iteration of main loop
  //-- Calculation of X^T W^(1/2)
  for(i = 0; i < n; i++) { 
    wi = sqrt(weight[i] * pi[i] * (1.0 - pi[i]));
    for(j = 0; j < k; j++)
      xw2[i*k + j] = x[i + j*n] * wi; 
  }
  //-- Transpose: W^(1/2)^T X
  trans(xw2, xw2t, k, n);
  //-- Calculation of XWX
  XtXasy(xw2t, fisher_cov, n, k);
  //-- Invert:
  linpack_det(fisher_cov, &k, &logdet);
  if (logdet < (-50)) {	
    error("Determinant of Fisher information matrix was %lf \n", exp(logdet));
  }
  else {
    linpack_inv(fisher_cov, &k); 
  }
  //-- Calculation of X^T W^(1/2) (X^TWX)^(-1)
  XtY(xw2, fisher_cov, tmp, k, n, k);
  XYdiag(tmp, xw2, Hdiag, n, k);

  
  // Calculation of loglikelihood using augmented dataset if firth:
  *loglik = 0.0;
  for(i = 0; i < n; i++){ //TODO: better solution? maybe augment w and y but then more memory occupation
    *loglik += y[i] * weight[i] * log(pi[i]) + (1-y[i]) * weight[i] * log(1-pi[i]);
  }
  if(firth){
    for(i=0; i<n; i++){
      // weight first replication of dataset with h_i * tau 
      *loglik += y[i] * Hdiag[i] * *tau * log(pi[i]) + (1-y[i]) * (Hdiag[i] * *tau) * log(1-pi[i]);
      // weight second replication of dataset with 2 * h_i * tau with opponent y
      *loglik += (1-y[i]) * Hdiag[i] * *tau * log(pi[i]) + y[i] *  Hdiag[i] * *tau * log(1-pi[i]);
    }
  }
  
  //Calculation of initial U*:
  if(firth){
    for(i=0; i < n; i++){
      w[i] = weight[i] * ((double)y[i]-pi[i] + 2 * *tau * Hdiag[i] * (0.5 - pi[i]));
    }
  } else {
    for(i=0; i < n; i++){
      w[i] = weight[i] * ((double)y[i] - pi[i]);
    }
  }
  XtY(x, w, Ustar, n, k, 1);

  *evals = 1, *iter = 0;
  int bStop = 0;
  
  //Start of iteration: 
  if(*maxit > 0){ // in case of maxit == 0 only evaluate likelihood
    for(;;){
      Rprintf("Iter: %d \n", *iter);
    //--Save iteration values:
      loglik_old = *loglik;
      copy(beta, beta_old, k);

      //--Calculation of (X^TWX)^(-1) using augmented dataset and only columns in selcol (columns to fit: colfit - 1)
      if(ncolfit > 0 && (selcol[0] != -1)) { // selcol[0] == -1 in case of just evaluating likelihood
        //-- Calculation of X^T W^(1/2)
        //---- XW^(1/2)
        // TODO: find better calculation method than ifelse
        for(i = 0; i < (3*n); i++) {
          if(i < n){
            wi = sqrt(weight[i] * pi[i] * (1.0 - pi[i])); 
          } else if(i < 2*n){
            wi = sqrt(weight[i] * *tau * Hdiag[(i % n)] * pi[(i % n)] * (1.0 - pi[(i % n)])); 
          } else {
            wi = sqrt(weight[i] * *tau * Hdiag[(i % n)] * pi[(i % n)] * (1.0 - pi[(i % n)]));
          }
          for(j = 0; j < ncolfit; j++){
            xw2_reduced_augmented[i*ncolfit + j] = xt[(i % n)*ncolfit + selcol[j]] * wi;
          }
        }
        
        //---- W^(1/2)^TX^T
        trans(xw2_reduced_augmented, xw2_reduced_augmented_t, ncolfit, (3*n)); 
        XtXasy(xw2_reduced_augmented_t, fisher_cov_reduced_augmented, (3*n), ncolfit);
        linpack_inv(fisher_cov_reduced_augmented, &ncolfit);
        
        //---- Remapping of fisher_cov_(reduced)_augmented
        for(i = 0; i < k*k; i++) { //Initialisation:
          cov_augmented[i] = 0.0;
        }
        for(i=0; i < ncolfit; i++){
          for(j=0; j < ncolfit; j++) {
            cov_augmented[selcol[i] + k*selcol[j]] = fisher_cov_reduced_augmented[i + ncolfit*j];
          }
        }
        
        // Actual computation of delta:
        XtY(cov_augmented, Ustar, delta, k, k, 1);
        
        // Check for maxstep:
        double mx = maxabs(delta, k) / *maxstep;
        if(mx > 1.0){
          for(i=0; i < k; i++) {
            delta[i] /= mx;
          }
        }
      }
      
      //Update coefficient vector beta:
      for(i=0; i < k; i++){
        beta[i] += delta[i];
      }
      
      //Start step-halvings
      for(halfs = 1; halfs <= *maxhs; halfs++) {
        //Calculate loglik:
        //--Update pi:
        XtY(xt, beta, pi, k, n, 1);
        for(i = 0; i < n; i++){
          pi[i] = 1.0 / (1.0 + exp( - pi[i] - offset[i]));	
        }
        
        //--Calculation of hat matrix diagonal;
        //-- Calculation of X^T W^(1/2)
        for(i = 0; i < n; i++) { 
          wi = sqrt(weight[i] * pi[i] * (1.0 - pi[i]));
          for(j = 0; j < k; j++)
            xw2[i*k + j] = x[i + j*n] * wi; 
        }
        //-- Transpose: W^(1/2)^T X
        trans(xw2, xw2t, k, n);
        //-- Calculation of XWX
        XtXasy(xw2t, fisher_cov, n, k);
        //-- Invert:
        linpack_inv(fisher_cov, &k); 
        //-- Calculation of X^T W^(1/2) (X^TWX)^(-1)
        XtY(xw2, fisher_cov, tmp, k, n, k);
        XYdiag(tmp, xw2, Hdiag, n, k);
        
        // Calculation of loglikelihood using augmented dataset if firth:
        *loglik = 0.0;
        for(i = 0; i < n; i++){ //TODO: better solution? maybe augment w and y but then more memory occupation
          *loglik += y[i] * weight[i] * log(pi[i]) + (1-y[i]) * weight[i] * log(1-pi[i]);
        }
        if(firth){
          for(i=0; i<n; i++){
            // weight first replication of dataset with 1+ h_i * tau
            *loglik += weight[i] * y[i] * Hdiag[i] * *tau * log(pi[i]) + weight[i] * (1-y[i]) * (Hdiag[i] * *tau) * log(1-pi[i]);
            // weight second replication of dataset with h_i * tau with oppenent y
            *loglik += weight[i] * (1-y[i]) * Hdiag[i] * *tau * log(pi[i]) + weight[i] * y[i] * Hdiag[i] * *tau * log(1-pi[i]);
          }
        }
        //Increase evaluation counter
        (*evals)++;
        
        //Calculation of U*: (needed just as a return value)
        if(firth){
          for(i=0; i < n; i++){
            w[i] = weight[i] * ((double)y[i]-pi[i] + 2 * *tau * Hdiag[i] * (0.5 - pi[i]));
          }
        } else {
          for(i=0; i < n; i++){
            w[i] = weight[i] * ((double)y[i] - pi[i]);
          }
        }
        XtY(x, w, Ustar, n, k, 1);
        
        //Convergence check:
        if(*loglik >= (loglik_old - *lconv)){
           break;
        }
        //Update beta:
        for(i=0; i < k; i++){
          delta[i] /= 2.0;
          beta[i] -= delta[i];
        }
      }
      
      if(*maxhs == 0){ //if no half stepping: Update pi and compute Hdiag for the next iteration + compute loglik to check for convergence
        //Update predicted prob: 
        XtY(xt, beta, pi, k, n, 1);
        for(i = 0; i < n; i++){
          pi[i] = 1.0 / (1.0 + exp( - pi[i] - offset[i]));	
        }
        //Calculation of hat matrix diagonal for next iteration; needed for loglik calculation on augmented dataset
        //If step halfing is activated - Hdiag is computed there
        //-- Calculation of X^T W^(1/2)
        for(i = 0; i < n; i++) { 
          wi = sqrt(weight[i] * pi[i] * (1.0 - pi[i]));
          for(j = 0; j < k; j++)
            xw2[i*k + j] = x[i + j*n] * wi; 
        }
        //-- Transpose: W^(1/2)^T X
        trans(xw2, xw2t, k, n);
        //-- Calculation of XWX
        XtXasy(xw2t, fisher_cov, n, k);
        //-- Invert:
        linpack_det(fisher_cov, &k, &logdet);
        if (logdet < (-50)) {	
          error("Determinant of Fisher information matrix was %lf \n", exp(logdet));
        }
        else {
          linpack_inv(fisher_cov, &k); 
        }
        //-- Calculation of X^T W^(1/2) (X^TWX)^(-1)
        XtY(xw2, fisher_cov, tmp, k, n, k);
        XYdiag(tmp, xw2, Hdiag, n, k);
        // Calculation of loglikelihood using augmented dataset if firth:
        *loglik = 0.0;
        for(i = 0; i < n; i++){ //TODO: better solution? maybe augment w and y but then more memory occupation
          *loglik += y[i] * weight[i] * log(pi[i]) + (1-y[i]) * weight[i] * log(1-pi[i]);
        }
        if(firth){
          for(i=0; i<n; i++){
            // weight first replication of dataset with 1+ h_i * tau
            *loglik += weight[i] * y[i] * Hdiag[i] * *tau * log(pi[i]) + weight[i] * (1-y[i]) * (Hdiag[i] * *tau) * log(1-pi[i]);
            // weight second replication of dataset with h_i * tau with opponent y
            *loglik += weight[i] * (1-y[i]) * Hdiag[i] * *tau * log(pi[i]) + weight[i] * y[i] * Hdiag[i] * *tau * log(1-pi[i]);
          }
        }
        //Increase evaluation counter
        (*evals)++;
        
        //Calculation of U*:
        if(firth){
          for(i=0; i < n; i++){
            w[i] = weight[i] * ((double)y[i]-pi[i] + 2 * *tau * Hdiag[i] * (0.5 - pi[i]));
          }
        } else {
          for(i=0; i < n; i++){
            w[i] = weight[i] * ((double)y[i] - pi[i]);
          }
        }
        XtY(x, w, Ustar, n, k, 1);
      }
      
      loglik_change = *loglik - loglik_old;
      
      //Check convergence of main loop:
      if((*iter >= *maxit) || (
        (maxabsInds(delta, selcol, ncolfit) <= *xconv) && 
          (maxabsInds(Ustar, selcol, ncolfit) < *gconv) &&
          (loglik_change < *lconv))){
        bStop = 1;
      }
      //if((*iter < *maxit) && (halfs >= *maxhs) && (*maxhs >= 5) && (*loglik < loglik_old)){
      //  bStop = 1;   // stop if half-stepping with at least 5 steps was not successful;
      //}
      if(bStop){
        break;
      }
      //Increase iteration counter
      (*iter)++;
      
      Rprintf("loglik: %f \n", *loglik);
      Rprintf("Ustar:");
      for(i=0; i<k; i++){
        Rprintf("%f ", Ustar[i]);
      }
      Rprintf("\n");
      Rprintf("beta:");
      for(i=0; i<k; i++){
        Rprintf("%f ", beta[i]);
      }
      Rprintf("\n");
      
    } //End of iterations
    
    if(ncolfit > 0 && (selcol[0] != -1)) {
      copy(cov_augmented, fisher_cov, k*k);
    }
    
    convergence[0] = loglik_change;
    convergence[1] = maxabsInds(Ustar, selcol, ncolfit);
    convergence[2] = maxabsInds(delta, selcol, ncolfit);
  }
}



void logistffit_IRLS(double *x, int *y, int *n_l, int *k_l, 
								double *weight, double *offset, 
								double *beta, // beta is I/O
								int *colfit, int *ncolfit_l, int *firth_l, 
								int *maxit, double *maxstep, int *maxhs,
								double *lconv, double *gconv, double *xconv, double* tau, 
								// output: 
								double *fisher_cov,		// k x k
								double *pi,						// n
								double *Hdiag,				// n
								double *loglik,
								int *evals,
								int *iter,
								double *convergence		// 3
){
	long n = (long)*n_l, k = (long)*k_l, ncolfit = (long)*ncolfit_l;
	double wi;
	long i, j;
	// memory allocations

	double *xt;
	double *beta_old;
	double *xw2;
	double *xw2t;
	double *tmpNxK;
	int *selcol;
	double *newresponse; // newresponse of IRLS
	double *delta;
	double *fisher_cov_reduced_augmented; //reduced versions in case only a subset of variables should be fitted
	double *xw2_reduced_augmented;
	double *xw2t_reduced_augmented;
	double *tmp1_reduced;
	double *covs_full;
	double *xw2_reduced;

	 if (NULL == (xt = (double *) R_alloc(k * n, sizeof(double)))){error("no memory available\n");}
	 if (NULL == (beta_old = (double *) R_alloc(k ,sizeof(double)))){error("no memory available\n");}
	 if (NULL == (xw2 = (double *) R_alloc(k * n, sizeof(double)))){error("no memory available\n");}
	 if (NULL == (xw2t = (double *) R_alloc(n * k, sizeof(double)))){error("no memory available\n");}
	 if (NULL == (tmpNxK = (double *) R_alloc(n * k, sizeof(double)))){error("no memory available\n");}
	 if (NULL == (selcol = (int *) R_alloc(ncolfit, sizeof(int)))){error("no memory available\n");}
	 if (NULL == (newresponse = (double *) R_alloc(n, sizeof(double)))){error("no memory available\n");}
	 if (NULL == (delta = (double *) R_alloc( k, sizeof(double)))){error("no memory available\n");}
	 if (NULL == (fisher_cov_reduced_augmented = (double *) R_alloc(ncolfit* ncolfit, sizeof(double)))){error("no memory available\n");}
	 if (NULL == (xw2_reduced_augmented = (double *) R_alloc(ncolfit* n * 3, sizeof(double)))){error("no memory available\n");}
	 if (NULL == (xw2t_reduced_augmented = (double *) R_alloc(ncolfit* n * 3, sizeof(double)))){error("no memory available\n");}
	 if (NULL == (tmp1_reduced = (double *) R_alloc(ncolfit* n, sizeof(double)))){error("no memory available\n");}
	 if (NULL == (covs_full = (double *) R_alloc(k* k, sizeof(double)))){error("no memory available\n");}
	 if (NULL == (xw2_reduced = (double *) R_alloc(ncolfit* n, sizeof(double)))){error("no memory available\n");}
	
	
	trans(x, xt, n, k);
	
	// init loglik
	double loglik_old, loglik_change = 5.0;
	
	*evals = 0, *iter = 0;
	int bStop = 0;
	
	//init delta: difference between beta values in iteration i-1 and i
	//and beta
	for(i=0; i < k; i++){
			delta[i] = 5.0;
	 }
	
	for(i=0; i < ncolfit; i++){
	    selcol[i] = colfit[i] - 1;
	 }
	
	//Calculate initial likelihood and Hdiag for first iteration:
	// calculation of pi
	XtY(xt, beta, pi, k, n, 1);
	for(i = 0; i < n; i++){
	  pi[i] = 1.0 / (1.0 + exp( - pi[i] - offset[i]));
	} 
	
	// XW^(1/2)
	for(i = 0; i < n; i++) {
	  wi = sqrt(weight[i] * pi[i] * (1.0 - pi[i])); 
	  for(j = 0; j < k; j++){
	    xw2[i*k + j] = x[i + j*n] * wi;
	  }
	}
	
	//Calculation of Hat diag:
	trans(xw2, xw2t, k, n); //W^(1/2)^TX^T
	XtXasy(xw2t, fisher_cov, n, k); //X^TWX
	linpack_inv(fisher_cov, &k); 
	XtY(xw2, fisher_cov, tmpNxK, k, n, k);
	XYdiag(tmpNxK, xw2, Hdiag, n, k);
	
	// Calculation of loglikelihood using augmented dataset if firth:
	*loglik = 0.0;
	for(i = 0; i < n; i++){ //TODO: better solution? maybe augment w and y but then more memory occupation
	  *loglik += y[i] * weight[i] * log(pi[i]) + (1-y[i]) * weight[i] * log(1-pi[i]);
	  // weight first replication of dataset with 1+ h_i * tau 
	  *loglik += y[i] * Hdiag[i] * *tau * log(pi[i]) + (1-y[i]) * (Hdiag[i] * *tau) * log(1-pi[i]);
	  // weight second replication of dataset with h_i * tau with opponent y
	  *loglik += (1-y[i]) * Hdiag[i] * *tau * log(pi[i]) + y[i] * Hdiag[i] * *tau * log(1-pi[i]);
	}
	(*evals)++;
	
  
  //Start IRLS: 
	for(;;){
	  Rprintf("iter: %d \n", *iter);
    loglik_old = *loglik;
    copy(beta, beta_old, k);

    XtY(xt, beta_old, newresponse, k, n, 1);
    for(i=0; i < n; i++){
        wi = weight[i] * pi[i] * (1.0 - pi[i]); //W^(-1)
        if(fabs(0.0-wi) < 0.00000000001){
          error("weight");
        }
        newresponse[i] += 1/wi*(weight[i]*((double)y[i]-pi[i]) + 2 * *tau * Hdiag[i] * (0.5 - pi[i]));
    }
    
    // Fisher cov based on augmented dataset: 
    for(i = 0; i < (3*n); i++) {
      if(i < n){
        wi = sqrt(weight[i] * pi[i] * (1.0 - pi[i])); 
      } else {
        wi = sqrt(2 * *tau * Hdiag[(i % n)] * pi[(i % n)] * (1.0 - pi[(i % n)]));
      }
      for(j = 0; j < ncolfit; j++){
        xw2_reduced_augmented[i*ncolfit + j] = xt[(i % n)*ncolfit + selcol[j]] * wi;
        if(i < n){
          //needed for multiplication with newresponse in b_new = fisher_cov * X^TW * newresponse 
          xw2_reduced[i*ncolfit + j] = xt[i*ncolfit + selcol[j]] * wi * wi;
        }
      }
    }
    
    //---- W^(1/2)^TX^T
    trans(xw2_reduced_augmented, xw2t_reduced_augmented, ncolfit, (3*n)); 
    XtXasy(xw2t_reduced_augmented, fisher_cov_reduced_augmented, (3*n), ncolfit);
    linpack_inv(fisher_cov_reduced_augmented, &ncolfit);
    	
    //(X^TWX)^(-1)X^TW
  	XY(fisher_cov_reduced_augmented, xw2_reduced, tmp1_reduced, ncolfit, ncolfit, n);
    	  
  	double tmp;
  	for(j = 0; j < ncolfit; j++) {
  	   tmp = 0.0;
  	   for(i = 0; i < n; i++){
  	       tmp += tmp1_reduced[j + i*ncolfit]*newresponse[i];
  	   }
  	   beta[selcol[j]] = tmp;
  	}
  	
  	for(i = 0; i<k; i++){
  	  Rprintf("beta: %f \n", beta[i]);
  	}
  	
  	//Calculate likelihood: 
  	// calculation of pi
  	XtY(xt, beta, pi, k, n, 1);
  	for(i = 0; i < n; i++){
  	  pi[i] = 1.0 / (1.0 + exp( - pi[i] - offset[i]));
  	} 
  	
  	// XW^(1/2)
  	for(i = 0; i < n; i++) {
  	  wi = sqrt(weight[i] * pi[i] * (1.0 - pi[i])); 
  	  for(j = 0; j < k; j++){
  	    xw2[i*k + j] = x[i + j*n] * wi;
  	  }
  	}
  	
  	//Calculation of Hat diag:
  	trans(xw2, xw2t, k, n); //W^(1/2)^TX^T
  	XtXasy(xw2t, fisher_cov, n, k); //X^TWX
  	linpack_inv(fisher_cov, &k); 
  	
  	XtY(xw2, fisher_cov, tmpNxK, k, n, k);
  	XYdiag(tmpNxK, xw2, Hdiag, n, k);
  	
  	// Calculation of loglikelihood using augmented dataset if firth:
  	*loglik = 0.0;
  	for(i = 0; i < n; i++){ //TODO: better solution? maybe augment w and y but then more memory occupation
  	  *loglik += y[i] * weight[i] * log(pi[i]) + (1-y[i]) * weight[i] * log(1-pi[i]);
  	  // weight first replication of dataset with h_i * tau 
  	  *loglik += y[i] * Hdiag[i] * *tau * log(pi[i]) + (1-y[i]) * (Hdiag[i] * *tau) * log(1-pi[i]);
  	  // weight first replication of dataset with h_i * tau with opponent y
  	  *loglik += (1-y[i]) * Hdiag[i] * *tau * log(pi[i]) + y[i] * Hdiag[i] * *tau * log(1-pi[i]);
  	}
  	(*evals)++;
  	
  	loglik_change = *loglik - loglik_old;
  	for(i=0; i < k; i++){
  		delta[i] = beta[i]-beta_old[i];
  	}
        
  	(*iter)++;
    		
  	if((*iter >= *maxit) || ((maxabsInds(delta, selcol, ncolfit) <= *xconv) && (loglik_change < *lconv)) ) {
  	    bStop = 1;
  	}
    		
  	if(bStop){
  		break;
  	}
  }

	
	// return adjusted vcov matrix if not all variables were fitted:
	for(i = 0; i < k*k; i++) {
	  covs_full[i] = 0.0; // init 0
	}
	for(i=0; i < ncolfit; i++){
	  for(j=0; j < ncolfit; j++) {
	    covs_full[selcol[i] + k*selcol[j]] = fisher_cov_reduced_augmented[i + ncolfit*j];
	  }   
	}
	copy(covs_full, fisher_cov, k*k);
	
	convergence[0] = loglik_change;
	convergence[2] = maxabsInds(delta, selcol, ncolfit);
}


// fit
void logistffit(double *x, int *y, int *n_l, int *k_l, 
								double *weight, double *offset, 
								double *beta, // beta is I/O
								int *colfit, int *ncolfit_l, int *firth_l, 
								int *maxit, double *maxstep, int *maxhs,
								double *lconv, double *gconv, double *xconv, double* tau, 
								// output: 
								double *fisher_cov,		// k x k
								double *Ustar,				// k
								double *pi,						// n
								double *Hdiag,				// n
								double *loglik,				// 1
								int *evals,
								int *iter,
								double *convergence		// 3
)
{
	long n = (long)*n_l, k = (long)*k_l, firth = (long)*firth_l, ncolfit = (long)*ncolfit_l;
	double wi, logdet;
	long i, j, halfs;
	double HdiagSum;
	
	// memory allocations

	double *xt;
	double *beta_old;
	double *xw2;
	double *xw2t;
	double *tmpNxK;
	double *w;
	double *XX_XW2;
	double *XX_XW2t;
	double *XXcovs;
	double *XX_Fisher;
	double *delta;
	double *XBeta;
	int *selcol;
	double *tmp; //to check if pi could get too small

	 if (NULL == (xt = (double *) R_alloc(k * n, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (beta_old = (double *) R_alloc(k ,sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (xw2 = (double *) R_alloc(k * n, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (xw2t = (double *) R_alloc(n * k, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (tmpNxK = (double *) R_alloc(n * k, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (w = (double *) R_alloc(n, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (XX_XW2 = (double *) R_alloc( ncolfit * n, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (XX_XW2t = (double *) R_alloc( n * ncolfit, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (XXcovs = (double *) R_alloc(k * k, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (XX_Fisher = (double *) R_alloc(ncolfit * ncolfit, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (delta = (double *) R_alloc( k, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (XBeta = (double *) R_alloc( n, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (selcol = (int *) R_alloc(ncolfit, sizeof(int))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (tmp = (double *) R_alloc(n, sizeof(double))))
	 {
	   error("no memory available\n");
	 }
	
	//int bIsInverted ;
	int bStop = 0;
	
	//initialise predicted probabilities pi
	trans(x, xt, n, k);
	XtY(xt, beta, pi, k, n, 1);	// X'beta -> temporary pi
	
	for(i = 0; i < n; i++){
	  tmp[i] = exp( - pi[i] - offset[i]);
	  if(tmp[i]>1.0e+34){
	    error("predicted probabilities too small in %d . element, numerator: %e",i+1, tmp[i]);
	  }
		pi[i] = 1.0 / (1.0 + tmp[i]);	// final pi
	}
	
	// init loglik
	double loglik_old, loglik_change = 5.0;
	*loglik = 0.0;
	for(i = 0; i < n; i++)
		*loglik += (y[i] == 1) ? weight[i] * log(pi[i]) : weight[i] * log(1.0 - pi[i]);
	
	for(i=0; i < ncolfit; i++)
		selcol[i] = colfit[i] - 1;
	
	//xw2[i,j]=x[i,j]*w_j with w_j=pi_j(1-pi_j)
	for(i = 0; i < n; i++) { 
		wi = sqrt(weight[i] * pi[i] * (1.0 - pi[i])); // weight
		for(j = 0; j < k; j++)
			xw2[i*k + j] = x[i + j*n] * wi; // multiply whole col with weight
	}
	
	trans(xw2, xw2t, k, n);
	XtXasy(xw2t, fisher_cov, n, k); // calc XWX
	if(firth == 1) {
		linpack_det(fisher_cov, &k, &logdet);
	  if (logdet < (-50)) {	
	    error("Determinant of Fisher information matrix was %lf \n", exp(logdet));
	  }
	  else {
	    linpack_inv(fisher_cov, &k); //if not singular then compute inverse
	    *loglik += *tau * logdet;
	  }
	} 
	else {
	  linpack_inv(fisher_cov, &k);
	}

	
	//Rprintf("*** loop start ***\n");
	// ****** main loop ******
	*evals = 1, *iter = 0;
	for(;;) {
		loglik_old = *loglik;
		copy(beta, beta_old, k);
		
		if(*iter > 0) {
			for(i = 0; i < n; i++) {
				wi = sqrt(weight[i] * pi[i] * (1.0 - pi[i])); // weight
				for(j = 0; j < k; j++)
					xw2[i*k + j] = x[i + j*n] * wi;	// multiply whole col with weight
			}
			//Rprintf("xw2 : "); Rprintf(xw2, k, n);
			
			trans(xw2, xw2t, k, n);
			XtXasy(xw2t, fisher_cov, n, k); // calc XWX
			//Rprintf("fisher : "); Rprintf(fisher_cov, k, k);
			linpack_inv(fisher_cov, &k); // fisher is changed here to covs!!!
			//bIsInverted = 1;
			
			//Rprintf("inv(fisher) : "); Rprintf(fisher_cov, k, k);
		}
		
		if(firth || bStop) {
			// compute diagonal of H
			XtY(xw2, fisher_cov, tmpNxK, k, n, k);
			XYdiag(tmpNxK, xw2, Hdiag, n, k);
			//Rprintf("Hdiag : "); Rprintf(Hdiag, 1, n);
			HdiagSum = 0;
			for(i=0; i<n; i++)
			  HdiagSum += Hdiag[i];
			//Rprintf("Hdiag sum: %f \n", HdiagSum);
		}
		if(firth) 
			for(i=0; i < n; i++){
				w[i] = weight[i] * ((double)y[i]-pi[i]) + 2 * *tau * Hdiag[i] * (1/2 - pi[i]);
		    //Rprintf(" y %5.2f: ", y[i]);
		    //Rprintf(" pi %5.2f: ", pi[i]);
		    //Rprintf(" H %5.2f: ", Hdiag[i]);
			}
		else
			for(i=0; i < n; i++){
				w[i] = weight[i] * ((double)y[i] - pi[i]);
			}
		//Rprintf("weights : "); Rprintf(w, 1, n);
		XtY(x, w, Ustar, n, k, 1);
		//Rprintf("Ustar : "); 
		//Rprintf(Ustar, 1, k);
		
		if(ncolfit > 0 && selcol[0] != -1) {
			//if(ncolfit == k)
			//	copy(fisher_cov, XXcovs, k*k);
			//else {
				for(i = 0; i < n; i++) {
					wi = sqrt(weight[i] * pi[i] * (1.0 - pi[i])); // weight
					if(firth)
						wi = wi * sqrt(1 + Hdiag[i] * 2 * *tau);   // adj covariance for penalty 
					for(j = 0; j < ncolfit; j++)
						XX_XW2[i*ncolfit + j] = x[i + selcol[j]*n] * wi;	// multiply whole col with weight
				}
				//Rprintf("XXXW2 : "); Rprintf(XX_XW2, ncolfit, n);
				
				trans(XX_XW2, XX_XW2t, ncolfit, n);
				XtXasy(XX_XW2t, XX_Fisher , n, ncolfit);
				//for(i=0; i < 2*k*n; i++) Rprintf(" %f ", XX_XW2t[i]);
				linpack_inv(XX_Fisher, &ncolfit); // XX_fisher is changed here to inverse!!
				//for(i=0; i < k*k; i++) Rprintf(" %f ", XX_Fisher[i]);
				//bIsInverted = 1;
				//Rprintf("inv(fisher) : "); Rprintf(XX_Fisher, ncolfit, ncolfit);
				
				for(int i = 0; i < k*k; i++) XXcovs[i] = 0.0; // init 0
				for(i=0; i < ncolfit; i++)
					for(j=0; j < ncolfit; j++) {
						XXcovs[selcol[i] + k*selcol[j]] = XX_Fisher[i + ncolfit*j]; // re-map
					}
				//Rprintf("XXcovs : "); Rprintf(XXcovs, k, k);
				//for(i=0; i < k*k; i++) Rprintf(" %f ", XXcovs[i]);
			//}
			
			if(bStop)
				break;
			//for(i=0; i < k; i++) Rprintf(" %f ", Ustar[i]);
			XtY(XXcovs, Ustar, delta, k, k, 1);
			double mx = maxabs(delta, k) / *maxstep;
			if(mx > 1.0)
				for(i=0; i < k; i++) {
					delta[i] /= mx;
				  //Rprintf("delta %f: ", delta[i]);
				}
		}
		else
		{
			if(bStop)
				break;
			for(i=0; i < k; i++) delta[i] = 0.0; // init 0
		}
		//Rprintf("delta : "); Rprintf(delta, 1, k);
		
		(*evals)++;
		
		if(*maxit > 0) {
			(*iter)++;
			//Rprintf("**** iteration %d\n", *iter);
			for(i=0; i < k; i++){
				beta[i] += delta[i];
			  //Rprintf("delta %f: ", delta[i]);
			  //Rprintf("iter %d", i);
			//Rprintf("beta 1): "); Rprintf(beta, 1, k);
			}
			for(halfs = 1; halfs <= *maxhs; halfs++) {
				//Rprintf("**** iter: %d halfstep %ld\n", *iter, halfs);
				XtY(xt, beta, XBeta, k, n, 1);
				for(i=0; i < n; i++)
				  //Rprintf("offset %f ", offset[i]);
					pi[i] = 1.0 / (1.0 + exp(-XBeta[i] - offset[i]));
					//Rprintf("pi %f ", pi[i]);
				//Rprintf("pi : "); 
				//Rprintf(pi, 1, n);
				
				*loglik = 0.0;
				for(i = 0; i < n; i++)
					*loglik += (y[i] == 1) ? weight[i] * log(pi[i]) : weight[i] * log(1.0 - pi[i]);
				
				if(firth) {
					for(i = 0; i < n; i++) {
						wi = sqrt(weight[i] * pi[i] * (1.0 - pi[i])); // weight
					  //Rprintf("%f ", pi[i]);
						for(j = 0; j < k; j++)
							xw2[i*k + j] = x[i + j*n] * wi;	// multiply whole col with weight
						  //Rprintf("%f ", xw2[i*k + j]);
						  //Rprintf("%f ", wi);
					}
					//Rprintf("xw2 : "); 
				  //Rprintf(xw2, k, n);
					
					trans(xw2, xw2t, k, n);
					XtXasy(xw2t, fisher_cov, n, k); // calc XWX
					//Rprintf("fisher : "); 
					//Rprintf(fisher_cov, k, k);
					linpack_det(fisher_cov, &k, &logdet); // fisher_cov is unchanged here; only det computed
					//bIsInverted = 0;
					
					*loglik += *tau * logdet;
				}
				(*evals)++;
				
				//Rprintf("Iter %d halfs %d \n", *iter, halfs);
				//Rprintf("loglik %f  old: %f \n", *loglik, loglik_old);
				//Rprintf("* beta[0] half stepped: %f \n", beta[0]);
				//Rprintf("* beta[2] half stepped: %f \n", beta[2]);
				//Rprintf("* max Ustar: %f \n", maxabsInds(Ustar, selcol, ncolfit));
				
				if(*loglik >= loglik_old - *lconv)
					break; // stop half steps 
				
				for(i=0; i < k; i++){
//					beta[i] -= delta[i] * powf(2.0, (float)-halfs);  
          delta[i] /= 2.0;
				  beta[i] -= delta[i];
				}
			}
			//Rprintf("****** beta: "); 
			//Rprintf(beta, 1, k);
			//
		}
		
		loglik_change = *loglik - loglik_old;

		/*Rprintf("maxDelta:%f maxUs:%f LLdelta:%f \n",
					 maxabsInds(delta, selcol, ncolfit),
					 maxabsInds(Ustar, selcol, ncolfit),
					 loglik_change);*/
		/*if(*iter >= *maxit)
		  Rprintf("algorithm might not have converged");
		  error("alg not conv");*/
		  
		
		if((*iter >= *maxit) || (
			 (maxabsInds(delta, selcol, ncolfit) <= *xconv) && 
			 (maxabsInds(Ustar, selcol, ncolfit) < *gconv) &&
			 (loglik_change < *lconv)))
			bStop = 1;
		if((*iter < *maxit) && (halfs >= *maxhs) && (*maxhs >= 5) && (*loglik < loglik_old))
		  bStop = 1;   // stop if half-stepping with at least 5 steps was not successful;
	}
	
	copy(XXcovs, fisher_cov, k*k);
	
	convergence[0] = loglik_change;
	convergence[1] = maxabsInds(Ustar, selcol, ncolfit);
	convergence[2] = maxabsInds(delta, selcol, ncolfit);
}

// profile likelihood
void logistplfit(double *x, int *y, int *n_l, int *k_l, 
							double *weight, double *offset, 
							double *beta, // beta is I/O (init)
							int *iSel, int *which, double *LL0, int *firth_l, 
							// control parameter:
							int *maxit, double *maxstep, int *maxhs,
							double *lconv, double *xconv, double *tau,
							// output: 
							double *betahist,			// k * maxit
							double *loglik,				// 1
							int *iter,						// 1 
							double *convergence		// 2
							)
// NB
// princ.comp. not implemented
// ortho not implemented
{
	long n = (long)*n_l, k = (long)*k_l, firth = (long)*firth_l;
	double wi, logdet;
	long i, j, halfs;
	
	// memory allocations
	double *xt;
	double *beta_old;
	double *xw2;
	double *xw2t;
	double *tmpNxK;
	double *w;
	double *tmpKx1;
	double *Vinv;
	double *cov;
	double *tmp1x1;
	double *delta;
	double *XBeta;	
	double *fisher;
	double *Ustar;
	double *pi;
	double *Hdiag;

	 if (NULL == (xt = (double *) R_alloc(k * n, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (beta_old = (double *) R_alloc(k, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (xw2 = (double *) R_alloc(k * n, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (xw2t = (double *) R_alloc(k * n, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (tmpNxK = (double *) R_alloc(k * n, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (w = (double *) R_alloc(n, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (tmpKx1 = (double *) R_alloc(k, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (Vinv = (double *) R_alloc(k * k, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (cov = (double *) R_alloc(k * k,sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (tmp1x1 = (double *) R_alloc(1, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (delta = (double *) R_alloc(k, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (XBeta = (double *) R_alloc(n, sizeof(double))))
	 {
	 error("no memory available\n");
	 }	
	 if (NULL == (fisher = (double *) R_alloc(k * k, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (Ustar = (double *) R_alloc(k, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (pi = (double *) R_alloc(n, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	 if (NULL == (Hdiag = (double *) R_alloc(n, sizeof(double))))
	 {
	 error("no memory available\n");
	 }
	
	// init pi
	trans(x, xt, n, k);
	XtY(xt, beta, pi, k, n, 1);	// X'beta -> temporary pi
	for(i = 0; i < n; i++)
		pi[i] = 1.0 / (1.0 + exp( - pi[i] - offset[i]));	// final pi
	
	// init XW2
	for(i = 0; i < n; i++) {
		wi = sqrt(weight[i] * pi[i] * (1.0 - pi[i])); // weight
		for(j = 0; j < k; j++)
			xw2[i*k + j] = x[i + j*n] * wi; // multiply whole col with weight
	}
	
	trans(xw2, xw2t, k, n);
	XtXasy(xw2t, fisher, n, k); // calc XWX
	
	// init loglik
	*loglik = 0.0;
	for(i = 0; i < n; i++)
		*loglik += (y[i] == 1) ? weight[i] * log(pi[i]) : weight[i] * log(1.0 - pi[i]);
	
	if(firth == 1) {
		linpack_det(fisher, &k, &logdet);
		*loglik += *tau * logdet;
	} 
	
//	Rprintf("*** loop start ***\n");
	// ****** main loop ******
	double loglik_old, lambda, mx;
	//double maxabsdelta;
	*iter = 0;
	for(;;) {
		(*iter)++;
		
		// compute covarince
		copy(fisher, cov, k*k);
		linpack_inv(cov, &k);
		
		if(firth) {
			// compute diagonal of H
			XtY(xw2, cov, tmpNxK, k, n, k);
			XYdiag(tmpNxK, xw2, Hdiag, n, k);
//			Rprintf("Hdiag : "); Rprintf(Hdiag, 1, n);
		}
		//Rprintf("pi : "); Rprintf(pi, 1, n);
		
		if(firth) 
			for(i=0; i < n; i++)
				w[i] = weight[i] * ((double)y[i] - pi[i]) + 2 * *tau * Hdiag[i] * (1/2 - pi[i]);
		else
			for(i=0; i < n; i++)
				w[i] = weight[i] * ((double)y[i] - pi[i]);
//		Rprintf("weight : "); Rprintf(weight, 1, n);
//		Rprintf("w : "); Rprintf(w, 1, n);
		
		XtY(x, w, Ustar, n, k, 1);
//		Rprintf("Ustar : "); Rprintf(Ustar, 1, k);
		
		for(i=0; i < k*k; i++) Vinv[i] = -cov[i];
//		Rprintf("Vinv : "); Rprintf(Vinv, k, k);
		
		XtY(Ustar, Vinv, tmpKx1, k, 1, k); 
//		Rprintf("tmpkx1 : "); Rprintf(tmpKx1, k, 1);
		XtY(tmpKx1, Ustar, tmp1x1, k, 1, 1);
//		Rprintf("val = %f LL0=%f loglik=%f which=%f isel=%f Vinv(i,i)=%f \n", 
//					 tmp1x1[0], *LL0, *loglik, (double)*which, (double)*iSel,
//					 Vinv[k*((*iSel)-1) + (*iSel)-1]);
		double underRoot = 2.0 * ((*LL0 - *loglik) + 0.5 * tmp1x1[0]) /  Vinv[k*((*iSel)-1) + (*iSel)-1]; // Vinv[i,i]
		lambda = (underRoot < 0.0) ? 0.0 : (double)(*which) * sqrt(underRoot); // sqrt(neg) -> set lambda 0
//		Rprintf("lambda = %f \n", lambda);
		
		Ustar[(*iSel)-1] += lambda;
		XtY(cov, Ustar, delta, k, k, 1);
		mx = maxabs(delta, k) / *maxstep;
		if(mx > 1.0)
			for(i=0; i < k; i++)
				delta[i] /= mx;
//		Rprintf("delta : "); Rprintf(delta, 1, k);
		
		for(i=0; i < k; i++)
			beta[i] += delta[i];
		loglik_old = *loglik;
//		Rprintf("beta : "); Rprintf(delta, 1, k);	
		
		
		for(halfs = 0;;) {
//			Rprintf("**** iter: %d halfstep %ld\n", *iter, halfs);
			XtY(xt, beta, XBeta, k, n, 1);
			for(i=0; i < n; i++)
				pi[i] = 1.0 / (1.0 + exp(-XBeta[i] - offset[i]));
			//Rprintf("pi : "); Rprintf(pi, 1, n);
			
			*loglik = 0.0;
			for(i = 0; i < n; i++)
				*loglik += (y[i] == 1) ? weight[i] * log(pi[i]) : weight[i] * log(1.0 - pi[i]);
			
			if(firth) {
				for(i = 0; i < n; i++) {
					wi = sqrt(weight[i] * pi[i] * (1.0 - pi[i])); // weight
					for(j = 0; j < k; j++)
						xw2[i*k + j] = x[i + j*n] * wi;	// multiply whole col with weight
					//Rprintf("%f ", wi);
				}
				//Rprintf("xw2 : "); Rprintf(xw2, k, n);
				
				trans(xw2, xw2t, k, n);
				XtXasy(xw2t, fisher, n, k); // calc XWX
//				Rprintf("fisher : "); Rprintf(fisher, k, k);
				linpack_det(fisher, &k, &logdet); // fisher_cov is unchanged here; only det computed
				
				*loglik += *tau * logdet;
			}
			
			//Rprintf("loglik %f  old: %f \n", *loglik, loglik_old);
			//Rprintf("* beta half stepped): "); Rprintf(beta, 1, k);
			
			if((halfs > *maxhs) || ((fabs(*loglik - *LL0) < fabs(loglik_old - *LL0)) && (*loglik > *LL0)))
				break; // stop half steps 
			
			for(i=0; i < k; i++) {
				delta[i] /= 2.0;		// half the delta
				beta[i] -= delta[i];
			}
			halfs++;
		} // end half steps
		
//		Rprintf("****** beta after half steps: "); Rprintf(beta, 1, k);
		
		for(i=0; i < k; i++)
			betahist[i * (*maxit) + (*iter) - 1] = beta[i];
		
		/*Rprintf("maxDelta:%f maxUs:%f LLdelta:%f \n",
		 maxabsInds(delta, selcol, ncolfit),
		 maxabsInds(Ustar, selcol, ncolfit),
		 loglik_change);*/
		
		if((*iter >= *maxit) || ((fabs(*loglik - *LL0) <= *lconv) && (maxabs(delta, k) < *xconv)))
			break;
	}
	
	convergence[0] = fabs(*loglik - *LL0);
	convergence[1] = maxabs(delta, k);
}





