#### ---- this script serves the purpose of building the NR algorithm from scratch to solve for the parameters in the -
### logistic regression model

#### ---- NR has not been done for the proportional odds model due to time constraints. BFGS directly from optim was used for this.

#### ---- to implement BFGS we still require the objective function (likelihood), first and second derivative functions
#### ---- these have been implemented in this script, and will be used in the estimation.R script to perform estimation

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(magrittr)
library(glue)
library(glmnet)
library(numDeriv)
library(MASS)


# Log-likelihood Functions -------------------------------------------------

  ## -- ll for logistic regression (lr)
log_lik_logistic <- function(beta, x, y){
  
  n = nrow(x) # number of observations
  p = ncol(x) # number of covariates
  eta = x %*% beta  # linear predictor
  loglik = 0
  
  for(i in 1:n){
    y_i = as.numeric(y[i,1])
    eta_i = eta[i]
    p_i = plogis(eta_i)
    # avoid log(0) by adding small constant
    loglik = loglik + y_i * log(p_i + 1e-10) + (1 - y_i) * log(1 - p_i + 1e-10)
  }
  return(loglik)
}

  ## -- ll for proportional odds lr
log_lik_prop_odds <- function(params, x, y, n_covs) {
  
  beta = params[1:n_covs]
  theta = params[n_covs+1:length(params)]
  
  n = nrow(x)        # number of observations
  p = ncol(x)        # number of predictors
  J = length(unique(y))         # number of outcome categories (assumes y is a vector of 1:J)
  eta = x %*% beta   # linear predictor: eta_i = x_i^T * beta
  
  # Step 1: Compute cumulative probabilities gamma[,j] = P(Y <= j)
  gamma = matrix(0, nrow = n, ncol = J)
  for (j in 1:(J - 1)) {
    gamma[, j] = plogis(theta[j] - eta)
  }
  gamma[, J] = 1  # By definition, P(Y <= J) = 1
  
  # Step 2: Compute category probabilities: P(Y = j) = gamma_j - gamma_{j-1}
  probs = matrix(0, nrow = n, ncol = J)
  for (j in 1:J) {
    lower = if (j == 1) rep(0, n) else gamma[, j - 1]
    probs[, j] = gamma[, j] - lower
  }
  
  # Step 3: Avoid log(0) by clipping probabilities
  eps = 1e-12
  probs = pmax(probs, eps)
  
  # Step 4: Compute log-likelihood: sum over log P(y_i | x_i)
  # This pulls the probability for each observation’s actual category
  ll = sum(log(probs[cbind(1:n, y + 1)]))
  
  # Return log-likelihood and intermediate structures
  return(ll)
}



# Score Function (gradient) -----------------------------------------------


  ## -- score for lr
score_logit <- function(beta, x, y){
  
  xb = x %*% beta
  pi = exp(xb) / (1 + exp(xb))
  y = as.matrix(y, nrow = nrow(y))
  
  score = matrix(data = unname(t(x) %*% (y - pi)), nrow = length(beta), ncol = 1)
  
  return(score)
}

  ## -- score for proportional odds lr
score_prop_odds <- function(params, x, y, n_covs) {
  
  beta = params[1:n_covs]
  theta = params[(n_covs+1):length(params)]
  
  n = nrow(x)               # Number of observations
  p = length(beta)          # Number of predictors
  J = length(theta) + 1     # Number of outcome categories
  
  eta = x %*% beta          # Linear predictor (n x 1)
  
  grad_beta = rep(0, p)     # Gradient vector for beta
  grad_theta = rep(0, J - 1) # Gradient vector for theta
  
  eps = 1e-12               # Small number for numerical stability
  
  for (i in 1:n) {
    x_i = x[i, ]
    eta_i = eta[i]
    for (j in 1:J) {
      # Threshold transformations
      t_j    = if (j <= J - 1) theta[j] - eta_i else Inf
      t_jm1  = if (j == 1) -Inf else theta[j - 1] - eta_i
      # CDF and PDF values
      F_j    = plogis(t_j)
      F_jm1  = plogis(t_jm1)
      f_j    = dlogis(t_j)
      f_jm1  = if (j == 1) 0 else dlogis(t_jm1)
      # Probability of being in category j
      pi_ij  = F_j - F_jm1
      pi_ij  = max(pi_ij, eps)  # Avoid divide-by-zero or log(0)
      indicator = as.numeric(y[i] == j - 1)  # 1 if cat is j
      # Gradient wrt beta
      grad_beta = grad_beta + indicator * ((f_jm1 - f_j) / pi_ij) * x_i
      # Gradient wrt theta
      if (j <= J - 1) {
        grad_theta[j] = grad_theta[j] + indicator * (f_j / pi_ij)
      }
      if (j > 1) {
        grad_theta[j - 1] = grad_theta[j - 1] - indicator * (f_jm1 / pi_ij)
      }
    }
  }
  
  return(c(grad_beta, grad_theta))
}


# Hessian matrices (2nd derivative) ---------------------------------------

  ## -- note that not hessian has been derived for the proportional odds lr because it is not required to implement BFGS

  ## -- hessian for lr
hessian_logit <- function(beta_t, x, y){
  
  xb = x %*% beta_t
  pi = exp(xb) / (1 + exp(xb))
  
  W = diag(c(1-pi)*c(pi), nrow = nrow(x))
  
  hessian = matrix(data = t(x) %*% W %*% x, nrow = length(beta_t), ncol = length(beta_t))
  
  return(hessian)
}



# Helper functions --------------------------------------------------------

  ## -- the h(t) function in NR is simply = inverse(hessian) * score. This is done in the function below for logistic regression.

h_t_lr <- function(beta_t, x, y){
  
  n_params = length(beta_t)
  score = as.matrix(unlist(score_logit(beta = beta_t, x, y)), nrow = n_params)
  
  hessian = hessian_logit(beta_t, x, y)
  
  return(solve(hessian) %*% 
           score)
}



# Newton Raphson (LR) -----------------------------------------------------

  ## -- the function below can be used to optimize the parameters in a logistic regression model

optim_nr_logit <- function(beta_init, x, y, tol = 1e-3, iter = 1e3){
  
  start_time = Sys.time()
  
  log_lik_vector = rep(NA, iter)
  
  for(i in 1:iter){
    #print(glue("Iteration: {i}"))
    if(i == 1){
      
      beta_t = as.vector(beta_init + h_t_lr(beta_init, x, y))
      
      log_lik_vector[i] = log_lik_logistic(beta_t, x, y)
      
    }else{
      
      ll_prev = log_lik_vector[i-1]
      
      beta_t = beta_t + h_t_lr(beta_t, x, y)
      
      ll_t = log_lik_logistic(beta_t, x, y)
      
      log_lik_vector[i] = ll_t
      
      if(abs(ll_prev - ll_t) < tol){
        end_time = Sys.time()
        print(glue("Algorithm converged in {i} iterations."))
        #names(beta_t) = colnames(x); names(beta_t)[1] = "Intercept"
        return(list(estimates = beta_t, 
                    ll = log_lik_vector[!is.na(log_lik_vector)], 
                    time = end_time - start_time))
      }
    }
  }
  print("Algorithm did not converge")
  return(list(estimates = NA, ll = log_lik_vector))
}
