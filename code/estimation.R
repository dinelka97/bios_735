# Introduction ------------------------------------------------------------

  ## -- this script serves the purpose of defining the functions that are to be included within our package so that a user can -
    ## -- use it to run the proportional odds logistic regression model.
  ## -- a separate script is used to run the same for logistic regression.

source("code/optim_v2.R")

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(magrittr)
library(glue)
library(ggplot2)
library(MASS)

### -- example of running a proportional odds model

  ## -- the user can provide a data frame (subset as required) and obtain estimates.
  ## -- user has to specify which variable is the response variable (everything else will be covariates).
  ## -- this function assumes that we pre-specify theta thresholds beforehand. We use empirical evidence to -
    ## -- determine the thresholds. Use CDF of logistic function for this. Done within the function.

    ### -- FUNCTION ARGUMENTS
  ## -- formula: refers to the usual formula that a user provides in any regression modeling. 
  ## -- df: dataframe that the user wants the method/function applied to.
  ## -- method: specify if user wants to use logistic regression (method == "lr") or proportional odds logistic regression (method == "polr")


    ### -- OUTPUT
  ## -- beta: this is computed for both lr and polr. For polr, we don't obtain an intercept as this is captured in theta.
          # - beta refers to the fixed effects of the covariates
  ## -- theta: this is computed only for polr. 
          # - the theta values can be used to compare probabilities between different groups.


# Estimating lr and polr --------------------------------------------------

glm_v2 <- function(formula, df, method){

  ## extract response variable name
  response = as.character(formula)[2]

  ## extract the response variable
  y = df %>% dplyr::select(all_of(response))
  
  if(method == "lr" & nrow(unique(y)) != 2){
    stop("Make sure that the response variable has 2 categories.")
  }
  
  ## convert this response to a matrix
  y_mat = as.matrix(y)
  y_mat_num = apply(y_mat, 2, as.numeric)
  
  ## extract covariate names
  covariates = as.character(formula)[3]
  covariates = strsplit(covariates, "\\s*\\+\\s*")[[1]]
  
  ## subset df to only the response and the covariates
  df %<>% dplyr::select(all_of(c(response, covariates)))
  
  ## if any factor variables are present, implement one-hot encoding
  
  factor_df = Filter(is.factor, df %>% dplyr::select(-all_of(response)))
  names_factor = names(Filter(is.factor, df %>% dplyr::select(-all_of(response))))
  
  if(length(names_factor) > 0){
    one_hot_df = model.matrix(~ ., data = factor_df)[,-1]
    
    numeric_df = Filter(Negate(is.factor), df)
    df = cbind(numeric_df, one_hot_df, y_mat_num)
  }

  ## convert the covariates to a matrix
  x_mat = as.matrix(df %>% dplyr::select(-all_of(response)), nrow = nrow(df))
  n_covariates = ncol(x_mat)
  names_covariates = colnames(x_mat)
  
  if(method == "polr"){
    ## extract the theta names (based on the categories in the data)
    vec = sort(pull(unique(y)))
    theta_names = paste0("theta", vec[-length(vec)], vec[-1])
    

    ## obtain theta thresholds (from data) - these are what we would be using as the initial parameter estimates to the model
    ## -- however, the user has autonomy to pick their own estimates. If none given, then this is what will be used as default.
    cum_probs = cumsum(prop.table(table(y_mat)))[1:4]
    theta_init = qlogis(cum_probs)  
    
    ## the default initial parameter estimates for the beta (fixed effect) coefficients are all zero (no intercept in prop odds)
    ## -- however, again as before, the user has autonomy to pick their own initialization. 
    
    beta_init = rep(0, n_covariates) 
    
    ## -- using the above, we optimize our likelihood using the BFGS algorithm
    
    ## -- first get our initial estimates
    
    params_init = c(beta_init, theta_init)
    names(params_init) = c(names_covariates, theta_names)
    
    ## -- now for the algorithm, we directly use BFGS
    
    fit = optimx::optimx(
      par = params_init,
      fn = log_lik_prop_odds,
      gr = score_prop_odds,
      method = "BFGS",
      control = 
        list(
          trace = 0,
          maximize = T,
          abstol = T
        ),
      x = x_mat, 
      y = y_mat_num,
      n_covs = n_covariates
    )
    
    rownames(fit) = NULL
    
    
    return(list(beta = unlist(fit[names_covariates]), 
                theta = unlist(fit[theta_names]),
                ll = unlist(fit["value"]),
                time = unlist(fit["xtime"])))
    
  }else if(method == "lr"){
    
    ## -- because logistic regression requires an intercept we need to add in an intercept column
    
    x_mat = cbind(1, x_mat)
  
    ## the default initial parameter estimates for the beta (fixed effect) coefficients are all zero (including the intercept)
    ## -- however, again as before, the user has autonomy to pick their own initialization. 
    
    beta_init = rep(0, n_covariates + 1) 
    names(beta_init) = c("Intercept", names_covariates)
    
    ## -- using the above, we optimize our likelihood using the NR algorithm that has been handcoded and laid out in optim.R
    
    fit = optim_nr_logit(beta_init = beta_init, x = x_mat, y = y_mat_num)
    
    estimates = as.vector(fit$estimates)
    names(estimates) = names(beta_init)
    
    ll = fit$ll[length(fit$ll)]
    time = fit$time
    
    return(list(beta = estimates, 
                ll = ll,
                time = time)
    )
  }
}



# Testing on heart dataset ------------------------------------------------


    ## -- load dataset & create 2 separate response variables (1 for binary & 1 as is)

df_heart <- read_csv("data/derived/df_v1.csv")

    ## -- remove missing values
df_heart <- na.omit(df_heart)

    ## -- creating two response variables
df_heart %<>%
  dplyr::select(-...1) %>%
  mutate(y_mult = factor(num),
         y_bin = factor(ifelse(num == "0", 0, 1)),
         across(c(sex, fbs, slope), as.factor)
  )


    ### -- testing it for proportional odds logistic regression
formula = y_mult ~ age + fbs + sex + cp + chol + exang + thalach + slope

      ## -- using glm_v2
res_glm_v2 = glm_v2(formula, df = df_heart, method = "polr")

      ## -- using polr function from MASS
res_polr = polr(formula, data = df_heart)

      ## -- are the coefficients the same?
        ## -- sort results (just name a and b for the 2 sets of results)
a = res_glm_v2$beta[sort(names(res_glm_v2$beta))]
b = res_polr$coefficients[sort(names(res_polr$coefficients))]

all(a - b < 1e-4) 

      ## -- is the final log-likelihood the same?
unname(res_glm_v2$ll - logLik(res_polr) < 1e-4)


    ### -- testing it for logistic regression
formula = y_bin ~ age + fbs + sex + cp + chol + exang + thalach + slope

      ## -- using glm_v2
res_glm_v2 = glm_v2(formula, df = df_heart, method = "lr")

      ## -- using polr function from MASS
res_glm = glm(formula, data = df_heart, family = "binomial")

      ## -- are the coefficients the same?
a = res_glm_v2$beta[c("Intercept", sort(setdiff(names(res_glm_v2$beta), "Intercept")))]
b = res_glm$coefficients[sort(names(res_glm$coefficients))]

all(a - b < 1e-4)

      ## -- is the final log-likelihood the same?
unname(res_glm_v2$ll - logLik(res_glm) < 1e-4)






