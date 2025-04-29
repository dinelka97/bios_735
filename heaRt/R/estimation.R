# Introduction ------------------------------------------------------------

  ## -- this script serves the purpose of defining the functions that are to be included within our package so that a user can -
    ## -- use it to run both, a proportional odds logistic regression model and a logistc regression model.

# Load libraries ----------------------------------------------------------

### -- example of running a proportional odds model

## -- the user can provide a data frame (subset as required) and obtain estimates.
## -- user has to specify which variable is the response variable (everything else will be covariates).
## -- this function assumes that we pre-specify theta thresholds beforehand. We use empirical evidence to -
## -- determine the thresholds. Use CDF of logistic function for this. Done within the function.


#' Fits regular and multi-category logistic regression
#'
#' This glm_v2 function fits a logistic regression model for binary outcomes or a proportional odds cumulative logit model for multi-category outcomes.
#'
#' @param formula The model formula provided in regression modeling form (e.g. Y ~ X1 + X2).
#' @param df The dataframe the method will be applied to.
#' @param method The method to be used. This is either logistic regression (method = "lr") or proportional odds logistic regression (method = "polr").
#'
#' @return a list with the following elements:
#' \itemize{
#' \item{beta: fixed effects of the covariates (computed for both lr and polr). For polr, we don't obtain an intercept as this is captured in theta.}
#' \item{theta: theta values compare probabilities between different groups (this is computed only for polr).}
#' \item{ll: the log-likelihood value (computed for both lr and polr).}
#' \item{time: the time it takes to fit (computed for both lr and polr).}
#' }
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom readr read_csv
#' @importFrom optimx optimx
#' @importFrom MASS polr
#' @importFrom glue glue
#' @import magrittr
#'
#' @examples
#' df_heart <- read_csv("data/derived/df_v1.csv")
#' df_heart <- na.omit(df_heart)
#'
#' # create binary and multi-category outcomes
#' df_heart %<>% dplyr::select(-...1) %>% mutate(y_mult = factor(num),
#'                                   y_bin = factor(ifelse(num == "0", 0, 1)),
#'                                   across(c(sex, fbs, slope), as.factor))
#'
#' #' # fit logistic regression model
#' formula_bin = y_bin ~ age + fbs + sex + cp + chol + exang + thalach + slope
#' res_glm_v2 = glm_v2(formula_bin, df = df_heart, method = "lr")
#'
#' # fit proportional odds cumulative logit model
#' formula = y_mult ~ age + fbs + sex + cp + chol + exang + thalach + slope
#' res_polr = glm_v2(formula, df = df_heart, method = "polr")
#'
#' @export
glm_v2 <- function(formula, df, method){

  # check for a valid method
  valid_methods <- c("lr", "polr")
  if (!method %in% valid_methods) {
    stop(paste("Invalid method. Method must be one of:", paste(valid_methods,
                                                               collapse=", ")))
  }

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

  # check for missing variables
  missing_vars <- setdiff(covariates, names(df))
  if (length(missing_vars) > 0) {
    stop(paste("Missing variables in dataframe:", paste(missing_vars,
                                                        collapse=", ")))
  }

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
    # vec = sort(pull(unique(y)))
    # vec = sort(as.vector(unique(y)[,1]))
    if (is.factor(y[[1]])) {
      # if the response is a factor, get its levels directly
      vec = levels(y[[1]])
    } else {
      # otherwise, extract the unique values safely
      vec = sort(unique(as.vector(unlist(y))))
    }
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
