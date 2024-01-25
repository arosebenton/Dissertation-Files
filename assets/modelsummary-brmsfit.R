# Functions for Bayesian Model Presentation Using {modelsummary}----
glance.brmsfit <- function(x, 
                           bayes_R2 = NULL, 
                           looR2 = NULL,
                           looic = NULL, 
                           marglik = NULL,
                           kfold = NULL,
                           ...) {
  
  ## Retrive the names of the already stored criteria
  criteria <- names(x$criteria)
  
  ## Retreive Bayes R2
  if(isTRUE(bayes_R2)) {
    R2 <- .extract_R2(x, criteria)
  } else {
    R2 <- data.frame(R2 = NA_real_)
  }
  
  ## Retreive LOO-IC
  if(isTRUE(looic)) {
    loo_ic <- .extract_looic(x, criteria)
  } else {
    loo_ic <- data.frame(loo_ic = NA_real_)
  }
  
  ## Retreive LOO Adjusted R2
  if(isTRUE(looR2)) {
    looR2_df <- .extract_loo_R2(x, criteria)
  } else {
    looR2_df <- data.frame(loo_R2.mean = NA_real_)
  }
  
  ## Retreive K-Fold IC
  if(isTRUE(kfold)) {
    kfold_elpd <- .extract_kfold(x, criteria)
  } else {
    kfold_elpd <- data.frame(kfold_elpd = NA_real_)
  }
  
  ## Marginal Likelihood
  if(isTRUE(marglik)) {
    marglik <- .extract_marglik(x, criteria)
  } else {
    marglik <- data.frame(marglik = NA_real_)
  }
  
  ## Get the number of groups for multilevel models
  if(!is.null(brms::ngrps(x))) {
    ngroups <- brms::ngrps(x)
  } else {
    ngroups <- data.frame(ngroups = NA_real_)
  }
  
  ## Number of observations
  obs <- data.frame(n = nobs(x))
  
  ## Bind everything into a single data frame
  out <- do.call(cbind, list(R2, loo_ic, kfold_elpd, looR2_df, marglik, ngroups, obs))
  out <- out[, !is.na(out)]
  return(out)
}

## LOO-IC Extraction Helper----
.extract_looic <- function(x, criteria) {
  
  criteria_pos <- match("loo", criteria)
  
  loo_df <- data.frame(
    elpd.loo = x$criteria[[criteria_pos]]$estimates[1,1],
    elpd.loo.se = x$criteria[[criteria_pos]]$estimates[1,2],
    loo.ic = x$criteria[[criteria_pos]]$estimates[3,1],
    loo.ic.se = x$criteria[[criteria_pos]]$estimates[3,2]
  )
  
  return(loo_df)
}

## K-Fold CV Extraction Helper----
.extract_kfold <- function(x, criteria) {
  
  criteria_pos <- match("kfold", criteria)
  
  kfold_df <- data.frame(
    elpd.kfold = x$criteria[[criteria_pos]]$estimates[1,1],
    elpd.kfold.se = x$criteria[[criteria_pos]]$estimates[1,2],
    kfold.ic = x$criteria[[criteria_pos]]$estimates[3,1],
    kfold.ic.se = x$criteria[[criteria_pos]]$estimates[3,2]
  )
  
  return(kfold_df)
}

## LOO Adusted R2 Extraction Helper----
.extract_loo_R2 <- function(x, criteria) {
  
  criteria_pos <- match("loo_R2", criteria)
  
  loo_R2_df <- data.frame(loo_R2.mean = mean(x$criteria[[criteria_pos]]))
  
  return(loo_R2_df)
}


## Bayes R2 Extraction Helper----
.extract_R2 <- function(x, criteria) {
  
  criteria_pos <- match("bayes_R2", criteria)
  
  if (!is.null(x$criteria[[criteria_pos]][["conditional_R2"]])) {
    conditional_R2 <- as.data.frame(x$criteria[[criteria_pos]][["conditional_R2"]])[c(1,3:4)]
    colnames(conditional_R2) <- c("cond.R2", "cond.R2.lower", "cond.R2.upper")
  } 
  
  else {
    conditional_R2 <-  NA_real_
  }
  
  if (!is.null(x$criteria[[criteria_pos]][["marginal_R2"]])) {
    marginal_R2 <- as.data.frame(x$criteria[[criteria_pos]][["marginal_R2"]])[c(1,3:4)]
    colnames(marginal_R2) <- c("marg.R2", "marg.R2.lower", "marg.R2.upper")
  }
  
  else {
    marginal_R2 <-  NA_real_
  }
  
  R2 <- do.call(cbind, list(conditional_R2, marginal_R2))
  
  return(R2)
}

## Marginal Likelihood Extraction Helper----
.extract_marglik <- function(x, criteria) {
  
  criteria_pos <- match("marglik", criteria)
  
  marglik <- data.frame(
    marglik.med = do.call("median", list(x$criteria[[criteria_pos]]$logml)),
    marglik.iqr = do.call("IQR", list(x$criteria[[criteria_pos]]$logml))
  )
  
  return(marglik)
}

#' Custom Tidy Method for Making Tables for Bayesian Models usung modelsummary
#' 
#' This function provides a custom tidy method for objects of class brmsfit
#' that, among other things, correctly handles models multinomial logistic
#' regression models.
#'
# Arguments
##
##  x           An object of class brmsfit
##
##  conf_level  1 - alpha level to use for the uncertainty estimates
##
##  parameters
##
##  signif      Logical indicating whether to include a column for p-values 
##              based on posterior tail quantities.
##
##  ...         Additional arguments passed to bayestestR::describe_posterior

# Returns
##
##  A data frame containing the relevant model information

tidy.brmsfit <- function(x, 
                         conf_level = 0.95, 
                         parameters = "^b_|^sd_|^cor_", 
                         signif = NULL,
                         ...) {
  
  ## Extract the model information
  posterior <- bayestestR::describe_posterior(
    x,
    ci = conf_level,
    parameters = parameters,
    effects = "all",
    ...
  )
  
  ## Build the data frame for modelsummary 
  out <- data.frame(
    term = posterior$Parameter,
    estimate = posterior$Median,
    conf.level = posterior$CI,
    conf.low = posterior$CI_low,
    conf.high = posterior$CI_high,
    pd = posterior$pd,
    rope.percentage = posterior$ROPE_Percentage,
    rhat = posterior$Rhat,
    ess = posterior$ESS
  )
  
  # Combatability with multilevel and fixed effects only models
  if(!is.null(brms::ngrps(x))) {
    out$effect <- posterior$Effects
  }
  else {
    out$effect <- "fixed"
  }
  
  # If model family is of class categorical needs to be handled differently
  if(isTRUE(grepl(x$family[1], "categorical"))) {
    ## Decompose the term column into a response category identifier
    ## and a term name identifier
    out$response <- as.character(stringr::str_extract_all(out$term, "mu.*?(?=_)"))
    
    ## Ovewrwrite the original term column
    out$term <- stringr::str_remove_all(out$term, "_mu.*?(?=_)")
    
  }
  
  # Option to return stars if reviewer 2 is a dick and demands you report 
  # "statistical significance" for some absurd reason
  if(isTRUE(signif)) {
    ## Use rope.percentage for this, though could also use bayestestR::pd_to_p
    out$p.value <- ifelse(out$effect == "fixed", out$rope.percentage, 1)
  }
  
  # Return the data frame object
  return(out)
}
