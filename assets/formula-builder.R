# A Function for building combinations of formulas----
formula_builder <- function(lhs, rhs, ...) {
  
  ## Validate the formula arguments
  stopifnot("lhs and rhs should be character vectors" = (is.character(lhs) & is.character(rhs)))
  
  ## Make a list of all unique combinations of lhs ~ rhs 
  formulas <- expand.grid(lhs, rhs, stringsAsFactors = FALSE)
  
  ## Initialize a list to store the formulas in
  out <- list()
  
  ## Build a list of formulas for each pair formulas
  for (i in 1:nrow(formulas)) {
    out[[i]] <- glue::glue_collapse(formulas[i, ]) 
  }
  
  ## Return the output
  return(out)
}

# Arguments
##
##  lhs   A character vector specifying the names of 
##        response variables to use on the left hand 
##        side of the formulas
##
##  rhs   A character vector specifying the formulas 
##        to use on the right hand  side of the model 
##        equation

# Returns
##
##  A list of model formulas containing all unique
##  combinations of lhs ~ rhs

# Usage Example
##
##  # Specify the inputs for the lhs argument
##  responses <- c("sex", "drugs", "rock")
##
##  # Specify the inputs for the rhs argument
##  covariates <- c(
##    " ~ age + education + party_id",
##    " ~ age + education",
##    )
##
##  # Build the formulas
##  model_forms <- formula_builder(lhs = responses, rhs = covariates)