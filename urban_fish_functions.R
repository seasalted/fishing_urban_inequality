#===============================================================================
# Functions to connect and analyze MRIP metropolitan site landings with socioeconomic datasets
#  
# Name: 
# Created: J. Zachary (Zach) Koehn
# Modified: 
# Email: zkoehn@uw.edu
# For: SESYNC Graduate Pursuit
# Date started: 12/17/2018
# Revised: 05/16/2019
#===============================================================================



# create function that passes multinomial logit model calls with polr

polr_model_objs_function <- function(model_call,data) {
  model_object <- polr(model_call,data=data, Hess = TRUE)
  return(model_object)
}


# create function that passes multinomial logit model calls with {nnet} multinommultinom_model_objs_function <- function(model_call,data) {
multinom_model_objs_function <- function(model_call,data)   {
require(nnet)
  model_object <- multinom(model_call,data=data, Hess = TRUE)
  return(model_object)
}


# extracts multinom model object information to calculate pvalues
multinom_return_pvalue_function <- function(model_object) {
  z <- summary(model_object)$coefficients/summary(model_object)$standard.errors
  # 2-tailed Wald z tests to test significance of coefficients
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  return(p)
}


# extracts multinom model object coefficents
multinom_return_coefs_function <- function(model_object) {
  coefs <- summary(model_object)$coefficients
  return(coefs)
}

# extracts multinom model object coefficents
multinom_return_odds_function <- function(model_object) {
  oddsratio <- exp( coef(model_object) )
  return(oddsratio)
}



