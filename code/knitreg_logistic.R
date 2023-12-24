# a wrapper for knitreg() to produce odds ratios table for logistic regression models

knitreg_logistic <- function(l, ...) {
  
  # coerce to a list if l is just a single model
  if (!"list" %in% class(l)[1]) {
    l <- list(l)
  }
  
  # check that all models are logistic regression models
  # first check that they are of class glm
  glm_test <- 0
  for (i in 1:length(l) ) {
    glm_test <-  glm_test + ("glm" %in% class(l[[i]]) )
  }
  if (glm_test != length(l) ) {stop("All models must be logistic regression models") }
  # now check they are the correct family of glm
  logit_test <- 0
  for (i in 1: length(l) ) {
    logit_test <- logit_test + 
      (l[[i]]$family$family == "binomial") + 
      (l[[i]]$family$link == "logit")
  }
  if (logit_test != 2*length(l) ) {stop("All models must be logistic regression models") }
  
  # odds ratios
  oc <- list()
  for (i in 1:length(l) ) {oc[[i]] <- exp(coef(l[[i]] ) ) }
  
  # lower and upper confidence limits for odds ratios
  ocl <- list()
  for (i in 1:length(l) ) {ocl[[i]] <- exp(confint(l[[i]] )[ , "2.5 %"] ) }
  ocu <- list()
  for (i in 1:length(l) ) {ocu[[i]] <- exp(confint(l[[i]] )[ , "97.5 %"] ) }
  
  # pass to knitreg()
  knitreg(l, 
          override.coef = oc, 
          override.ci.low = ocl, 
          override.ci.up = ocu, 
          ci.test = 1,
          ...)
  
}
