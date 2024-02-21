##' A function to calculate the power under pre-specified effect size, variance, and
##' correlation using Monte Carlo sampling scheme by calling multi-core parallel computing
##' @title A function to calculate the power under pre-specified effect size,
##' variance, and correlation using Monte Carlo sampling scheme by fitting ANCOVA fixed effects model,
##' ANOVA random effects model, Cox model, and frailty model.
##' @param sim  total number of Monte Carlo replicates in a simulation.
##' @param n an integer number to specify the number of PDX lines.
##' @param m an integer number to specify the number of individuals per PDX line per treatment.
##' @param beta Treatment effect for the treated group.
##' @param tau2 Error variance of random effect.
##' @param alpha Significance level. Default is 0.05.
##' @param lambda Scale parameter of Weibull distribution for the baseline hazard.
##' @param nu Shape parameter of Weibull distribution for the baseline hazard.
##' @param sigma2 Error variance of log survival time for both treatment groups if the outcome follows a normal distribution.
##' @param two.sided Logical value to indicate if a two-sided hypothesis testing is conducted.
##' @param distr distributional assumption of survival time.
##' @param lambdaC Rate parameter of exponential distribution for the hazard of censoring.
##' @param censor logical value of whether a censoring distribution is considered in a data generation setting. Default is TRUE.
##' @param model a string to indicate which model is fitted.
##' @param ncores number of cores for parallel computing. For Windows users, parallel computing only supports \code{model = ANOVA}.
##' @export
##'

simfit <- function(sim = 500, n, m, beta, tau2, alpha = 0.05, lambda = 0.03,
                   nu = 2, sigma2 = 1, two.sided = TRUE, distr = c("Weibull", "normal"),
                   lambdaC = 0.1, censor = TRUE, model = c("both", "ANOVA", "Cox-frailty"),
                   ncores = NULL) {

  if (is.null(ncores)) ncores <- parallel::detectCores()

  if (distr == "Weibull") {

    if (Sys.info()[1] != "Windows") {
      ParaMatrixRaw <- parallel::mclapply(1:sim, bootfit,
                                          n = n, m = m,
                                          beta = beta, tau2 = tau2, alpha = alpha,
                                          lambda = lambda, nu = nu, distr = distr, two.sided = two.sided,
                                          lambdaC = lambdaC, censor = censor, model = model,
                                          mc.cores = ncores)
    } else if (model == "ANOVA") {
      cl <- parallel::makeCluster(ncores)
      ParaMatrixRaw <- parallel::parLapply(cl, 1:sim, bootfit,
                                           n = n, m = m,
                                           beta = beta, tau2 = tau2, alpha = alpha,
                                           lambda = lambda, nu = nu, distr = distr, two.sided = two.sided,
                                           lambdaC = lambdaC, censor = censor, model = model)
      parallel::stopCluster(cl)
    } else {
      ParaMatrixRaw <- parallel::mclapply(1:sim, bootfit,
                                          n = n, m = m,
                                          beta = beta, tau2 = tau2, alpha = alpha,
                                          lambda = lambda, nu = nu, distr = distr, two.sided = two.sided,
                                          lambdaC = lambdaC, censor = censor, model = model,
                                          mc.cores = 1)
    }



  } else if (distr == "normal") {

    if (Sys.info()[1] != "Windows") {
      ParaMatrixRaw <- parallel::mclapply(1:sim, bootfit,
                                          n = n, m = m,
                                          beta = beta, tau2 = tau2, alpha = alpha,
                                          sigma2 = sigma2, distr = distr, two.sided = two.sided,
                                          lambdaC = lambdaC, censor = censor, model = model,
                                          mc.cores = ncores)
    } else if (model == "ANOVA"){
      cl <- parallel::makeCluster(ncores)
      ParaMatrixRaw <- parallel::parLapply(cl, 1:sim, bootfit,
                                           n = n, m = m,
                                           beta = beta, tau2 = tau2, alpha = alpha,
                                           sigma2 = sigma2, distr = distr, two.sided = two.sided,
                                           lambdaC = lambdaC, censor = censor, model = model)
      parallel::stopCluster(cl)
    } else {
      ParaMatrixRaw <- parallel::mclapply(1:sim, bootfit,
                                          n = n, m = m,
                                          beta = beta, tau2 = tau2, alpha = alpha,
                                          sigma2 = sigma2, distr = distr, two.sided = two.sided,
                                          lambdaC = lambdaC, censor = censor, model = model,
                                          mc.cores = 1)
    }

  } else {
    return(0)
  }


  return(ParaMatrixRaw)



}
