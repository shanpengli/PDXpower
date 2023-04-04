##' @export
##'

simfit <- function(sim = 500, n, m, beta, tau2, alpha = 0.05, lambda = 0.03,
                   nu = 2, two.sided = TRUE, sigma2 = 1, distr = c("Weibull", "normal"), lambdaC = 0.1, censor = TRUE, ncores = 10) {

  if (distr == "Weibull") {
    ParaMatrixRaw <- parallel::mclapply(1:sim, bootfit,
                                        n = n, m = m,
                                        beta = beta, tau2 = tau2, alpha = alpha,
                                        lambda = lambda, nu = nu, distr = distr, two.sided = two.sided,
                                        lambdaC = lambdaC, censor = censor,
                                        mc.cores = ncores)
  } else if (distr == "normal") {
    ParaMatrixRaw <- parallel::mclapply(1:sim, bootfit,
                                        n = n, m = m,
                                        beta = beta, tau2 = tau2, alpha = alpha,
                                        sigma2 = sigma2, distr = distr, two.sided = two.sided,
                                        lambdaC = lambdaC, censor = censor,
                                        mc.cores = ncores)
  } else {
    return(0)
  }


  return(ParaMatrixRaw)



}
