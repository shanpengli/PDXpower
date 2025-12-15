simfit <- function(sim = 500, n, m, beta0 = 5, beta, tau2, alpha = 0.05, lambda = 0.03,
                   nu = 2, sigma2 = 1, two.sided = TRUE, distr = c("Weibull", "normal"),
                   Ct = 5, censor = TRUE, model = c("both", "ANOVA", "Cox-frailty"),
                   ncores = NULL) {

  if (is.null(ncores)) ncores <- parallel::detectCores()

  if (distr == "Weibull") {

    if (Sys.info()[1] != "Windows") {
      ParaMatrixRaw <- parallel::mclapply(1:sim, bootfit,
                                          n = n, m = m, beta0 = beta0,
                                          beta = beta, tau2 = tau2, alpha = alpha,
                                          lambda = lambda, nu = nu, distr = distr, two.sided = two.sided,
                                          Ct = Ct, censor = censor, model = model,

                                          mc.cores = ncores)
    } else if (model == "ANOVA") {
      cl <- parallel::makeCluster(ncores)
      ParaMatrixRaw <- parallel::parLapply(cl, 1:sim, bootfit,
                                           n = n, m = m, beta0 = beta0,
                                           beta = beta, tau2 = tau2, alpha = alpha,
                                           lambda = lambda, nu = nu, distr = distr, two.sided = two.sided,
                                           Ct = Ct, censor = censor, model = model)
      parallel::stopCluster(cl)
    } else {
      ParaMatrixRaw <- parallel::mclapply(1:sim, bootfit,
                                          n = n, m = m, beta0 = beta0,
                                          beta = beta, tau2 = tau2, alpha = alpha,
                                          lambda = lambda, nu = nu, distr = distr, two.sided = two.sided,
                                          Ct = Ct, censor = censor, model = model,

                                          mc.cores = 1)
    }



  } else if (distr == "normal") {

    if (Sys.info()[1] != "Windows") {
      ParaMatrixRaw <- parallel::mclapply(1:sim, bootfit,
                                          n = n, m = m, beta0 = beta0,
                                          beta = beta, tau2 = tau2, alpha = alpha,
                                          sigma2 = sigma2, distr = distr, two.sided = two.sided,
                                          Ct = Ct, censor = censor, model = model,

                                          mc.cores = ncores)
    } else if (model == "ANOVA"){
      cl <- parallel::makeCluster(ncores)
      ParaMatrixRaw <- parallel::parLapply(cl, 1:sim, bootfit,
                                           n = n, m = m, beta0 = beta0,
                                           beta = beta, tau2 = tau2, alpha = alpha,
                                           sigma2 = sigma2, distr = distr, two.sided = two.sided,
                                           Ct = Ct, censor = censor, model = model)
      parallel::stopCluster(cl)
    } else {
      ParaMatrixRaw <- parallel::mclapply(1:sim, bootfit,
                                          n = n, m = m, beta0 = beta0,
                                          beta = beta, tau2 = tau2, alpha = alpha,
                                          sigma2 = sigma2, distr = distr, two.sided = two.sided,
                                          Ct = Ct, censor = censor, model = model,

                                          mc.cores = 1)
    }

  } else {
    return(0)
  }

  return(ParaMatrixRaw)
}
