# PDOXpower

An R package for power calculation for PDOX experiment to determine the number of PDX lines versus subsampling within PDX lines by fitting ANCOVA fixed effects model, ANCOVA random effects model, Cox model, and fraity model

# Run the package using a toy example

require(PDXpower)
n = c(4, 6)
m = c(1, 2)
a <- PowerTable(n = n, m = m, betaA = log(30), betaB = log(60),
                          sigma2 = 0.2^2, rho = 0.1, sim = 10)
plotpower(a)
