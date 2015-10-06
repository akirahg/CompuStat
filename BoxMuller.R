#Simulación Box-Muller

BoxMuller <- function(nsim, miu, sigma){
  res <- data.frame()
  U1 <- runif(nsim,0,1)
  U2 <- runif(nsim,0,1)
  X.standard <- sqrt((-2)*log(U2))*cos(2*pi*U1)
  Y.standard <- sqrt((-2)*log(U2))*sin(2*pi*U1)
  X <- miu + sigma*X.standard
  Y <- miu + sigma*X.standard
  X.normal <- rnorm(nsim, mean = miu, sd = sigma)
  res <- data.frame(BoxMuller1=X, Normal = X.normal)
  return(res)
}
