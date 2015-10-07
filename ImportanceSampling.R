# Importance sampling helper script and functions

integrando <- function(x,m=1){
  m*exp(-m*x)
}

MethodComparison <- function(nsim, m, a=0, b=2, FUN=integrando, alpha=.05){
  quant = qnorm(alpha/2, lower.tail = FALSE)
  #Estimación con Uniformes [0.2]
  U <- runif(nsim,0,2)
  Eval.Unif <- 2*sapply(U,FUN)
  Estim.Unif <-mean(Eval.Unif)
  S2.Unif <- var(Eval.Unif)
  lu.Unif <- Estim.Unif + sqrt(S2.Unif/nsim)*quant
  li.Unif <- Estim.Unif - sqrt(S2.Unif/nsim)*quant
  #Estimación con exponencial truncada
  U2 <- runif(nsim,0,1)
  ExpTruncada <- ((-1/m)*(log(1-(U2*(1-exp(-2*m))))))
  Eval.ExpTruncada <- FUN(ExpTruncada,m)*((1-exp(-2*m))/(1-exp(-m*ExpTruncada)))
  Estim.ExpTruncada <-mean(Eval.ExpTruncada)
  S2.ExpTruncada <- var(Eval.ExpTruncada)
  lu.ExpTruncada <- Estim.ExpTruncada + sqrt(S2.ExpTruncada/nsim)*quant
  li.ExpTruncada <- Estim.ExpTruncada - sqrt(S2.ExpTruncada/nsim)*quant
  #Estimación con Beta(1,5)
  Beta <- rbeta(nsim,shape1=1, shape2=2*m)
  Eval.Beta <- FUN(Beta,m)/dbeta(Beta,shape1=1,shape2=2*m)
  Estim.Beta <-mean(Eval.Beta)
  S2.Beta <- var(Eval.Beta)
  lu.Beta <- Estim.Beta + sqrt(S2.Beta/nsim)*quant
  li.Beta <- Estim.Beta - sqrt(S2.Beta/nsim)*quant
  real.value <- 1-exp((-2)*m)
  results <- data.frame(Nsim=nsim,LI.Unif=li.Unif,Estim.Unif=Estim.Unif,LU.Unif=lu.Unif,
                        LI.Exp=li.ExpTruncada,Estim.Exp=Estim.ExpTruncada,LU.Exp=lu.ExpTruncada,
                        LI.Beta=li.Beta,Estim.Beta=Estim.Beta,LU.Beta=lu.Beta,Real.Value=real.value)
  return(results)
}

FullDataGenerator <- function(m){
  full.results <- data.frame()
  full.results <- rbind(MethodComparison(1000,m),MethodComparison(2000,m),
                        MethodComparison(3000,m),MethodComparison(4000,m),
                        MethodComparison(5000,m),MethodComparison(6000,m),
                        MethodComparison(7000,m),MethodComparison(8000,m),
                        MethodComparison(9000,m),MethodComparison(10000,m))
}

ErrorGenerator <- function(m){
  results <- FullDataGenerator(m)
  errors <- data.frame(Nsim = results$Nsim,
                       Error.Unif = abs(results$Estim.Unif - results$Real.Value),
                       Error.Exp = abs(results$Estim.Exp - results$Real.Value),
                       Error.Beta = abs(results$Estim.Beta - results$Real.Value))
  return(errors)
}



