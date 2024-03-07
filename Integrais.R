# -------- função -------- #
# d - coeficientes empiricos
# t - parâmetro da logistica
# a - alpha
# s - desvio padrão

LogShrink <- function(d, t, a, s){
  # Criando variáveis
  n <- length(d)
  u <- rnorm(n)
  apx.1 <- vector(mode='numeric')
  apx.2 <- vector(mode='numeric')
  
  # f.d.p. logistica
  logistica <- function(x, t=t){
    exp(-x/t)/(t * (1 + exp(-x/t)))^2
  }
  
  # Apoximando as integrais
  for (i in 1:n){
    apx.1[i] <- (s*u[i] + d[i]) * logistica(s*u[i] + d[i], t=t)
    apx.2[i] <- logistica(s*u[i] + d[i], t=t)
  }
  
  num <- (1-a) * mean(apx.1)
  den <- a * dnorm(d/s)/s + (1-a) * mean(apx.2)
  
  return(num/den)
}
LogShrink(c(0,1,2,3), 1, 0.8, 1)
