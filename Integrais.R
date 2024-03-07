library(bayesShrink)

# -------- Função -------- #
# d - coeficientes empiricos
# t - parâmetro da logistica
# a - alpha
# s - desvio padrão

LogShrink <- function(d, t, a, s){
  # Criando variáveis
  n <- length(d)
  u <- rnorm(n)
  int.1 <- vector(mode='numeric')
  int.2 <- vector(mode='numeric')
  
  # f.d.p. logistica
  logistica <- function(x){
    exp(-x/t)/(t * (1 + exp(-x/t)))^2
  }
  
  # Apoximando as integrais
  for (i in 1:n){
    int.1[i] <- (s*u[i] + d[i]) * logistica(s*u[i] + d[i])
    int.2[i] <- logistica(s*u[i] + d[i])
  }
  
  num <- (1-a) * mean(int.1)
  den <- a * dnorm(d/s)/s + (1-a) * mean(int.2)
  
  return(num/den)
}


# -------- Testes -------- #
set.seed(282829)


LogShrink(c(0,1,2,3), 1, 0.8, 1)


curve(LogShrink(x, t=1, a=0.8    , s=1), from=-6, to=6)
curve(logshrink(x, t=1, alpha=0.8, s=1), from=-6, to=6)


uu <- rnorm(1)
logistica_t <- function(x, t){
  exp(-x/t)/(t * (1 + exp(-x/t)))^2
}
s <- 1
d <- 1
a <- 0.9

apx1 <- (s*uu + d) * logistica_t(s*uu + d, t=1)
apx2 <- logistica_t(s*uu + d, t=1)

n1 <- (1-a) * apx1
d1 <- a * dnorm(d/s)/s + (1-a) * mean(apx2)
r1 <- n1/d1
r1


# bayesShrink
t <- 1

x = (s * uu + d) * (cosh((s * uu + d)/(2 * t)))^(-2)
int1 = mean(x)
y = (cosh((s * uu + d)/(2 * t)))^(-2)
int2 = mean(y)
num = (1 - a) * int1
den = 4 * t * a * dnorm(d, 0, s)/s + (1 - a) * int2
logshrink = num/den
logshrink


