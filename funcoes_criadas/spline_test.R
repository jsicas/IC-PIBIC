# Função para teste de splines

spline_test <- function(rep=1, data, formula, snr=4)
{
  # objetos
  MSE <- vector(mode='double')
  response <- as.character(formula[[2]])
  n <- length(data[[response]])
  
  for (i in 1:rep) {
    # ruído
    ruido <- rnorm(n, 0, sd(data[[response]])/snr)  # sd(ruido) = sd(sinal)/SNR
    data.ruido <- data
    data.ruido[[response]] <- data[[response]] + ruido  # Adicionando ruído
    
    # modelo
    fit <- lm(formula, data.ruido)
    MSE[i] <- mean(fit$residual^2)
  }
  return(MSE)
}