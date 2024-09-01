# Função de simulação das propostas de escolha de limiar

# data: pontos da função real
# snr: razão sinal-ruído
# policy: política para escolha de limiar
# type: regra de limiar
# lambda: escolher manualmente o limiar
# filter.number: quantidade de momentos nulos, controla a suavidade da ondaleta
# family: especifica a família de ondaletas

wave_test <- function(data, snr, rep=1, policy='sure', type='soft', lambda=0,
                      filter.number=1, family='DaubExPhase')
{
  # Criando objetos
  ISE <- vector(mode='double')
  coef_nulo <- vector(mode='integer')
  percent_coef_nulo <- vector(mode='double')
  lambda_armazenado <- vector(mode='double')
  n <- length(fun_real)
  
  for (i in 1:rep) {
    # Ruido
    sd_ruido <- sd(fun_real)/SNR  # sd(ruido) = sd(sinal)/SNR
    ruido <- rnorm(n=n, mean=0, sd=sd_ruido)
    fun_ruido <- fun_real + ruido  # Adicionando ruido
    
    # Wavelet
    ywt <- wd(fun_ruido, filter.number=filter.number, family=family)  # DWT
    if (policy == 'u') {
      sigma <- mad(accessD(ywt, level=nlevelsWT(ywt) - 1))
      lambda_u <- sigma * sqrt(2 * log(n))
      lambda_armazenado <- append(lambda_armazenado, lambda_u)
      ywt_T <- threshold(ywt, policy='manual', type=type, value=lambda_u)
    } else {
      lambda_armazenado <- append(lambda_armazenado,
                                  threshold(ywt, policy=policy, type=type,
                                                              return.threshold=T)[1])
      ywt_T <- threshold(ywt, policy=policy, type=type, value=lambda)
    }
    fun_estimada <- wr(ywt_T)  # IDWT
    
    # ISE e percentual de coeficientes nulos
    residuo <- fun_real - fun_estimada
    ISE <- append(ISE, sum(residuo^2)/n)  # sum (\hat{g} - g)/n
    
    for (j in 2:(nlevelsWT(ywt_T) - 1)) {
      coef_nulo <- append(coef_nulo, accessD(ywt_T, lev=j) == 0)
    }
    percent_coef_nulo <- append(percent_coef_nulo,
                                sum(coef_nulo)/length(coef_nulo))
  }
  if (policy == 'manual') {lambda_armazenado <- rep(lambda, rep)}
  return(data.frame('ISE'=ISE,
                    'PCN'=percent_coef_nulo,
                    'lambda'=lambda_armazenado))
}
