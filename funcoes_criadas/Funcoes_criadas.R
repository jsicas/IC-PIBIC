# --------  phi_{jk}(x) -------- #
phi_jk <- function(x, j, k=0){
  ifelse(k/2^j < x & x <= (k+1)/2^j, 2^(j/2), 0)
}


# -------- Aproximação de Haar para funções no intervalo [0,1] -------- #
f_j <- function(x, fun, j, wt=0){
  apx <- 0
  integrais <- vector(mode='numeric')
  pontos <- seq(0,1, length=1 + 2^j)
  
  for (k in 1:(length(pontos)-1)){
    integrais <- append(integrais,
                        integrate(fun, pontos[k], pontos[k+1])$value)
  }
  
  c <- integrais * 2^(j/2)
  
  for (k in 1:length(c)){
    apx <- apx + c[k]*phi_jk(x, j, k-1)
  }
  
  return(apx)
}


# -------- Shrinkage da logistica -------- #
# d - coeficientes empiricos
# a - alpha
# s - desvio padrão
# t - parâmetro da logistica

LogisShrink <- function(d, a, s, t){
  u <- rnorm(10000)
  delta <- vector(mode='double')
  
  for(i in 1:length(d)){
    int1 <- mean((s*u + d[i]) * dlogis(s*u + d[i], scale=t))
    int2 <- mean(dlogis(s*u + d[i], scale=t))
    delta[i] <- (1-a) * int1/(a * dnorm(d[i], sd=s)/s + (1-a) * int2)
  }
  
  return(delta)
}


# -------- Função de simulação -------- #
simulacao <- function(fun_real, SNR, rep=1, policy='sure', type='soft', lambda=0,
                      filter.number=1, family='DaubExPhase') {
  # Criando objetos
  ISE <- vector(mode='double')
  coef_nulo <- vector(mode='integer')
  percent_coef_nulo <- vector(mode='double')
  lambda_armazenado <- vector(mode='double')
  n <- length(fun_real)
  barra <- progress::progress_bar$new(total = rep) # barra de progresso
  
  for (i in 1:rep) {
    barra$tick()
    #message('Repeticao ', i)
    
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
      lambda_armazenado <- append(lambda_armazenado, threshold(ywt, policy=policy,
                                                               type=type, return.threshold=T)[1])
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
  if (policy == 'manual'){lambda_armazenado <- rep(lambda, rep)}
  return(data.frame('ISE'=ISE,
                    'PCN'=percent_coef_nulo,
                    'lambda'=lambda_armazenado))
}