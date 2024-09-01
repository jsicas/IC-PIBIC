# função de simulação para classificação via ondaletas com 2 classes

# fun_real - pontos da função original
# fun_mod - pontos da função modificada
# snr - razão sinal-ruído (snr=sd(sinal)/sd(ruido))
# rep - número de replicações
# n_treino - número de curvas por classes para treino
# n_teste - número de curvas por classes para teste
# signal = sd(sinal)

class_wave <- function(fun_real, fun_mod, snr, rep, n_treino, n_teste=NULL, signal=7)
{
  # configuracoes
  require(wavethresh)
  require(magrittr)
  if (is.null(n_teste)) n_teste <- n_treino

  n <- length(fun_real)  # quantidade de pontos por curva
  resposta <- vector('list', rep)
  pb <- progress::progress_bar$new(format = '  [:bar] :percent :elapsed Iteração :current de :total',
    total = rep, clear = FALSE, width = 70)

  # normalizando
  fun_real <- fun_real/sd(fun_real) * signal
  fun_mod <- fun_mod/sd(fun_mod) * signal
  # replicando
  for (i in 1:rep) {if (i %% 50 == 0) pb$tick(50)  # atualiza barra de progresso
    # amostra treino
    treino <- matrix(nrow = 2*n_treino, ncol = n)
    
    for (j in 1:n_treino) {
      treino[2*j - 1, ] <- fun_real + rnorm(n, 0, signal/snr)  # impar = C1
      treino[2*j, ] <- fun_mod + rnorm(n, 0, signal/snr)       # par = C2
    }
    
    # DWT e formatação dos dados
    wav_treino <- apply(treino, 1, wd) %>%
      sapply(function(x = threshold(treino)) c(x$D, accessC(x, level=0))) %>%
      t() %>% data.frame()
    
    wav_treino$c <- factor(rep(c('C1', 'C2'), n_treino))  # classes
    
    # treinando modelo
    fit <- glm(c ~ . , data=wav_treino, family=binomial)
    
    # amostra teste
    teste <- matrix(nrow = 2*n_teste, ncol = n)
    
    for (j in 1:n_teste) {
      teste[2*j - 1, ] <- fun_real + rnorm(n, 0, signal/snr)  # impar = C1
      teste[2*j, ] <- fun_mod + rnorm(n, 0, signal/snr)       # par = C2
    }
    
    wv_teste <- apply(teste, 1, wd) %>%
      sapply(function(x = threshold(teste)) c(x$D, accessC(x, level=0))) %>%
      t() %>% data.frame()
    
    # resultado do modelo
    predicao <- predict(fit, type = 'response', newdata = data.frame(wv_teste))
    class_modelo <- ifelse(predicao > 0.5, 'C2', 'C1')
    resposta[i] <- list(table('Modelo' = class_modelo,
                         'Classe Verdadeira' = factor(rep(c('C1', 'C2'), n_teste))))
  }
  return(resposta)
}
