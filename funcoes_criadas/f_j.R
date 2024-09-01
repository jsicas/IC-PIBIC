# Aproximação de Haar para funções no intervalo [0,1]

# x: ponto avaliado
# fun: função
# j: nível de resolução

f_j <- function(x, fun, j)
{
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