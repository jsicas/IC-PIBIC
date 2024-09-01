phi_jk <- function(x, j, k=0)
{
  ifelse(k/2^j < x & x <= (k+1)/2^j, 2^(j/2), 0)
}