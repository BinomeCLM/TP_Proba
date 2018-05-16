library(randtoolbox)


LoiBinomiale <- function (n,p)
{
  p_k <- (factorial(n)/(factorial(p)*factorial(n-p)))*(p^(k))*((1-p)^(n-k))
  return (p_k)
}