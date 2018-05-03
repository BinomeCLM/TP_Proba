library(randtoolbox)

MersenneTwister <- function(n,p=1,graine)
{
  set.seed(graine,kind='Mersenne-Twister')
  x <- sample.int(2^32-1,n*p)
  x <- matrix(x,nrow=n,ncol=p)
  return(list(x=x,s=x[n]))
}


Sobol <- function(n,p) 
{
  return(round(sobol(n,p)*(2^31-1)))
}


binary <- function(x)
{
  if((x<2^31)&(x>0))
    return( as.integer(rev(intToBits(as.integer(x)))) )
  else{
    if((x<2^32)&(x>0))
      return( c(1,binary(x-2^31)[2:32]) )
    else{
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}

RANDU <- function(k, graine)
{
  a <- 65539
  b <- 0
  m <- 2^31
  tab <- array(0, k)
  for(i in 1:k)
  {
    tab[i] <- graine
    graine <- (a*graine+b)%%m
  }
  return(tab)
}

StandardMinimal <- function(k, graine)
{
  a <- 16807
  b <- 0
  m <- 2^31-1
  tab <- array(0, k)
  for(i in 1:k)
  {
    tab[i] <- graine
    graine <- (a*graine+b)%%m
  }
  return(tab)
}
