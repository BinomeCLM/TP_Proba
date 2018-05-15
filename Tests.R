library(randtoolbox)
source('generateurs.R')

TestQu1 <- function ()
{
  hist(RANDU(1000, 200))
  hist(StandardMinimal(1000, 200))
  mt = MersenneTwister(1000, 1, 200)
  hist(mt$x[,1], main="MersenneTwister")
  sob <- Sobol(1000,1)
  hist(sob,xlab='',main='Sobol')
}

TestQu2 <- function ()
{
  n <- 1000
  
  uRandu <- RANDU(1000,20)
  plot(uRandu[1:(n-1)],uRandu[2:n])
  
  uStm <- StandardMinimal(1000,20)
  plot(uStm[1:(n-1)],uStm[2:n])
  
  uMt <- MersenneTwister(n, 1, 200)
  plot(uMt$x[1:(n-1)],uMt$x[2:n])
  
  uSob <- Sobol(n,1)
  plot(uSob[1:(n-1)],uSob[2:n])
  
  
}

Frequency <- function(x, nb)
{

  n <- nb*length(x)
  sommeTotal <- 0
  for (i in 1:length(x)){
    bn <- binary(x[i])[(32-nb+1):32]
    sommeTotal <- sommeTotal + sum(2*bn-1)
  }
  
  sObs <- abs(sommeTotal)/(n^(1/2)) 
  pValeur <- 2*(1-pnorm(sObs))
  
  return (pValeur)
}

QualiteGenerateur <- function(nomGenerateur)
{
  tabPval <- array(0, 100)
  tab <- sample(1:2000,100)
  if (nomGenerateur==1){
    for (i in 1:100){
      val <- RANDU(k = 1000, graine = tab[i])
      tabPval[i] <- Frequency(val, 31)
    }
  }
  else if (nomGenerateur == 2) {
    for (i in 1:100){
      val <- StandardMinimal(k = 1000, graine = tab[i])
      tabPval[i] <- Frequency(val, 31)
    }
  }
  else if (nomGenerateur == 3){
    for (i in 1:100){
      val <- Sobol(1000, 1)
      tabPval[i] <- Frequency(val, 31)
    }
  }
  else {
    for (i in 1:100){
      val <- MersenneTwister(1000,1, graine = tab[i])
      tabPval[i] <- Frequency(val$x[,1], 32)
    }
  }
  return (tabPval)
}

Runs <- function(x,nb)
{
  sommeTotal <- 0
  proportion <- 0
  n <- nb*length(x)
  VnObs <- 1 # Car +1 à la fin forcément
  
  for (i in 1:length(x)){
    bn <- binary(x[i])[(32-nb+1):32]
    sommeTotal <- sum(bn) + sommeTotal
    for (j in 1:(length(bn)-1)){
      if (bn[j]==bn[j+1]){
        VnObs <- VnObs + 1
      }
    }
  }
  
  proportion <- sommeTotal/n
  
  if(abs(proportion-(1/2))>=(2/(nb^(1/2)))){
    pValeur <- 0.0
    return (pValeur)
  }
  else {
    pValeur <- 2*(1-pnorm((abs(VnObs-2*proportion*n*(1-proportion)))/(2*(n^(1/2))*proportion*(1-proportion))))
  }
  
  return (pValeur)
}

testRuns <- function(nomGenerateur)
{
  tabPval <- array(0, 100)
  tab <- sample(1:2000,100)
  if (nomGenerateur==1){
    for (i in 1:100){
      val <- RANDU(k = 1000, graine = tab[i])
      tabPval[i] <- Runs(val, 31)
    }
  }
  else if (nomGenerateur == 2) {
    for (i in 1:100){
      val <- StandardMinimal(k = 1000, graine = tab[i])
      tabPval[i] <- Runs(val, 31)
    }
  }
  else if (nomGenerateur == 3){
    for (i in 1:100){
      val <- Sobol(1000, 1)
      tabPval[i] <- Runs(val, 31)
    }
  }
  else {
    for (i in 1:100){
      val <- MersenneTwister(1000,1, graine = tab[i])
      tabPval[i] <- Runs(val$x[,1], 32)
    }
  }
  return (tabPval)
}