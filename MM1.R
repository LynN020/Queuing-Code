# M/M/1 Calculations
# Lyn 2.18.23


# p_sub_n(x) ##################

# ITERATIVE METHOD ===

p_sub_n_iterative <- function(lambda, mu, n = 1){
  # pn = (1-rho)*(rho^n)
  # rho = lambda/mu
  # rho < 1
  rho = lambda/mu
  if(rho < 1 ){
    print("rho < 1, no system explosion. All good")
  }else{
    print("rho is NOT <1, system explodes KABOOM!")
  }
  p_sub_n = (1 - rho)*(rho^n)
  return(p_sub_n)
}

# example: mod4, tab 2 
lambda = 5 #per hr
mu = 6 #per hr
p_sub_n_iterative(lambda = lambda, mu = mu, n = 10)
