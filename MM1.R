# M/M/1 Calculations
# Lyn 2.18.23
# This file holds Measure of Effectiveness for M/M/1 system as well as p_n
# page 85 print, 95 scroll 
# mod 4, tab 2 example 

# ITERATIVE METHOD ====
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

# MEASURE OF EFFECTIVENESS =====
# L = avg number of customers in system 
# Lq = avg number of customers in queue
# Lq_prime = expected size of a nonempty queue
# W = average wait time in system 
# example in mod 4, tab 2 or 3.1 in tetb ook 
get_rho <- function(lambda, mu){
  return(lambda/mu)
}

# L = avg number of customers in system = E[N] = sum(n*p_sub_n) from 0 to inf
# L = (1-rho)*sum(n*rho^n) from 0 to inf

L(rho = 5/6, lambda_mu_given = F)

L <- function(rho = NULL ,lambda = NULL,  mu = NULL, lambda_mu_given = T){
  if(lambda_mu_given == T){
    rho = get_rho(lambda, mu)
  }
  if(rho < 1 ){
    print("rho < 1, no system explosion. All good")
  }else{
    print("rho is NOT <1, system explodes KABOOM!")
  }
  L = rho/(1-rho)
  return(L)
}
L(lambda = 5, mu = 6)


Lq <- function(rho = NULL ,lambda = NULL,  mu = NULL, lambda_mu_given = T){
  if(lambda_mu_given == T){
    rho = get_rho(lambda, mu)
  }
  Lq = rho^2/(1-rho)
  return(Lq)
}
Lq(lambda = 5, mu = 6)

Lq_prime<- function(rho = NULL ,lambda = NULL,  mu = NULL, lambda_mu_given = T){
  if(lambda_mu_given == T){
    rho = get_rho(lambda, mu)
  }
  print("expected size of a nonempty queue (Lq'):")
  return(1/(1-rho))
}
Lq_prime(lambda = 5,mu = 6)

# p_0 = percentage of time arrive won't have to wait
# because there are 0 people in the system
p_0 <- function(rho = NULL ,lambda = NULL,  mu = NULL, lambda_mu_given = T){
  if(lambda_mu_given == T){
    rho = get_rho(lambda, mu)
  }
  print("percentage of time customer arrives and doesn't have to wait (p_0):")
  return(1-rho)
}
p_0(lambda = 5, mu = 6)


W <- function(lambda = NULL, mu = NULL, lambda_mu_given = T, L_given = F){
  if(L_given == T){
    print("average waiting time in system (W):")
    return(L/lambda)
  }
  if(lambda_mu_given == T){
    rho = get_rho(lambda, mu)
  }
  W = rho/(lambda*(1-rho))
  print("average waiting time in system (W):")
  return(W)
}
W(lambda = 5, mu = 6)

Wq <- function(lambda = NULL, mu = NULL, lambda_mu_given = T, Lq_given = F){
  if(Lq_given == T){
    print("average wait time in queue (Wq):")
    
    return(L/lambda)
  }
  if(lambda_mu_given == T){
    rho = get_rho(lambda, mu)
  }
  Wq = (rho/(mu - lambda))
  print("average wait time in queue (Wq):")
  return(Wq)
}
Wq(lambda = 5, mu = 6)

rho_to_the_n <- function(rho, seats){
  n = seats+ 1
  print(paste("there are ", seats, "seats, and probability that a customer arrives and cannot find seat is (rho^(seats + 1):"))
  return(rho^n)
}
rho_to_the_n(rho = 5/6, seats = 4)



