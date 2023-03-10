# homework #2 Queuing Theory 
# Lyn Nguyen 

library(dplyr)
getEventBasedResults <- function(S_of_n, A_of_n = NULL, T_of_n = NULL){
  # Need at least 2 parameters! (S_of_n & either A_of_n or T_of_n)

  # Given S(n) and A(n)
  # if A(n) arrival time is input, calculate T(n)
  if(is.null(T_of_n)){ 
    get_T_of_n <- function(A_of_n){
      return(dplyr::lead(A_of_n) - A_of_n)
    }
    T_of_n <- get_T_of_n(A_of_n)
  }
  
  # Given S(n) and T(n)
  # if T(n) inter-arrival is input, calculate A_of_n
  # T(n) = inter-arrival time between n and n+1 customer
  if(is.null(A_of_n)){
    A_of_n <- c(0) # because first customer arrives at time 0 
    
    for(i in 2:length(T_of_n)){
      this_A_of_n <- T_of_n[i-1] + A_of_n[i-1] 
      A_of_n[i] <- this_A_of_n
    }
  }
  
  print(T_of_n)
  print(A_of_n)
  
  n <- length(A_of_n)
  
  output <- data.frame(n = 1:n, A_of_n, S_of_n, T_of_n)
  
  # U(1) = time first customer enters service is 0 
  output$U_of_n[1] <- A_of_n[1]#0 
  output$D_of_n[1] <- output$U_of_n[1] + output$S_of_n[1]
  output$Wq_of_n[1] <- output$U_of_n[1] - output$A_of_n[1]
  output$W_of_n[1] <- output$Wq_of_n[1] + output$S_of_n[1]
  
  for(i in 2:nrow(output)){
    output$U_of_n[i] <- max((output$D_of_n[i-1]), output$A_of_n[i]) 
    output$D_of_n[i] <- output$U_of_n[i] + output$S_of_n[i]
    output$Wq_of_n[i] <- output$U_of_n[i] - output$A_of_n[i]
    output$W_of_n[i] <- output$Wq_of_n[i] + output$S_of_n[i]
  }
  
  # "mean line delay" or "mean time in queue" = Wq
  Wq <-  sum(output$Wq_of_n)/n
  
  # "mean system waiting time" or "mean time in system" = W
  W <- sum(output$W_of_n)/n
  
  # lambda = rate of arrival = mean arrival rate = # customers/total system time
  lambda <- n/output$D_of_n[n]
  
  # "average system size" = L 
  L <-  lambda*W
  
  # "average queue size" = Lq
  Lq <- lambda*Wq
  
  # "fraction of time server was idle" = p_idle = 1-p_busy
  p_busy = L-Lq
  p_idle = 1-p_busy
  

  result = list(result_table = output, column_sums = colSums(output, na.rm = T), 
                Wq = Wq, W = W, lambda = lambda, L= L, Lq= Lq, p_idle = p_idle)
  return(result)
  
}


# Given D(n) & A(n)
getEventBasedResults_given_D_and_A <- function(D_of_n, A_of_n){
  T_of_n <- dplyr::lead(A_of_n) - A_of_n
  o <- data.frame(n = 1:length(D_of_n), A_of_n, S_of_n = NA, T_of_n, 
                       U_of_n = NA, D_of_n)
  o$S_of_n[1] <- o$D_of_n[1] - o$A_of_n[1]
  o$U_of_n[1] <- o$D_of_n[1] - o$S_of_n[1]
  o$Wq_of_n[1] <- o$U_of_n[1] - o$A_of_n[1]
  o$W_of_n[1] <- o$Wq_of_n[1] + o$S_of_n[1]
  
  for(i in 2:nrow(o)){
    o$U_of_n[i] <- max((o$D_of_n[i-1]), o$A_of_n[i]) 
    o$S_of_n[i] <- o$D_of_n[i] - o$U_of_n[i]
    o$Wq_of_n[i] <- o$U_of_n[i] - o$A_of_n[i]
    o$W_of_n[i] <- o$Wq_of_n[i] + o$S_of_n[i]
  }
  n <- length(A_of_n)
  # "mean line delay" or "mean time in queue" = Wq
  Wq <-  sum(o$Wq_of_n)/n
  # "mean system waiting time" or "mean time in system" = W
  W <- sum(o$W_of_n)/n
  # lambda = rate of arrival = mean arrival rate 
  lambda <- n/o$D_of_n[n]
  # "average system size" = L 
  L <-  lambda*W
  # "average queue size" = Lq
  Lq <- lambda*Wq
  # "fraction of time server was idle" = p_idle = 1-p_busy
  p_busy = L-Lq
  p_idle = 1-p_busy
  result = list(result_table = o, column_sums = colSums(o, na.rm = T), 
                Wq = Wq, W = W, lambda = lambda, L= L, Lq= Lq, p_idle = p_idle)
  return(result)  
}



# Reset input to NULL's
resetInputs <- function(){
  S_of_n <<- NULL
  T_of_n <<- NULL
  A_of_n <<- NULL
}


# Problem 1 =====
resetInputs()
# customers
n <- c(1:20)
# Interarrival time (from n to n + 1)
T_of_n <- c(1, 9, 6, 4, 7, 9, 5, 8, 4, 10, 6, 12, 6, 8, 9, 5, 7, 8, 8, 7)
# Service Time
S_of_n <- c(3, 7, 9, 9, 10, 4, 8, 5, 5, 3, 6, 3, 5, 4, 9, 9, 8, 6, 8, 3)
hw2.prob1 <- getEventBasedResults(S_of_n = S_of_n, A_of_n = A_of_n, T_of_n = T_of_n)
hw2.prob1

# subset where A(n) >= D(n-1) represents customers who immediately taken into service
a <- hw2.prob1$result_table
a <- a %>% filter(A_of_n >= lag(D_of_n))
sum(a$S_of_n)/nrow(a) # S' = 6.43 = W'= system wait time of customers immediately served


# Problem 2 ========
resetInputs()
n <- 1:10
A_of_n <- seq(5,50, by = 5)
D_of_n <- c(7, 17, 23, 29, 35, 38, 39, 44, 46, 60)
getEventBasedResults_given_D_and_A(D_of_n, A_of_n)



# Lecture Example (Section 1.6 textbook) ======
resetInputs()
n <- c(1:12)
S_of_n <- c(1, 3, 6, 2, 1, 1, 1, 2, 5, 1, 1, 3)
# A_of_n <- c(0, 2, 3, 6, 7, 8, 12, 14, 19, 20, 24, 26)
# what if T is provided? 
T_of_n <- c(2, 1, 3, 1, 1, 4, 2, 5, 1, 4, 2, NA)
getEventBasedResults(S_of_n = S_of_n, A_of_n = A_of_n, T_of_n = T_of_n)






# MARKOV CHAIN #####

# Problem 5 
P = matrix(c(.2, .3, .5, .4, .4, .2, .4, .6, 0), nrow = 3, byrow = T)
# part b
P%*%P
# part c
P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P 
# part e
P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P 



# Problem 6 
P = matrix(c(.25, .2, .12, .43, .25, .2, .12, .43,
             0, .25, .2, .55, 0, 0, .25, .75), 
           ncol = 4, byrow = T)
P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P
  # check if pi is correct 
pi = matrix(c(.027, .077, .22561, .673), nrow = 1, byrow = T)

pi %*% P

e = matrix(c(1,1,1,1), nrow = 4)
pi%*%e
