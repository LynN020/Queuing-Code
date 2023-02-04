# homework #2 Queuing Theory 
# Lyn Nguyen 

library(dplyr)


getEventBasedResults <- function(S_of_n, A_of_n = NULL, T_of_n = NULL){
  # Need at least 2 parameters! (S_of_n & either A_of_n or T_of_n)

  # if A(n) arrival time is input, calculate T(n)
  if(is.null(T_of_n)){ 
    get_T_of_n <- function(A_of_n){
      return(dplyr::lead(A_of_n) - A_of_n)
    }
    T_of_n <- get_T_of_n(A_of_n)
  }
  
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
  output$U_of_n[1] <- 0 
  output$D_of_n[1] <- output$U_of_n[1] + output$S_of_n[1]
  output$Wq_of_n[1] <- output$U_of_n[1] - output$A_of_n[1]
  output$W_of_n[1] <- output$Wq_of_n[1] + output$S_of_n[1]
  
  for(i in 2:nrow(output)){
    output$U_of_n[i] <- max((output$D_of_n[i-1]), output$A_of_n[i]) 
    output
    output$D_of_n[i] <- output$U_of_n[i] + output$S_of_n[i]
    output
    output$Wq_of_n[i] <- output$U_of_n[i] - output$A_of_n[i]
    output
    output$W_of_n[i] <- output$Wq_of_n[i] + output$S_of_n[i]
  }
  
  # "mean line delay" or "mean time in queue" = Wq
  Wq <-  sum(output$Wq_of_n)/n
  
  # "mean system waiting time" or "mean time in system" = W
  W <- sum(output$W_of_n)/n
  
  # lambda = rate of arrival = mean arrival rate 
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





# Lecture Example (Section 1.6 textbook) ======
resetInputs()
n <- c(1:12)
S_of_n <- c(1, 3, 6, 2, 1, 1, 1, 2, 5, 1, 1, 3)
# A_of_n <- c(0, 2, 3, 6, 7, 8, 12, 14, 19, 20, 24, 26)
# what if T is provided? 
T_of_n <- c(2, 1, 3, 1, 1, 4, 2, 5, 1, 4, 2, NA)
getEventBasedResults(S_of_n = S_of_n, A_of_n = A_of_n, T_of_n = T_of_n)











# A(n) = time arrives 
get_A_of_n <- function(){
  if(n == 0){
    return(0) # first customer's arrival starts the clock at system that starts out empty
  }else{
    return(T_of_n_minus_1 + A_of_n_minus_1)
  }
}




# U(1) = time first customer enters service is 0 
U_1 = 0

# T(n) = inter-arrival time between n and n+1 customer 
get_T_of_n <- function(A_of_n){
  return(dplyr::lead(A_of_n) - A_of_n)
}

# A(n) = time arrives 
get_A_of_n <- function(){
  if(n == 0){
    return(0) # first customer's arrival starts the clock at system that starts out empty
  }else{
    return(T_of_n_minus_1 + A_of_n_minus_1)
  }
}

# U(n) = time customer n starts service
get_U_of_n <- function(D_of_n, A_of_n){
 return(max(dplyr::lag(D_of_n), A_of_n))
}

# D(n) = Departure time
get_D_of_n <- function(U_of_n, S_of_n){
  return(U_of_n + S_of_n)
}

# Wq(n) = Time customer spends in q
get_Wq_of_n <- function(U_of_n, A_of_n){
  return(U_of_n - A_of_n)
}

# W(n) = Time customer spends in system
get_W_of_n <- function(Wq_of_n, S_of_n){
  return(Wq_of_n + S_of_n)
}



# Output 
# average time in the q: Wq_of_n 
# average time in the system: W_of_n

# average system wait time of those who had to wait for service. 
  # exclude those who were immediately taken into service 
  # i.e. exclude n's where A(n) arrival = U(n) start service 

# average length of queue ?? 

# average number in the system: N_of_n

# 