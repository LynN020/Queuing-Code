# homework #2 Queuing Theory 
# Lyn Nguyen 

library(dplyr)

# Problem 1
# customers
n <- c(1:20)

# Interarrival time (from n to n + 1)
T_of_n <- c(1, 9, 6, 4, 7, 9, 5, 8, 4, 10, 6, 12, 6, 8, 9, 5, 7, 8, 8, 7)
# Service Time
S_of_n <- c(3, 7, 9, 9, 10, 4, 8, 5, 5, 3, 6, 3, 5, 4, 9, 9, 8, 6, 8, 3)

bookkeeping_input <- data.frame(n, T_of_n, S_of_n)



#Lecture Example
n <- c(1:12)
S_of_n <- c(1, 3, 6, 2, 1, 1, 1, 2, 5, 1, 1, 3)
A_of_n <- c(0, 2, 3, 6, 7, 8, 12, 14, 19, 20, 24, 26)
T_of_n <- get_T_of_n(A_of_n)
bk <- data.frame(n, A_of_n, S_of_n, T_of_n)
# bk$U_of_n[1] <- 0
# bk$D_of_n[1] <- bk$U_of_n[1] + bk$S_of_n[1]

# U(n) is wrong 
bk
bk$U_of_n <- NA
bk$D_of_n <- NA
bk$Wq_of_n <- NA
bk$W_of_n <- NA

bk$U_of_n[1] <- 0 
bk$D_of_n[1] <- bk$U_of_n[1] + bk$S_of_n[1]
bk$Wq_of_n[1] <- bk$U_of_n[1] - bk$A_of_n[1]
bk$W_of_n[1] <- bk$Wq_of_n[1] + bk$S_of_n[1]

for(i in 2:nrow(bk)){
    bk$U_of_n[i] <- max((bk$D_of_n[i-1]), bk$A_of_n[i]) 
    bk
    bk$D_of_n[i] <- bk$U_of_n[i] + bk$S_of_n[i]
    bk
    bk$Wq_of_n[i] <- bk$U_of_n[i] - bk$A_of_n[i]
    bk
    bk$W_of_n[i] <- bk$Wq_of_n[i] + bk$S_of_n[i]
}


bk

colSums(bk, na.rm = T)

getEventBasedResults <- function(S_of_n, A_of_n){

  # T(n) = inter-arrival time between n and n+1 customer 
  get_T_of_n <- function(A_of_n){
    return(dplyr::lead(A_of_n) - A_of_n)
  }
  
  T_of_n <- get_T_of_n(A_of_n)
  
  output <- data.frame(n = length(A_of_n), A_of_n, S_of_n, T_of_n)
  
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
  
  

  result = list(result_table = output, column_sums = colSums(output))
  return(result)
  
}
  
result <- getEventBasedResults(S_of_n, A_of_n)
result






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