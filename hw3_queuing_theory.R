# Module 3
# Lyn Nguyen 

# Problem 3 
Q = matrix(c(-3, 3, 0, 0, 1, -3, 2, 0, 0, 0, -5, 5, 0, 1, 4, -5), 
           ncol = 4, byrow = T)
P = Q
P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P%*%P
# check if pi is correct 
p_vector = matrix(c(1/16, 3/16, 6/16, 6/16), nrow = 1, byrow = T)

p_vector %*% Q

e = matrix(c(1,1,1,1), nrow = 4)
p_vector%*%e


# Problem 5 
# Finding the root for x^4 - 10x^3 + 35x^2 - 50x + 24
f <- function(x) {x^4 - 10*x^3 + 35*x^2 - 50*x + 24}
f(1)
f(2)
f(3)
f(4)
f(6) # 120, not a root
f(8) # 840 not a root
f(12) # 7290 not a root
f(24) # 212520 not a root 
f <- function(x) {x^3 - 6*x^2 + 11*x - 6}
f(1)
f(2)
f(3)
f(6) # 60, not a root
f <- function(x) {x^2 - 3*x + 2}
f(2)

f <- function(x) {(x-4)*(x-3)*(x-2)*(x-1)}
f(1)
f(2)
f(3)
f(4)
f(6) # 120, not a root
f(8) # 840 not a root
f(12) # 7290 not a root
f(24) # 212520 not a root 

D = matrix(c(1,1,1,1,4,3,2,1,16,9,4,1,64,27,8,1), nrow = 4, byrow = T) 
soln = matrix(c(0, 1, 0, 1), nrow = 4, byrow = T)
inv(D)
X = solve(D, soln)
D%*%X
