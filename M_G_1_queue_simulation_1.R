M_M_G <- function(){
a <- 2
b <- 5
d <- 10
t <- 0
t1 <- 0
t2 <- d
n <-0
i <-1
j <-1
k <- 1
iat <- rexp(2*d,rate=a)
ft <- c(rep(0,2*d))
s <- c(rep(0,2*d))
at <- c(rep(0,2*d))

while (t < d)
{
  if (t1 < t2)                
  {
    t <- t1
    t1 <- t + iat[i]
    t <- t1
    n <- n+1
    at[i] <- t1
    i <- i+1
    
    if (n == 1)
    {
      
      t2 = t + abs(rnorm(1,mean=(1/b),sd=1))
      
    }
  }
  else                        
  {
    
    
    s[j] <- t2 - t
    ft[j] <- t2
    t <- t2
    n <- n-1
    j <- j+1
   
    
    if (n > 0){
      
      t2 <- t + abs(rnorm(1,mean=(1/b),sd=1))
      s[j] <- t2 - t
      ft[j] <- t2
      t <- t2
      n <- n-1
      j <- j+1
    }
    
    else
    {
      t2 <- d
    }
  }
}

while(n !=0){
  
  t2 <- t + abs(rnorm(1,mean=(1/b),sd=1))
  s[j] <- t2 - t
  ft[j] <- t2
  t <- t2
  n <- n-1
  j <- j+1
  
  
}


at <- at[which(at[]!= 0)]
at <- at[which(at[] < d)]


l <- length(at)
at <- at[1:l]
ft <-ft[1:l]
s <- s[1:l]
s_2 <- s*s
e_s <- mean(s_2)
roh <- a/b


total_time <- ft - at
wait_time <- total_time - s
wait_time_avg <- mean(wait_time)
wait_time_avg_pk <- (a * e_s)/2* (1-roh)

print(paste("The simulated average waiting time of a customer in queue is found to be: ", wait_time_avg))
print(paste("The average waiting time of a customer in queue as calculated by Pollaczek-Khinchin mean formula is found to be: ", wait_time_avg_pk))
}

M_M_G()
