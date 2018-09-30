rm(list=ls())

name <- 'Zijie Feng'
liuid <- 'zijfe244'

sheldon_game <- function(player1, player2){
  s1 <- "rock"
  s2 <- "paper"
  s3 <- "scissors"
  s4 <- "lizard"
  s5 <- "spock"
  a <- "Player 1 wins!"
  b <- "Player 2 wins!"
  c <- "Draw!"
  if(player1 == player2){
    c
  }
  else if(player1 == s3){
    if(player2 == s2||player2 == s4){
      a
    }else
      b
  }
  else if(player1 == s2){
    if(player2 == s1||player2 == s5){
      a
    }else
      b
  }
  else if(player1 == s1){
    if(player2 == s4||player2 == s3){
      a
    }else
      b
  }
  else if(player1 == s4){
    if(player2 == s5||player2 == s2){
      a
    }
    else
      b
  }
  else if(player1 == s5){
    if(player2 == s3||player2 == s1){
      a
    }else
      b
  }else
    stop()
}

my_moving_median <- function(x, n, ...){
  if(!is.vector(x)||!is.numeric(n)||n<=0)
    stop()
  else{
    y <- NA
    for(i in 1:(length(x)-n)){
      y[i] <- median(x[i:(i+n)], ...)
    }
  }
  y
}

for_mult_table <- function(from, to){
  if(!is.numeric(from)||!is.numeric(to))
    stop()
  else{
    vec <- from:to
    temp <- vec*from
    for(i in ((from+1):to)){
      temp <- c(temp, vec*i)
    }
    out <- matrix(temp,nrow=length(vec))
    colnames(out) <- vec
    rownames(out) <- vec
  }
  out
}

cor_matrix <- function(X){
  if(!is.data.frame(X))
    stop()
  else{
    sz <- dim(X)
    n <- sz[1]    # number of objects
    m <- sz[2]    # number of features
    temp <- 0
    for(i in 1:m){
      a <- X[,i]
      m1 <-mean(a)
      v1 <- sum((a-m1)^2)/n
      for(j in 1:m){
        b <- X[,j]
        m2 <- mean(b)
        v2 <- sum((b-m2)^2)/n
        c <- sum((a-m1)*(b-m2))/n
        temp[(i-1)*m+j] <- c/sqrt(v1*v2)
      }
    }
    out <- matrix(temp, ncol=m)
    out
  }
}

find_cumsum <- function(x, find_sum){
  if(!is.numeric(x)||!is.vector(x)||!is.numeric(find_sum))
    stop()
  else{
    cs <- 0
    i <- 1
    while(cs<find_sum&&i<=length(x)){
      cs <- cs+x[i]
      i <- i+1
    }
    cs
  }
}

while_mult_table <- function(from, to){
  if(!is.numeric(from)||!is.numeric(to))
    stop()
  else{
    vec <- from:to
    temp <- vector()
    i <- from
    while(i%in%vec){
      temp <- c(temp, vec*i)
      i <- i+1
    }
    out <- matrix(temp,nrow=length(vec))
    colnames(out) <- vec
    rownames(out) <- vec
  }
  out
}

trial_division_factorization <- function(x){
  i <-2
  id <- 1
  e <- sqrt(x)
  temp <- x
  out <- vector()
  
  while(i<=e){
    if(temp%%i==0){
      temp <- temp/i
      out[id] <- i
      id <- id+1
    }else{
      i <- i+1
    }
  }
  out
}

repeat_find_cumsum <- function(x, find_sum){
  if(!is.numeric(x)||!is.vector(x)||!is.numeric(find_sum))
    stop()
  else{
    cs <- 0
    i <- 1
    repeat{
      cs <- cs+x[i]
      i <- i+1
      if(!cs<find_sum||!i<=length(x))
        break
    }
    cs
  }
}

repeat_my_moving_median <- function(x, n, ...){
  if(!is.vector(x)||!is.numeric(n)||n<=0)
    stop()
  else{
    y <- vector()
    i <- 1
    repeat{
      y[i] <- median(x[i:(i+n)], ...)
      i <- i+1
      if(i>(length(x)-n))
        break
    }
  }
  y
}

in_environment <- function(env){
  content <- ls(env)
  content
}

where <- function(fun){
  if(!is.character(fun))
    stop()
  else{
    out <- paste(fun, "not found!")
    p <- search()  #all packages
    for(pp in p){  #search the fun in each package
      if(fun %in% ls(pp)){
        out <- pp
        break()
      }
    }
    out
  }
}

cov <- function(X){
  if(!is.data.frame(X))
    stop()
  else{
    f <- function(c){sd(c)/mean(c)}
    as.vector(unlist(lapply(X, f)))
  }
}

moment <- function(i){
  stopifnot(is.numeric(i), i>0)
  function(X){
    m <- mean(X)
    n <- length(X)
    mu <- (sum((X-m)^i))/n
    mu
  }
}

mcmc_counter_factory <- function(burnin, thin){
  stopifnot(burnin>=0, thin>0)
  iteration <- 0
  store_sample <- FALSE
  sample <- 0
  
  function(){
    iteration <<- iteration+1
    if((iteration-burnin)>0&&(iteration-burnin)%%thin==0){
      store_sample <<- TRUE
      sample <<- sample+1
    }else
      store_sample <<- FALSE
    list(iteration, store_sample, sample)
  }
}



