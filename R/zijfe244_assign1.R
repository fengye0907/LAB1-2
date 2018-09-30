rm(list=ls())

name <- 'Zijie Feng'
liuid <- 'zijfe244'

# 1.1.1
my_num_vector <- function(){
  c(log(11, 10),cos(pi/5),exp(pi/3),(1173%%7)/19)
}

# 1.1.2
filter_my_vector <- function(x, leq){
  idx <- which(x>=leq)
  x[idx] <- NA
  x
}

# 1.1.3
dot_prod <- function(a, b){
  out <- sum(a*b)
  out
}

# 1.1.4
# I find when N=8, its value equals exp(1)=2.71828 with fifth decimal.
approx_e <- function(N){
  v <- 1/factorial(0:N)
  sum(v)
}

# 1.2.1
# The sum of each diagonal, row and column is 15.
my_magic_matrix <- function(){
  matrix(c(4,3,8,9,5,1,2,7,6),nrow=3)
}

# 1.2.2
calculate_elements <- function(A){
  prod(dim(A))
}

# 1.2.3
row_to_zero <- function(A, i){
  A[i,] <- 0
  A
}

# 1.2.4
add_elements_to_matrix <- function(A, x, i, j){
  A[i,j] <- A[i,j]+x
  A
}

# 1.3.1
my_magic_list <- function(){
  out <- list(info="my own list", my_num_vector(), my_magic_matrix())
  out
}

# 1.3.2
change_info <- function(x, text){
  x$info <- text
  x
}

# 1.3.3
add_note <- function(x, note){
  x <- c(x, note = note)
  x
}

# 1.3.4
sum_numeric_parts<- function(x){
  out <- 0
  for (i in 1:length(x)){
    t <- x[[i]]
    if(is.character(t) == FALSE){
      t0 <- as.numeric(t)
      out <- out+sum(t0)
    }
  }
  out
}

# 1.4.1
my_data.frame <- function(){
  id <- c(1, 2, 3)
  name <- c('John', 'Lisa', 'Azra')
  income <- c(7.30, 0.00, 15.21)
  rich <- c(FALSE, FALSE, TRUE)
  df <- data.frame(id, name, income, rich)
  df
}

# 1.4.2
sort_head <- function(df, var.name, n){
  idx <- order(df[,var.name],decreasing = TRUE)
  df0 <- df[idx,]
  df0[1:n,]
}

# 1.4.3
add_median_variable <- function(df, j){
  out <- df
  m <- median(df[,j])
  
  idx <- as.integer(df[,j]-m>0)
  idxSmaller <- df[,j]-m<0
  idx[idxSmaller] <- -1
  
  comp <- factor(idx,levels=c(1,0,-1),labels=c("Greater","Median","Smaller"))
  out[, 'compared_to_median'] <- comp
  out
}

# 1.4.4
analyze_columns <- function(df, j){
  n <- names(df)
  n1 <- n[j[1]]
  n2 <- n[j[2]]
  c1 <- df[j[1]]
  df1 <- c(mean(c1[[1]]), median(c1[[1]]), sd(c1[[1]]))
  c2 <- df[j[2]]
 # df2 <- data.frame(mean=mean(c2[[1]]), median=median(c2[[1]]), sd=sd(c2[[1]]))
  df2 <- c(mean(c2[[1]]), median(c2[[1]]), sd(c2[[1]]))
  C <- matrix(c(1,cor(c1[[1]],c2[[1]]),cor(c2[[1]],c1[[1]]),1),nrow=2)
  colnames(C) <- c(n1,n2)
  rownames(C) <- c(n1,n2)
  out <- vector(mode="list")
  out[[n1]] <- df1
  out[[n2]] <- df2
  out$correlation_matrix <- C
  out
}


