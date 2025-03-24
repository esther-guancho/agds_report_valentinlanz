r <- 2
area <- r^2*pi
print(area)


s <- rep(c(0,pi), length = 5)
s
?seq



d <- datasets::rivers
print(d)
class(d)
length(d)
mean(d)
median(d)
min(d)
max(d)
quantile(d, probs = 0.33)

q = datasets::quakes
dim(q)      #Dimension of the dataframe. Anzahl Zeilen und Spalten
magn <- q[,4]




f2c <- function(temp_f){
  temp_c <- (temp_f -32) * 5/9
  return (temp_c)
}
f2c(100)
f2c(50)





#Exercises 3.3

#----Gauss variations----
total <- 0
for (i in c(1:100)) {
  total <- total + i
  print(total)
}

#mit while-loop

total_while <- 0
idx <- 0
while(idx <= 100) {
  total_while <- total_while + idx
  idx <- idx + 1
  print(total_while)
}

#---- multiples of 3 and 7----

sum_of_multiples <- 0
for (i in c(1:100)){
  if ((i %% 3 == 0) && (i %% 7 == 0) ) {
    sum_of_multiples <- sum_of_multiples + i
  print(sum_of_multiples)
  }
}
cat("The sum of multiples of 3 and 7 within 1-100 is: ", sum_of_multiples)





# ----Nested Loops----


myvec <- c(8, 4, 12, 9, 15, 6)

mymat <- matrix(c(6, 7, 3, NA, 15, 6, 7, 
                  NA, 9, 12, 6, 11, NA, 3, 
                  9, 4, 7, 3, 21, NA, 6,
                  rep(5, 7)),
                nrow = 4, byrow = TRUE)
mymat
?matrix
length(mymat)
nrow(mymat)
d


max(myvec)
for (i in seq(nrow(mymat))) {
  m <- max(myvec)
  print(m)

  for(j in seq(length(mymat[i,]))) {
    if (is.na(mymat[i,j])) {
      mymat[i,j] <- m
    }
  }
  myvec <- myvec[!(myvec == m)]  #remove maximum value from myvec
  print(mymat)

}


# ----Interpolation----

new_vec <- c(rep(NA, 100))
for (i in c(1:100)){
  if (i <= 25) {
    new_vec[i] <- 6
  }
  else if (i >= 66) {
    new_vec[i] <- -20
  }
}
new_vec
plot(C(1:100), new_vec)

?approx
plot(approx(new_vec, method = "linear"))
