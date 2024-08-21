# Q1
## function to calculate the area of circle
area_circle<- function(radius) {return(pi * radius^2)}

## test (diameter = 3.4 cm)
area_circle(3.4 / 2)

# Q2
## function to convert Fahrenheit to centigrade
convert_f_to_c<- function(oF) {
  oC = (oF - 32) * (5 / 9)
  cat("Fahrenheit : value of ",oF, "is equivalent to value ",oC, "centigrade")
  }

# Q3
## function to return the summary (mean, median, range) of the input vector
summ_vec <- function(vector){
  cat("mean:", mean(vector), "\n") 
  cat("median:", median(vector),"\n")
  cat("range: ", range(vector)[1], " to ", range(vector)[2], "\n")
  dens<- density(vector)
  hist(vector, main = "", freq = FALSE)
  lines(dens, lty = 1, col = "blue")
}

# test 
summ_vec(rnorm(100, 35, 15))

# Q4
## function to find the median of a vector
find_median <- function(x){
  y<- sort(x)
  if(length(y) %% 2 != 0){
    return(y[(1 + length(y)) / 2])
  }
  else{
    return((y[length(y) / 2] + y[(length(y) / 2) + 1]) / 2)
  }
}

## test with odd length
x<- c(2, 1, 5, 4, 8, 7, 7)
find_median(x)

## test with even length
y<- c(3, 1, 7, 9, 10, 5)
find_median(y)

# Q5
## function that returns the Ricker model
ricker_model<- function(n0, r, time, k = 100){
  ricker<- rep(NA, time)
  ricker[1]<- n0
  t = 2
  while (t <= time){
    ricker[t] = ricker[t-1] * exp(r * (1 - (ricker[t-1] / k)))
    t = t + 1
  }
  return(ricker)
}
## test
ricker_model(50, 0.5, 3, 100)


