rm(list = ls())
library(plyr)
library(dplyr)
library(ggplot2)

##load the driver function to calculate the roots of quadratic
##with the coefficients as a function of N
source("quadroots2.R")

##initialise data frame with the first column
##containing the number of die faces
df <- data.frame(N = 1:100)
df$R1 <- NA                 ##first root of the quadratic
df$R2 <- NA                 ##second root of the quadratic


##invoke the function to compute the roots of the equation
for(i in 1:dim(df)[1]){
  roots <- quadRoots2(df$N[i])
  df$R1[i] <- roots[1]
  df$R2[i] <- roots[2]
}

##filter out roots which are bigger than N or less than 0
fix_wrong <- function(N){   ##closure
  function(x){
  x[x < 0 | x > N] <- 0
  x
  }
}

df <- ddply(df, c("N","R1"), transform, trueR1 = (fix_wrong(N))(R1))
df <- ddply(df, c("N","R2"), transform, trueR2 = (fix_wrong(N))(R2))


##function to compute the expected value of the game for the mentioned threshold
expectedValue <- function(N){
  function(x){
    ((x^2 + 3*x - N*N - N)/(2*N))*(N/(x-N))
  }
}

df <- ddply(df, c("N","trueR1"), transform, EV1 = (expectedValue(N))(trueR1))
df <- ddply(df, c("N","trueR2"), transform, EV2 = (expectedValue(N))(trueR2))


##select the root which gives the higher expected value
df <- ddply(df, c("EV1", "EV2"), transform, prelimEV = ifelse(EV1 < EV2, EV2, EV1))
df <- ddply(df, c("EV1", "EV2","trueR1","trueR2"), transform, prelimR = ifelse(EV1 < EV2, trueR2, trueR1))

#rounding off the root to the least integer greater than R 
##since it is a strictly increasing function
df <- ddply(df, c("prelimR"), transform, finalR = ceiling(prelimR))
df <- ddply(df, c("N","finalR"), transform, finalEV = (expectedValue(N))(finalR))

##Plotting the results
ggplot(df,aes(N,finalEV)) + geom_point(colour = "red") +
  ggtitle("Plot of Expected Value vs Number of Die faces")