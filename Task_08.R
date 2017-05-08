#Function#
#data frame with numeric values
functiondataframe <- data.frame(
  a=c(1,2,3,4,5,6,7,8,9,10),
  b=c(1,2,3,4,5,6,7,8,9,10)
)

#data frame with non-numeric values#
nonumericdataframe<- data.frame(
  a=c(2,5,70,40,"cheese",15),
  b=c(5,62,16,18,"bread",182)
)

#not a data frame#
notadataframe <- "sorry"

#make the function that will add the columns "a" and "b" from the dataframe and return the sum as a new column in the same dataframe#
addition <- function(x, name) {
  if(is.data.frame(x)!=TRUE)
    stop("THIS IS NOT A DATA FRAME")
  
  if(is.numeric(x[ ,1])!=TRUE)
     warning("column 1 contains non-numeric values")
    
  if(is.numeric(x[ ,2])!=TRUE)
     warning("column 2 contains non-numeric values")
  
  x[[name]] <- x[ ,1] + x[ ,2]
  
  return(x)
}

#test the function#

addition(functiondataframe, name="a+b")

addition(nonumericdataframe, name="a+b")

addition(notadataframe, name="a+b")

#The function looks like it is working#

#Next, make a function that adds all the numbers between 1 and 10,000 using using a for loop#
#Determine the length of the vector and store that value as "n"#
#The for loop repeats from 1 to n and each cycle consecutively adds the next number in that column to the previous sum#
#The output needs to start at 0#

additionloop <- function(x){
  n<-(length(x))
  output <-0
  for(i in 1:n){
    output <- output+x[i]
  }
  return(output)
}

#This function should return the same output as the sum() function:

additionloop(1:10^4)

sum(1:10^4)

#To test how fast each of these are, install the microbenchmark package

install.packages("microbenchmark")
library(microbenchmark)

#test the two functions
test.vec <- 1:10^4
microbenchmark(
  additionloop(test.vec),
  sum(test.vec)
  )
#The sum() function is much faster than the for loop
#sum() is a function which is coded in C
#Code execution is faster in C than in R

