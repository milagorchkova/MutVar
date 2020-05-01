install.packages("ggplot2")
install.packages("cvequality")
install.packages("data.table")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("tidyverse")
library(ggplot2)
library(cvequality)
library(data.table)
library(dplyr)
library(gridExtra)
library(tidyverse)

setwd("C:/Users/User/Documents/Grade - Year 3/eeb397/csvs")
dir()
data2B <- read.csv("2B.csv")
names(data2B)[1]<-paste("species") 
names(data2B)[2]<-paste("n")
names(data2B)[3]<-paste("mean")
names(data2B)[4]<-paste("sd")
str(data2B)


#initialize empty vectors
SD_vector <-vector(mode='numeric',length=nrow(data2B))
mean_vector <-vector(mode='numeric',length=nrow(data2B))
n_vector <-vector(mode='numeric',length=nrow(data2B))
df_vector <-vector(mode='numeric',length=nrow(data2B))

#create these vectors and update dataframe
SD_vector<-as.vector(data2B$sd)
mean_vector <-as.vector(data2B$mean)
n_vector <-as.vector(data2B$n)
df_vector <-as.vector((n_vector-1))
data2B$df <- data2B$n-1

SD_vector
mean_vector
n_vector
df_vector

#pooled/totals
mean_total <-mean(data2B$mean)
mean_total

SD_total <- sqrt (sum(((data2B$sd)^2) * data2B$df / sum(data2B$df)))
SD_total

n_total <-sum(data2B$n)
n_total

asymptotic_test2(k=(nrow(data2B)), n = data2B$n,s = data2B$sd, x = data2B$mean,1)
#create vectors needed for test of equality: THIS IS WHEN ONLY SUMMARY STATISTICS AVAILABLE
#OR to compare "one against all"
#need:
#k = value that is # groups
#n-vector with number measurements per group
#s=vector with standard deviation of each group
#x=vector with mean of each group
#seed=an integer where to start random numbers. gives reproducible outputs

#initialize temporary vectors for loop
n_tmp <-vector(mode='numeric',length=2)
s_tmp <-vector(mode='numeric',length=2)
x_tmp <-vector(mode='numeric',length=2)

n_tmp[[2]] <- n_total
s_tmp[[2]] <-SD_total
x_tmp[[2]] <-mean_total
one.v.all <-vector(mode='list',length=nrow(data2B))

for (i in 1:(nrow(data2B)))
{
  n_tmp[[1]] <- n_vector[[i]]
  s_tmp[[1]] <- SD_vector[[i]]
  x_tmp[[1]] <- mean_vector[[i]]
  
  one.v.all[[i]] <- asymptotic_test2(2, n = n_tmp,s = s_tmp, x = x_tmp,1)
}
#output list of test statistics and p-valukes
one.v.all


##make output table to interpret these results

one.v.all <- unlist(one.v.all)
str(one.v.all)

test_statistics <- vector(mode='numeric',length=nrow(data2B))
p_values <- vector(mode='numeric',length=nrow(data2B))

for  (i in 1:(nrow(data2B)))
{
  test_statistics[[i]] <- one.v.all[[(i+(i-1))]]
  p_values[[i]] <- one.v.all[[(i+i)]]
  
}

test_statistics
p_values

p_over_five <- p_values > 0.05
p_over_five

# if p greater than 5 that means variances are SAME as total. (recall we are looking for those that are same or greater than)

##if p is less than five, that means variances are different
#next step is to figure out if var is greater or less than total
#compare outputs
data2B$cv <- (data2B$sd/data2B$mean)*100

CV_total <- (SD_total/mean_total)*100
CV_total

CV_greater <- data2B$cv > CV_total
CV_greater

cv_results <-data.frame(test_statistics,p_values,p_over_five,CV_greater)         
view(cv_results)      
