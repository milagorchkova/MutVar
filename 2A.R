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
data2A <- read.csv("2A.csv")
names(data2A)[1]<-paste("species") 

#subset data by species
#Amil <- subset(data1A, species == "Achillea millefolium")

str(data2A)


#try intializing p as empty data.frame
p <- vector(mode="list", length=length(data2A$species))
SD_vector <-vector(mode='list',length=length(data2A$species))
mean_vector <-vector(mode='list',length=length(data2A$species))
n_vector <-vector(mode='list',length=length(data2A$species))

#create these vecctors
SD_vector <- as.vector((data2A$cv*data2A$mean)/100)
SD_vector

SD_total <- sd(data2A$mean, na.rm=TRUE)
SD_total

mean_vector <-as.vector(data2A$mean)
mean_vector

mean_total <-mean(data2A$mean)
mean_total

n_vector <-as.vector(data2A$n)
n_vector

n_total <-sum(data2A$n)
n_total


asymptotic_test2(k=(nrow(data2A)), n = n_vector,s = SD_vector, x = mean_vector,1)
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
one.vs.all <-vector(mode='list',length=nrow(data2A))

for (i in 1:(nrow(data2A)))
{
  n_tmp[[1]] <- n_vector[[i]]
  s_tmp[[1]] <- SD_vector[[i]]
  x_tmp[[1]] <- mean_vector[[i]]
  
  one.vs.all[[i]] <- asymptotic_test2(2, n = n_tmp,s = s_tmp, x = x_tmp,1)
}
#output list of test statistics and p-valukes
one.vs.all
