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
data1A <- read.csv("1A.csv")
names(data1A)[1]<-paste("species") 

str(data1A)

#each graph has its own scale
p1 <- ggplot(data=data1A, aes(time))+geom_density()+facet_wrap(~species,scales="free")

#each graph has same scale. adjust sets smoothing of the variation, also plots are coloured  in
p2 <- ggplot(data=data1A, aes(time))+geom_density(fill="blue",colour=NA,alpha=.2,adjust=3)+geom_line(stat="density",adjust=3)+facet_wrap(~species,scales="fixed")

#all variations overlaid 
p3 <- ggplot(data=data1A, aes(time,colour=species))+geom_density(adjust=3) 

#all overlapping variations with ALL VARIATION superimposed. all variation is not smoothed like the rest
p4 <- ggplot(data=data1A)+geom_density(aes(time,colour=species),adjust=3) + geom_density(aes(time),size=2,adjust=3)

bp1 <- ggplot(data=data1A)+geom_boxplot(aes(time,species)) +geom_boxplot(aes(time),size=2,alpha=0.2)+ylab('SPECIES')
#display plots
p1
p2
p3
p4
bp1

#find means and SD of each data
summary_table <- data1A %>% group_by(species) %>% summarize(n=n(), mean_time <- mean(time, na.rm=TRUE), sd_time <- sd(time, na.rm=TRUE), se_error <- sd_time/sqrt(n))
view(summary_table)

species_list <- unique(data1A$species)
species_list

i = 1

#initialize empty vectors to do CV equality tests
SD_vector <-vector(mode='list',length=length(unique(data1A$species)))
mean_vector <-vector(mode='list',length=length(unique(data1A$species)))
n_vector <-vector(mode='list',length=length(unique(data1A$species)))

#make a vectors for SD, mean, and n of each species group (to calculate one against all variances)
for (i in 1:(length(unique(data1A$species))))
     {
        #vector with standard deviation values for each species
        tmp<-subset(data1A,species==species_list[[i]])
        
        SD_vector[[i]] <-sd(tmp$time)
     
}

for (i in 1:(length(unique(data1A$species))))
{
  #vector with mean values for each species
  tmp<-subset(data1A,species==species_list[[i]])
  mean_vector[[i]] <-mean(tmp$time)
  
}

for (i in 1:(length(unique(data1A$species))))
{
  #vector with n-values for each group. i.e. how many values in each species group
  tmp<-subset(data1A,species==species_list[[i]])
  n_vector[[i]] <-length(tmp$time)
  
}

#output vectors to test
n_vector
SD_vector
mean_vector


#SD and mean of ALL values in dataframe (sample population)
SD_total <- sd(data1A$time, na.rm=TRUE)
mean_total <-mean(data1A$time)
n_total <-length(data1A$time)

SD_total
mean_total
n_total


#x= measurements, y = grouping variables, seed= keeps it consistent, any integer
#this gives us a test statistic to compare how similar variance is between
  #comparison of CV between every group in the dataframe. (intraspecfic to intraspecific)
x_vector<-as.vector(data1A$time)
grouping_vector<-as.vector(data1A$species)

result = asymptotic_test(x_vector, grouping_vector, 1)
str(result)

#create vectors needed for test of equality: THIS IS WHEN ONLY SUMMARY STATISTICS AVAILABLE
  #OR to compare "one against all" (i.e. intraspecific vs interspecific; tested one by one)
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
one.v.all <-vector(mode='list',length=length(unique(data1A$species)))

for (i in 1:(length(unique(data1A$species))))
{
  n_tmp[[1]] <- n_vector[[i]]
  s_tmp[[1]] <- SD_vector[[i]]
  x_tmp[[1]] <- mean_vector[[i]]
  
  one.v.all[[i]] <- asymptotic_test2(2, n = n_tmp,s = s_tmp, x = x_tmp,1)
}
#output list of test statistics and p-valukes
one.v.all
