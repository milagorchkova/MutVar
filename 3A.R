install.packages("ggplot2")
install.packages("cvequality")
install.packages("data.table")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("tidyverse")
install.packages("knitr")
library(knitr)
library(ggplot2)
library(cvequality)
library(data.table)
library(dplyr)
library(gridExtra)
library(tidyverse)

setwd("C:/Users/User/Documents/Grade - Year 3/eeb397/csvs")
dir()
data3A <- read.csv("3A.csv")

names(data3A)[1]<-paste("species") 
names(data3A)[2]<-paste("leaves")


#each graph has its own scale
p1 <- ggplot(data=data3A, aes(leaves))+geom_density()+facet_wrap(~species,scales="free")

#each graph has same scale. adjust sets smoothing of the variation, also plots are coloured  in
p2 <- ggplot(data=data3A, aes(leaves))+geom_density(fill="blue",colour=NA,alpha=.2,adjust=3)+geom_line(stat="density",adjust=3)+facet_wrap(~species,scales="fixed")

#all variations overlaid 
p3 <- ggplot(data=data3A, aes(leaves,colour=species))+geom_density(adjust=3) 

#all overlapping variations with ALL VARIATION superimposed. all variation is smoothed like the rest
p4 <- ggplot(data=data3A)+geom_density(aes(leaves,colour=species),adjust=3) + geom_density(aes(leaves),size=2,adjust=3) + ggtitle(str_wrap("Effect (New Leaves) of Ant Mutualists on H. physophora (3A)",width=42))
p4.1 <- ggplot(data=data3A)+geom_density(aes((fruits/flowers),colour=species),adjust=3) + geom_density(aes((fruits/flowers)),size=2,adjust=3)

bp1 <- ggplot(data=data3A)+geom_boxplot(aes(leaves,species)) +geom_boxplot(aes(leaves),size=2,alpha=0.2)+ylab('SPECIES')
#display plots
p1
p2
p3
p4
p4.1
#p4.1 not used due to non-normal distribution

bp1

#find means and SD of each data
summary_table <- data3A %>% group_by(species) %>% summarize(n=n(), mean_leaves <- mean(leaves, na.rm=TRUE), sd_leaves <- sd(leaves, na.rm=TRUE), se_error <- sd_leaves/sqrt(n))
view(summary_table)

species_list <- unique(data3A$species)
species_list

i = 1

#initialize empty vectors to do CV equality tests
SD_vector <-vector(mode='numeric',length=length(unique(data3A$species)))
mean_vector <-vector(mode='numeric',length=length(unique(data3A$species)))
n_vector <-vector(mode='numeric',length=length(unique(data3A$species)))

#make a vectors for SD, mean, and n of each species group (to calculate one against all variances)
for (i in 1:(length(unique(data3A$species))))
{
  #vector with standard deviation values for each species
  tmp<-subset(data3A,species==species_list[[i]])
  
  SD_vector[[i]] <-sd(tmp$leaves)
  
}

for (i in 1:(length(unique(data3A$species))))
{
  #vector with mean values for each species
  tmp<-subset(data3A,species==species_list[[i]])
  mean_vector[[i]] <-mean(tmp$leaves)
  
}

for (i in 1:(length(unique(data3A$species))))
{
  #vector with n-values for each group. i.e. how many values in each species group
  tmp<-subset(data3A,species==species_list[[i]])
  n_vector[[i]] <-length(tmp$leaves)
  
}

#output vectors to test
n_vector
SD_vector
mean_vector


#SD and mean of ALL values in dataframe (sample population)
SD_total <- sd(data3A$leaves, na.rm=FALSE)
mean_total <-mean(data3A$leaves)
n_total <-length(data3A$leaves)

SD_total
mean_total
n_total


#x= measurements, y = grouping variables, seed= keeps it consistent, any integer
#this gives us a test statistic to compare how similar variance is between
#comparison of CV between every group in the dataframe. (intraspecfic to intraspecific)
x_vector<-as.vector(data3A$leaves)
grouping_vector<-as.vector(data3A$species)

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
one.v.all <-vector(mode='list',length=length(unique(data3A$species)))

for (i in 1:(length(unique(data3A$species))))
{
  n_tmp[[1]] <- n_vector[[i]]
  s_tmp[[1]] <- SD_vector[[i]]
  x_tmp[[1]] <- mean_vector[[i]]
  
  one.v.all[[i]] <- asymptotic_test2(2, n = n_tmp,s = s_tmp, x = x_tmp,1)
}
#output list of test statistics and p-valukes
one.v.all

#change test statistic outputs into two vectors, of test stats and p-values
one.v.all <- unlist(one.v.all)
str(one.v.all)

test_statistics <- vector(mode='numeric',length=length(unique(data3A$species)))
p_values <- vector(mode='numeric',length=length(unique(data3A$species)))


for  (i in 1:(length(unique(data3A$species))))
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
CV_vector <- vector(mode='numeric',length=length(unique(data3A$species)))
CV_vector <- (SD_vector/mean_vector)*100

CV_total <- (SD_total/mean_total)*100
CV_total

CV_greater <- CV_vector > CV_total
CV_greater

#make dataframe to summarize results
cv_results <-data.frame(test_statistics,p_values,p_over_five,CV_greater)         
view(cv_results)        

#then, we have to merge/interpret results. we only care about SD_intra>SD_total if its SIGNIFICANT
#, so we are looking for number that are FALSE-TRUE, TRUE-(anything)