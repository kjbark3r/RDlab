######################################################
# Differences in resident and migrant elk nutrition
# Power analysis for Research Design Lab
# Kristin Barker - Dec 2015
######################################################

setwd("C:/Users/kjbark3r/Documents/GitHub/RDlab")
data2014 <- read.csv("data2014.csv")

#libraries
library(dplyr) #for bind_rows
library(tidyr) #for gather

#set number of bootstrap reps and sample size 
bootstrap.reps <- 1000   
n <- 12 #sample size

##############################################################
# Separate dataframes and bootstraps for each migratory status  
##############################################################

##########
#Migrants 
##########

#subset 2014 data
mig.data2014 <- filter(data2014, MigStatus == "Mig")

#create matrices to store bootstraps in
mig.bootstraps <- matrix(NA, bootstrap.reps, 24)
mig.bootstrap.avgs <- matrix(NA, bootstrap.reps, n) 

#bootstrap 2014 data to simulate 2015 data
for(j in 1:bootstrap.reps) {
  #pull 24 random samples with replacement
  mig.bootdata <- sample(1:nrow(mig.data2014), 24, replace = T) 
  #create matrix of bootstrapped data
  mig.newdata <- mig.data2014[mig.bootdata,]
  mig.bootstraps[j,] <- c(mig.newdata$PctFN)
}
  
#average 2 sequential data points at a time (w/o replacement)
##to sort of simulate averaging 2+ values per sampling period 
##save each run to a new row in bootstrap matrix
for(k in 1:bootstrap.reps) {
  for(i in 1:n) {
    mig.bootstrap.avgs[k,i] <- mean(sample(mig.bootstraps[k,], 2))
  }  
}

#make bootstrap matrix a dataframe for easier manipulation
#put all bootstrapped fecal N values in one column
#and create new column for migratory status (slightly hacky...)
mig.df <- as.data.frame(mig.bootstrap.avgs) %>%
  gather("MigStatus", "FecalN", 1:12) 
mig.df$MigStatus <- "Migrant"


##########
#RESIDENTS 
##########

#subset 2014 data 
res.data2014 <- filter(data2014, MigStatus == "Res")

#create matrices to store bootstraps in
res.bootstraps <- matrix(NA, bootstrap.reps, 24)
res.bootstrap.avgs <- matrix(NA, bootstrap.reps, n) 

##bootstrap 2014 data to simulate 2015 data
for(j in 1:bootstrap.reps) {
  ##pull 24 random samples with replacement
  res.bootdata <- sample(1:nrow(res.data2014), 24, replace = T) 
  ##create matrix of bootstrapped data
  res.newdata <- res.data2014[res.bootdata,]
  res.bootstraps[j,] <- c(res.newdata$PctFN)
}

#average 2 sequential data points at a time (w/o replacement)
##to sort of simulate averaging 2+ values per sampling period 
##save each run to a new row in bootstrap matrix
for(k in 1:bootstrap.reps) {
  for(i in 1:n) {
    res.bootstrap.avgs[k,i] <- mean(sample(res.bootstraps[k,], 2))
  }  
}

#make bootstrap matrix a dataframe for easier manipulation
#put all bootstrapped fecal N values in one column
#and create new column for migratory status (slightly hacky...)
res.df <- as.data.frame(res.bootstrap.avgs) %>%
  gather("MigStatus", "FecalN", 1:12) 
res.df$MigStatus <- "Resident"


############################################################
# Combine data and run power analysis  
############################################################

#combine migrant and resident data in tidy format 
all.boots <- rbind(mig.df, res.df)

#check out the data (for funzies)
boxplot(FecalN ~ MigStatus, all.boots, ylab="Fecal N (% dry matter)", 
        main="Adult Female Elk Nutrition")
summary(lm(FecalN ~ MigStatus, all.boots))

#Q1: Do I have 80% power to detect a 5% difference in nutrition?
power.t.test(n = n, delta = 0.05, sd = sd(all.boots$FecalN), 
             sig.level = 0.05, power = NULL)

#Q2: What % difference can I detect with 80% power? 
power.t.test(n = n, delta = NULL, sd = sd(all.boots$FecalN), 
             sig.level = 0.05, power = 0.8)

#Look at power ~ effect size
effect.sizes <- seq(0, 0.5, length.out = 100)
power.values <- sapply(effect.sizes, function (effect.sizes) 
  power.t.test(n = n, delta = effect.sizes, sd = sd(all.boots$FecalN), 
               sig.level = 0.05)$power)

plot(power.values ~ effect.sizes, ylab = "Power", xlab = "Effect Size",
    main = "Two-tailed t-test with alpha = 0.05", type = "l")