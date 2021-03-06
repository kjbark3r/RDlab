setwd("C:/Users/kjbark3r/Documents/GitHub/RDlab")

#data
data2014 <- read.csv("data2014.csv")

#libraries
library(dplyr)

############################################################
# Separate dataframes and analyses for each migratory status  
############################################################

##########
#MIGRANTS - subset and bootstrap 
mig.data2014 <- filter(data2014, MigStatus == "Mig")
mig.bootdata <- sample(1:nrow(mig.data2014), 24, replace = T) 
(mig.newdata <- mig.data2014[mig.bootdata,])

##set up vector to store migrant averages in
mig.finaldata <- NULL

##run loop 12 times, each time taking the average of 2 sequential data points
for(i in 1:12) {
  mig.finaldata[i] <- mean(sample(mig.newdata$PctFN, 2))
}

#create properly formatted dataframe of bootstrapped migrant data
mig <- data.frame(mig.finaldata)
mig$mig.status <- "Migrant"
colnames(mig)[1] <- "FecalN"


############
#RESIDENTS - subset and bootstrap 
res.data2014 <- filter(data2014, MigStatus == "Res")
res.bootdata <- sample(1:nrow(res.data2014), 24, replace = T) 
(res.newdata <- res.data2014[res.bootdata,])

##set up vector to store resident means in
res.finaldata <- NULL

##run loop 12 times, each time taking the average of 2 data points
for(i in 1:12) {
  res.finaldata[i] <- sample(res.newdata$PctFN, 2)
}

#create properly formatted dataframe of bootstrapped resident data
res <- data.frame(res.finaldata)
res$mig.status <- "Resident"
colnames(res)[1] <- "FecalN"
head(res)


############################################################
# Combine data and run power analysis  
############################################################

#combine migrant and resident data in tidy format 
fn <- bind_rows(mig, res)

#check out the data
boxplot(FecalN ~ mig.status, fn, ylab  = "Fecal N (% dry matter)", main = "Adult Female Elk Nutrition")

#determine variance values to use in power analysis
###i don't think lm below was right - do anova, not regression
#  (lm <- summary(lm(FecalN ~ mig.status, data = fn)))
(aov <- summary(aov(FecalN ~ mig.status, data = fn)))


#power analysis 
##First, determine what my power is given my current sample size.
power.anova.test(groups = 2, n = 12, between.var = 0.4312, within.var = 0.1726,
                 sig.level = 0.1, power = NULL)

##If not enough power to detect 5% effect size, 
##figure out what effect size I can detect (loop over effect sizes)


##initial thoughts about effect size loop
##need to play with this using hard-wired values 
##(i.e., vals pulled programmatically from aov - esp within.var)

#create vector of effect sizes

for (j in 0:0.5) {  #these should be effect sizes, NOT # times to loop
  reps <- 1000 #loop 1000x
  results <- matrix(NA, reps, 2) #store results in matrix (1000 rows, 2 cols)
  
  power.anova.test(groups = 2, n = 12, between.var = NULL, within.var = 0.1726,
                   sig.level = 0.1, power = 0.8)
}
