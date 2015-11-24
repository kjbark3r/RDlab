setwd("C:/Users/kjbark3r/Documents/GitHub/RDlab")

#read in data
data2014 <- read.csv("data2014.csv")

#call needed libraries
library(dplyr)

############################################################
# Separate dataframes and analyses for each migratory status  
############################################################

##########
#MIGRANTS - subset and bootstrap 
mig.data2014 <- filter(data2014, MigStatus == "Mig")
mig.bootdata <- sample(1:nrow(mig.data2014), 24, replace = T) 
(mig.newdata <- mig.data2014[mig.bootdata,])

##set up vector to store migrant means in
mig.finaldata <- NULL

##run loop 12 times, each time taking the average of 2 sequential data points
for(i in 1:12) {
  mig.finaldata[i] <- sample(mig.newdata$PctFN, 2)
  }
mig <- data.frame(mig.finaldata)

##calculate migrant mean and variance
mig.mean <- mean(mig$mig.finaldata)
mig.var <- var(mig$mig.finaldata)

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
res <- data.frame(res.finaldata)

##calculate resident mean and variance
res.mean <- mean(res$res.finaldata)
res.var <- var(res$res.finaldata)

############################################################
# Combine data and run power analysis  
############################################################

migres <- data.frame(c(mig, res))

#determine variance to use in power analysis
lm <- lm(mig.finaldata ~ res.finaldata, data = migres)
summary(lm)

anova(lm)


#power analysis 
##first, determine what my power is given my current sample size
##then, play with parameters
power.anova.test(groups = 2, n = 12, between.var = NULL, 
                 within.var = XXXXX)


############################################################
# Just playing around with data manipulation
# not for this project, just for future/general knowledge
############################################################
#trying some things I found on teh interwebz

##create data frame with two columns - mig FN and res FN
##in future do this and add sampling period as 3rd column
##so you can plot by sampling pd, look at temporal changes, etc
migres <- data.frame(c(mig, res))

##put all FN values from above into single vector
migres.df <- as.vector(c(migres$mig.finaldata, migres$res.finaldata))

