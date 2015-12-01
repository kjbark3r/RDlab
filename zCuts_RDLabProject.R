##DELETED CODE FROM RESEARCH DESIGN LAB PROJECT
##Things that functionally worked but weren't right to do

#create dataframe consisting of averages of bootstrapped data
##ACTUALLY I DON'T THINK I SHOULD DO THIS
##WORK WITH ENTIRE BOOTSTRAPPED MATRIX INSTEAD
mig.finaldata <- colMeans(mig.bootstrap.avgs, dim=1)
mig <- data.frame(mig.finaldata)
mig$mig.status <- "Migrant"
colnames(mig)[1] <- "FecalN"

#create dataframe consisting of averages of bootstrapped data
##ACTUALLY I DON'T THINK I SHOULD DO THIS
##WORK WITH ENTIRE BOOTSTRAPPED MATRIX INSTEAD
#res.finaldata <- colMeans(res.bootstrap.avgs, dim=1)
#res <- data.frame(res.finaldata)
#res$mig.status <- "Resident"
#colnames(res)[1] <- "FecalN"

#determine variance values to use in power analysis
###i don't think lm below was right - do anova, not regression
#  (lm <- summary(lm(FecalN ~ mig.status, data = fn)))
###below isn't right either, sigh...
#(aov <- summary(aov(FecalN ~ mig.status, data = fn)))

#fn <- bind_rows(mig, res)
#fn <- rbind(mig.bootstrap.avgs, res.bootstrap.avgs)
##close, but your data aren't delineated by migratory status
##cbind should add column
#mig.finaldata <- cbind(mig.bootstrap.avgs, "Migrant") #do this w tidy data
#res.finaldata <- cbind(res.bootstrap.avgs, "Resident") #ditto above

#power analysis 
##First, determine what my power is given my current sample size.
power.anova.test(groups = 2, n = 12, between.var = ???, within.var = ???,
                 sig.level = 0.05, power = 0.8)

#create vector of effect sizes

for (l in 0:0.5) {  #effect sizes
  reps <- 1000 #loop 1000x
  results <- matrix(NA, reps, 2) #store results in matrix (1000 rows, 2 cols - PowerxEffectsize)
  
  power.anova.test(groups = 2, n = 12, between.var = NULL, within.var = 0.1726,
                   sig.level = 0.1, power = 0.8)
}
