power.anova.test(groups = 4, n = 5, between.var = 1, within.var = 3)
# Power = 0.3535594

power.anova.test(groups = 4, between.var = 1, within.var = 3,
                 power = .80)
# n = 11.92613

## Assume we have prior knowledge of the group means:
groupmeans <- c(120, 130, 140, 150)
power.anova.test(groups = length(groupmeans),
                 between.var = var(groupmeans),
                 within.var = 500, power = .90)

elk<-c(9,14,15,142,15,11,7,6,15,10,11,10,7,21,18,16,20,9,12,14,0,0,0,0,0)

for( j in c(50,100,150,200,250,300,350, 400 ) ){
  nReps<-1000
  results<-matrix(NA, nReps, 2)
  for( i in 1:nReps ){
    z<-sample(elk, size=j, replace=T)
    results[i,]<-c(mean(z), sd(z)/sqrt(length(z)))
  }
  mean(results[,1])
  mean(results[,2])
  sd(results[,1])
  cv=mean(results[,2])/mean(results[,1])
  if( cv <= 0.1 ){
    cat( "Sample size = ", j, "cv = ", cv, "\n" )
    break
  }
}


cv<-1
j<-0
i<-0
while( cv > 0.1 ){
  j<-j+50
  i+1
  nReps<-1000
  results<-matrix(NA, nReps, 2)
  for( i in 1:nReps ){
    z<-sample(elk, size=j, replace=T)
    results[i,]<-c(mean(z), sd(z)/sqrt(length(z)))
  }
  mean(results[,1])
  mean(results[,2])
  sd(results[,1])
  cv=mean(results[,2])/mean(results[,1])
  if( i > 500 ){
    break
  }
}
cat( "Sample size = ", j, "cv = ", cv, "\n" )