data(Auto)
auto<-na.omit(Auto)
attach(auto)
#a) Which of the predictors are quantitative, and which...
lapply(auto,class)
origin<-as.factor(origin)
#b
#columns qualitative
cols.qlt = names(auto) %in% c("name", "origin")
#apply range in all columns except the qualitative
lapply(auto[,!cols.qlt],range)
cols.qlt = names(auto) %in% c("name", "origin")

#c
#What is the mean and standard deviation of each quantitative predictor?

lapply(auto[, !cols.qlt], function(x){ c('mean'=mean(x), 'sd'=sd(x))})

#d
Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each predictor in the subset of the data that remains?

lapply(auto[-(10:85), !cols.qlt], function(x){ c('mean'=mean(x), 'sd'=sd(x))})
#e) Using the full data set, investigate the predictors graphically, using scatterplots or other tools of your choice. Create some plots highlighting the relationships among the predictors. Comment on your findings.

pairs(auto[, !cols.qlt])

par(mfrow=c(2,2))
ggplot(auto,aes(displacement,acceleration))+geom_point(shape=1)
ggplot(auto,aes(weight,horsepower))+geom_point(shape=1)
ggplot(auto,aes(cylinders,mpg))+geom_point(shape=1)
ggplot(auto,aes(weight,mpg))+geom_point(shape=1)
#f
others.variables = !(names(auto) %in% "mpg" | cols.qlt)
par(mfrow=c(3,2))
for(i in names(auto[, others.variables])){
  plot(mpg, get(i), ylab=i)
}
