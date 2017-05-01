getwd()
report<- read.csv("300report.csv")
year13<-read.csv("year1.csv")
year12<-read.csv("year2.csv")
year11<-read.csv("year3(2011).csv")

newyear11<-na.omit(year11)
newyear12<-na.omit(year12)
newyear13<-na.omit(year13)
newyear11$Dual.Denom.3<-NULL
newyear11$Dual.Num.3<-NULL
newyear12$Dual.Denom.2<-NULL
newyear12$Dual.Num.2<-NULL
newyear13$Dual.Denom.2<-NULL
newyear13$Dual.Num.2<-NULL

## year 13

set.seed(1)
train13 = sample(1:nrow(newyear13),nrow(newyear13)*0.7)

library(gbm)
set.seed(1)
attach(newyear13)
boost.13 = gbm(DRate.1~.,newyear13[train13,],distribution = "gaussian", n.trees=4000,interaction.depth=5)

summary(boost.13)

yhat.boost=predict(boost.13,newdata=year13[-train13,],n.trees=4000)
year13.test=year13[-train13,"DRate.1"]
mean((yhat.boost-year13.test)^2)

## year 12

set.seed(1)
train12 = sample(1:nrow(newyear12),nrow(newyear12)*0.7)

library(gbm)
set.seed(1)
attach(newyear12)
boost.12 = gbm(DRate.2~.,newyear12[train12,],distribution = "gaussian", n.trees=4000,interaction.depth=5)

summary(boost.12)

par(mfrow=c(1,2))
plot(boost.12,i="Prog.Length")
plot(boost.12,i="School.Type")

## Year 11

set.seed(1)
train11 = sample(1:nrow(newyear11),nrow(newyear11)*0.7)

library(gbm)
set.seed(1)
attach(newyear11)
boost.11 = gbm(DRate.3~.,newyear11[train11,],distribution = "gaussian", n.trees=4000,interaction.depth=5)

summary(boost.11)

par(mfrow=c(1,2))
plot(boost.11,i="Prog.Length")
plot(boost.11,i="School.Type")

## first plot
par(mfrow=c(1,3))
summary(boost.13)
plot(boost.13,i="Prog.Length")
plot(boost.13,i="School.Type")