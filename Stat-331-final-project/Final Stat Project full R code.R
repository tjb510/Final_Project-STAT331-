#Final Project R Code

#importing data
flights <- read.csv("~/Downloads/flights.csv")
View(flights)
options(scipen = 999)

#splitting data
numtrain<-ceiling(.8*3000)
numtrain
set.seed(333)
train_ind<-sample(3000, numtrain)
#training data
tdata<-flights[train_ind, ]
#validation data (for testing)
vdata<-flights[-train_ind, ]
set.seed(NULL)
View(tdata)

#changing data so all positive and factored categorical variables
tdata$pos_dep_delay<-tdata$dep_delay+23
tdata$pos_arr_delay <- tdata$arr_delay +72
tdata$carrier <- factor(tdata$carrier)
tdata$origin <- factor(tdata$origin)

#Data visualization
#matrix scatter plots, broken up so plots are more clear
mat_sct = pairs(tdata[,c('dep_delay','sched_dep_time','month','arr_delay')])
mat_sct = pairs(tdata[,c('dep_delay','air_time','distance', 'hour')])

#correlation matrix
cor(tdata[,c('dep_delay','sched_dep_time','month','arr_delay','air_time','distance', 
             'hour')])
plot(tdata[,c('dep_delay','sched_dep_time','month','arr_delay','air_time','distance', 
              'hour')])
#finding mean values of departure delay based on carrier
carrier.dl<-subset(tdata,carrier=='DL')
carrier.ua<-subset(tdata,carrier=='UA')
carrier.aa<-subset(tdata,carrier=='AA')
mean(carrier.dl$dep_delay)
mean(carrier.ua$dep_delay)
mean(carrier.aa$dep_delay)

#finding mean values of departure delay based on Origin
origin.EWR<-subset(tdata,origin=='EWR')
origin.JFK<-subset(tdata,origin=='JFK')
origin.LGA<-subset(tdata,origin=='LGA')
mean(origin.EWR$dep_delay)
mean(origin.JFK$dep_delay)
mean(origin.LGA$dep_delay)

#fitting full linear model
tdata$carrier<-factor(tdata$carrier)
tdata$origin<-factor(tdata$origin)
flight.full<-lm(pos_dep_delay~month+sched_dep_time+pos_arr_delay+carrier+
                  origin+air_time+distance+hour,data=tdata)
summary(flight.full)

#interaction plot between two categorical variables
with(tdata,interaction.plot(carrier,origin,dep_delay,type='b',col=rainbow(2),pch=1:2))
with(tdata,interaction.plot(origin,carrier,dep_delay,type='b',col=rainbow(2),pch=1:2))

#getting residual plots of all vars vs departure delay
residuals=residuals(flight.full)
fitted=fitted(flight.full)
par(mfrow=c(2,2))

plot(residuals~fitted, data=tdata,main='residuals vs fitted')
plot(residuals~sched_dep_time,data=tdata,main='residuals vs scheduled departure delay (min)')
plot(residuals~arr_delay,data=tdata,main='residuals vs arrival delay (min)')
plot(residuals~factor(carrier),data=tdata,main='residuals vs carrier')
plot(residuals~factor(origin),data=tdata,main='residuals vs origin')
plot(residuals~air_time,data=tdata,main='residuals vs air time (min)')
plot(residuals~distance,data=tdata,main='residuals vs distance (miles)')
plot(residuals~hour,data=tdata,main='residuals vs hour')

###full model with transformed Y variable
flight.trans<-lm(log(pos_dep_delay) ~ month + pos_arr_delay + origin + 
                   carrier + air_time + distance, data = tdata)
summary(flight.trans)
#updated LINE assumption plots with transformed Y variable
plot(flight.trans)

#box cox plot
library(MASS)
boxcox(pos_dep_delay~month + pos_arr_delay + origin + 
         carrier + air_time + distance, data=tdata, plotit=TRUE, lambda=seq(-1,.5,by=0.01))

#
##
###
#outliers
###
##
#
par(mfrow=c(2,2))
#studentized deleted residuals
t_i=rstudent(final.model) 
#calculating std del residuals and plotting
plot(abs(t_i), ylab='Studentized deleted residuals')
identify(abs(t_i))
#point number 2060 has a huge negative residual
tcrit<-qt(1-.1/(2*n),df=n-k-2);tcrit
#point number 2060 has an outlying value for studentized deleted residuals but its not significant

#leverages
h_i=hatvalues(final.model); 
# plotting leverages
plot(h_i, ylab='Leverage')
k=5
n=nrow(tdata)
abline(h=2*(k+1)/n)

#cooks distance
D_i=cooks.distance(final.model) 
plot(D_i, ylab='Cooks Distance')
abline(h=.5)

#DFFITS
DFFITS_i=dffits(final.model) #DFFITS
plot(abs(DFFITS_i),ylab='DFFITS')
abline(h=2*sqrt(k+1)/n)
#getting residual plots of sig vars vs transformed departure delay
residuals=residuals(flight.trans)
fitted=fitted(flight.trans)
par(mfrow=c(2,2))

plot(residuals~fitted, data=tdata,main='residuals vs fitted')
plot(residuals~sched_dep_time,data=tdata,main='residuals vs scheduled departure delay (min)')
plot(residuals~arr_delay,data=tdata,main='residuals vs arrival delay (min)')
plot(residuals~factor(carrier),data=tdata,main='residuals vs carrier')
plot(residuals~factor(origin),data=tdata,main='residuals vs origin')
plot(residuals~air_time,data=tdata,main='residuals vs air time (min)')
plot(residuals~distance,data=tdata,main='residuals vs distance (miles)')
plot(residuals~hour,data=tdata,main='residuals vs hour')

#
##
###
####
#Best subsets

all=lm(pos_dep_delay~.,data=tdata[,-c(1,4,5)])
library(leaps)
best.sub=summary(a<-regsubsets(formula(all), data=tdata[,-1],method="exhaustive",nbest=2, nvmax=10))
best.sub


#Specify n and m
n = nrow(tdata) #sample size
m = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10) #num predictors in each model *SEE REMARK BELOW

#Compute and Store Quantities of interest (SSE,s,Rsq,AIC,BIC,Cp)
SSE = best.sub$rss #SSE 
s = sqrt(SSE/(n-m-1)) #s
Rsq = best.sub$rsq #R-squared 
Rsq.adj = best.sub$adjr2 #R-squared adjusted
BIC = n*log(SSE/n) + (m+1)*log(n)#BIC
AIC = n*log(SSE/n) + 2*(m+1) #AIC
Cp = best.sub$cp #Mallow's Cp

#Summary Table of Best Subsets
bs=data.frame(best.sub$outmat, Pars=m+1, Cp, AIC, BIC, Rsq, Rsq.adj, s)
View(bs)

par(mar=c(4,4,4,4))
par(mfrow=c(1,1))
plot(m, BIC, xlab="Number of Predictors", ylab='BIC values', main = "BIC vs Number of Predictors")
abline(1,1,lty=2)
plot(m,Rsq.adj,xlab='Number of Predictors',ylab='Adjusted Rsquared',
     main='Adjusted Rsquared vs Number of Predictors')

#equation for lowest bic model
#making carrier UA a binary variable
tdata$carrierUA <- ifelse(tdata$carrier =='UA', 1,0)
flight.lowbic<-lm(log(pos_dep_delay)~month+sched_dep_time+carrierUA+air_time+distance+pos_arr_delay,data=tdata)
summary(flight.lowbic)

#checking multicollinearity
library(car)
vif(flight.lowbic)
#high VIF, abt 72 for airtime and distance

#eliminating multicolinearity
#remove either airtime or distance
flight.noairtime<-lm(log(pos_dep_delay)~month+sched_dep_time+carrierUA+distance+pos_arr_delay,data=tdata)
summary(flight.noairtime)
flight.nodistance<-lm(log(pos_dep_delay)~month+sched_dep_time+carrierUA+air_time+pos_arr_delay,data=tdata)
summary(flight.nodistance)
#removing distance results in a higher r squared for the model

library(car)
vif(flight.nodistance)
#VIF's are all around 1 now, no multicolinnearity

#investigate interaction between carrierUA and airtime
flight.int<-lm(log(pos_dep_delay)~month+sched_dep_time+carrierUA+air_time+pos_arr_delay+air_time:carrierUA,data=tdata)
summary(flight.int)
#investigate interaction between carrierUA and airtime
anova(flight.nodistance,flight.int)
#interaction between carrierua and airtime is not significant pvalue=.07235

#interaction plot

#
##
###
####
#####this is the final model we settled on
final.model<-lm(log(pos_dep_delay)~month+sched_dep_time+carrierUA+air_time+
                  pos_arr_delay,data=tdata)
summary(final.model)
anova(final.model)

#####
####
###
##
#
#Overall F test in summary for final model
summary(final.model)
#creating a reduced model for partial F-test
model_reduced = lm(log(pos_dep_delay) ~ distance, data=tdata)
#conducting partial F-Test
anova(model_reduced, final.model)

#
##
###
#confidence interval for mean values and non UA carrier
mean.nonUA<-predict(final.model,data.frame(month=mean(tdata$month),
                                           sched_dep_time=mean(tdata$sched_dep_time),carrierUA=0,
                                           air_time=mean(tdata$air_time),pos_arr_delay=mean(tdata$pos_arr_delay)),level=.95,interval='confidence')
#back transforming to get value
exp(mean.nonUA)-23

#confidence interval for mean values and UA carrier
mean.UA<-predict(final.model,data.frame(month=mean(tdata$month),
                                        sched_dep_time=mean(tdata$sched_dep_time),carrierUA=1,
                                        air_time=mean(tdata$air_time),pos_arr_delay=mean(tdata$pos_arr_delay)),level=.95,interval='confidence')
#confidence intervals
exp(mean.nonUA)-23
exp(mean.UA)-23

#prediction interval for non ua carrier
pred.nonUA<-predict(final.model,data.frame(month=mean(tdata$month),
                                           sched_dep_time=mean(tdata$sched_dep_time),carrierUA=0,
                                           air_time=mean(tdata$air_time),pos_arr_delay=mean(tdata$pos_arr_delay)),
                    level=.95,interval='prediction')
#back transforming to get value
exp(pred.nonUA)-23

#prediction interval for UA carrier
pred.UA<-predict(final.model,data.frame(month=mean(tdata$month),
                                        sched_dep_time=mean(tdata$sched_dep_time),carrierUA=1,
                                        air_time=mean(tdata$air_time),pos_arr_delay=mean(tdata$pos_arr_delay)),
                 level=.95,interval='prediction')
#prediction intervals
exp(pred.UA)-23
exp(pred.nonUA)-23

# Validation

final.model <- lm(log(pos_dep_delay)~ month + sched_dep_time + carrierUA +
                    air_time + pos_arr_delay , data = tdata)
predicted <- predict(final.model, tdata)
# shift departure delay in test data to start at 1
min(vdata$dep_delay)
actual <- log(vdata$dep_delay + 14) 
MSPE = mean(predicted - actual)^2 ; MSPE
anova(final.model)

# Press and R^2 Pred
e_i = resid(final.model)
h_i = hatvalues(final.model)
SST = sum((anova(final.model))$'Sum Sq')
#calculating press and pred rsquared
PRESS = sum( (e_i/(1-h_i))^2 ) ; PRESS
Pred.Rsq = 1-PRESS/SST ; Pred.Rsq

#to compare values for press and adjrsquared
anova(final.model)
Anova(final.model)

#regression based on all the data

flights$pos_dep_delay<-flights$dep_delay+23
flights$pos_arr_delay<-flights$arr_delay+72
flights$carrierUA <- ifelse(flights$carrier =='UA', 1,0)

full.model<-lm(log(pos_dep_delay)~month+sched_dep_time+carrierUA+air_time+
                 pos_arr_delay,data=tdata)
summary(full.model)
anova(full.model)

