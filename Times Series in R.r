setwd("C:\\Users\\Ashish Arora\\Desktop\\Time series Case Study")

#Loading the data file
mydata<-read.csv("data.csv")
View(mydata)

myts   <- ts(mydata[,-c(1,2)], start=c(1996, 1), end=c(2005, 4), frequency=4) 
plot(myts)

#After looking at the graph it is observed that Rest.of.World is a patternless data rest all follow a multiplicative form
#so subsetting the data separately.


myts_ireland   <- ts(mydata$Ireland, start=c(1996, 1), end=c(2005, 4), frequency=4) 
myts_Other.EU   <- ts(mydata$Other.EU, start=c(1996, 1), end=c(2005, 4), frequency=4) 
myts_Rest.of.Europe   <- ts(mydata$Rest.of.Europe, start=c(1996, 1), end=c(2005, 4), frequency=4) 
myts_Total  <- ts(mydata$Total, start=c(1996, 1), end=c(2005, 4), frequency=4) 

myts_Rest.of..World  <- ts(mydata$Rest.of..World, start=c(1996, 1), end=c(2005, 4), frequency=4) 

# plot series
plot(myts_ireland)
plot(myts_Other.EU)
plot(myts_Rest.of.Europe)
plot(myts_Total)
plot(myts_Rest.of..World)#this is an non-seasonal patternless data so we can implement an exponential smoothening techniques


fit<- decompose(myts_ireland, type = c("multiplicative"))
ls(fit)
forecast(fit$x, 4,level=c(90,95))
accuracy(forecast(fit$x, 4))#The MAPE value of 2.428393 for Ireland predicted values
#FORECASTING AND PLOTTING VALUES FOR NEXT QUATER(2006)
final<-forecast(fit$x, 4,level=c(90,95))
plot(final)

fit1<- decompose(myts_Other.EU, type = c("multiplicative"))
forecast(fit1$x, 4,level=c(90,95))
accuracy(forecast(fit1$x, 4))#The MAPE value of 2.279375 for Other EU predicted values
final1<-forecast(fit1$x, 4,level=c(90,95))
plot(final1)

fit2<- decompose(myts_Rest.of.Europe, type = c("multiplicative"))
forecast(fit2$x, 4,level=c(90,95))
accuracy(forecast(fit2$x, 4))#The MAPE value of 2.871732 for Rest of Europe predicted values
final2<-forecast(fit1$x, 4,level=c(90,95))
plot(final2)

fit3<- decompose(myts_Total, type = c("multiplicative"))
forecast(fit3$x, 4,level=c(90,95))
accuracy(forecast(fit3$x, 4))#The MAPE value of 1.948733 for Total predicted values
final3<-forecast(fit1$x, 4,level=c(90,95))
plot(final3)
####NoW for Rest of world data####
fit4.1 <- HoltWinters(myts_Rest.of..World, beta=FALSE, gamma=FALSE)
ls(fit4.1)
require(forecast)
accuracy(fit4.1$fitted, myts_Rest.of..World)

# double exponential - models level and trend
fit4.2 <- HoltWinters(myts_Rest.of..World, gamma=FALSE)
accuracy(fit4.2$fitted, myts_Rest.of..World)
# triple exponential - models level, trend, and seasonal components
fit4.3 <- HoltWinters(myts_Rest.of..World)
accuracy(fit4.3$fitted, myts_Rest.of..World)


#ETS- Exponential method
fit5<-ets(myts_Rest.of..World)
accuracy(fit5$fitted, myts_Rest.of..World)
summary(fit5)
final5<-forecast(fit5, 4 ,level = c(90,95))
plot(final5)
#Thus for rest of world the lowest MAPE is for ETS method thus we use that to predict the next quaters' values.

##All The final graphs 
attach(mtcars)
par(mfrow=c(3,2)) 
plot(final)
plot(final1)
plot(final2)
plot(final3)
plot(final5)

#The predicted results are stored in final,final1, final2,final3, final5