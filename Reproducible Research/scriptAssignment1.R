library(plyr)
library(ggplot2)
library(scales)
library(timeDate)
library(Rmisc)

## We start by reading the csv file and saving it as the main data frame
mydata<-read.csv("activity.csv")

##stepsPerDay is a data frame that contains the sum of steps per each day in mydata
stepsPerDay<-ddply(mydata, .(date), summarise, stepsDay = sum(steps, na.rm = TRUE))

##We have to give date format to the date column of strings
stepsPerDay$date<-as.Date(stepsPerDay$date)

##The histogram plot is created with dates in x axis and steps count on y axis
##plot2<-qplot( data=stepsPerDay, x = date, y = stepsDay, geom = "histogram", stat="identity")

plot2<-ggplot(stepsPerDay, aes(x=date, y=stepsDay)) + geom_bar(stat="identity")

##Showing the histogram on screen, formating x axis to months
plot2 + scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"))

stepsMeanPerDay<-ddply(mydata, "date", summarise, stepsDay = mean(steps, na.rm = TRUE))

stepsMedianPerDay<-ddply(mydata, "date", summarise, stepsDay = median(steps, na.rm = TRUE))



##We have to give date format to the date column of strings
stepsMeanPerDay$date<-as.Date(stepsMeanPerDay$date)

##Plotting the time series of the average number of steps
plot3<-qplot( data=stepsMeanPerDay, x = date, y = stepsDay,stat="identity")
plot3 + geom_line() 



mydata5<-mydata
mydata5$interval<-as.factor(mydata5$interval)
maxStepsPerInterval<-ddply(mydata5, "interval", summarise, maxAvSteps = mean(steps, na.rm = TRUE))
maxStepsPerInterval[max(maxStepsPerInterval$maxAvSteps),]



library(mice)
md.pattern(mydata)


library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(mydata), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


tempData <- mice(mydata,m=5,maxit=50,meth='pmm',seed=500)
completedData <- complete(tempData,1)


##stepsPerDayMiss is a data frame that contains the sum of steps per each day in mydata, after missing values are imputed
stepsPerDayMiss<-ddply(mydata, .(date), summarise, stepsDayMiss = sum(steps, na.rm = TRUE))

##We have to give date format to the date column of strings
stepsPerDayMiss$date<-as.Date(stepsPerDayMiss$date)

##The histogram plot is created with dates in x axis and steps count on y axis
plot7<-qplot( data=stepsPerDayMiss, x = date, y = stepsDayMiss, geom = "histogram", stat="identity")

##Showing the histogram on screen, formating x axis to months
plot7 + scale_x_date(breaks = date_breaks("months"),labels = date_format("%b"))


weekdayTRUEweekendFALSE<-isWeekday(mydata5$date)
mydata8<-cbind(mydata5,weekdayTRUEweekendFALSE)

subsetWeekdays<-mydata8[weekdayTRUEweekendFALSE=="TRUE",]
subsetWeekends<-mydata8[weekdayTRUEweekendFALSE=="FALSE",]

AvStepsPerIntervalWeekdays<-ddply(subsetWeekdays, "interval", summarise, AvStepsWeekdays = mean(steps, na.rm = TRUE))
AvStepsPerIntervalWeekends<-ddply(subsetWeekends, "interval", summarise, AvStepsWeekend = mean(steps, na.rm = TRUE))

dataFrame7<-cbind(AvStepsPerIntervalWeekdays,AvStepsPerIntervalWeekends$AvStepsWeekends)


plot8Weekdays<-ggplot(dataFrame7, aes(x=interval, y=AvStepsWeekdays)) + geom_point()+scale_x_discrete(breaks=c(0, 500, 1000, 1500, 2000,2355))
plot8Weekends<-ggplot(dataFrame7, aes(x=interval, y=AvStepsPerIntervalWeekends$AvStepsWeekends)) + geom_point()+scale_x_discrete(breaks=c(0, 500, 1000, 1500, 2000,2355))
multiplot(plot8Weekdays,plot8Weekends,cols=2)
