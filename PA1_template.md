# Reproducible Research: Peer Assessment 1


In this report, we make use of the data drawn from the personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data was  collected for a period of two months during October and November. It includes the number of steps taken in 5 minute intervals each day. We first start by describing the variation of the averaged steps per day and per time interval, then we study the impact of the missing day measurments' in our analysis. Finally, we compare the activity of the participants during weekdays and weekend.

## Loading and preprocessing the data


```r
activity<-read.csv("activity.csv", sep=",", header=TRUE)
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```


## What is mean total number of steps taken per day?


```r
##plotting the histogram
png('hist1.png')
hist(with(activity,tapply(steps,date,sum)), main="Distribution of total number of activities per day",xlab="interval of steps", col="purple")
##computing the mean and the median of the sum of the total nmber of steps per ##day
mean<-as.integer(mean(with(activity,tapply(steps, date,sum)),na.rm=TRUE))
median<-median(with(activity,tapply(steps,date,sum)),na.rm=TRUE,round=2)
##Plotting the median and the mean lines
abline(v=mean,col="red", lwd="2")
abline(v=median, col="blue",lwd="2")
dev.off()
```

We can see the histogram, illustrating the variation of the total number of steps taken per day, in "hist1.png". The mean is equal to 10766 and the median to 10765. Both values are close, meaning that there is roughly no
"outsider" values. That is why we see only one vertical line in the histogram
(the blue one).

## What is the average daily activity pattern?


```r
##Computting the mean of number of steps per intervals along the days
mean_interval<-with(activity,tapply(steps,interval,mean,na.rm=TRUE))
##Plotting the graph of the variation of the mean_interval
png('fig1.png')
plot(unique(activity$interval),mean_interval,type="l",lwd="2",col="blue")
##Computing the maximum of mean_interval
idx_max<-which(mean_interval==max(mean_interval))
max<-unique(activity$interval)[idx_max]
##Plotting the line corresponding to the max
axis(1, at =835,col= "red")
abline(v=max,col="red",lwd="2")
dev.off()
```

The variation of the average daily activity pattern is shown in 
"fig1.png". The maximum of this value is reached at 835 minutes. This
means that the maximum of activity states around "1h55 pm" (if we make the
assumption that 0 mn corresponds to midnight).

## Imputing missing values


```r
##identfying the missing measurments days
idx_na<-with(activity,tapply(is.na(steps), date,sum))
## Computing the number of missing measurements days
date_na<-sum(!idx_na==0)
##filling the missing values with the mean_interval
activity$steps[is.na(activity$steps)]<-rep(mean_interval,date_na)
##Plotting the histogram with the new values
png('hist2.png')
hist(with(activity,tapply(steps,date,sum)), main="Distribution of total number of activities per day",xlab="interval of steps", col="purple")
##Computing the "new" mean and median
mean<-mean(with(activity,tapply(steps, date,sum)),na.rm=TRUE)
median<-median(with(activity,tapply(steps,date,sum)),na.rm=TRUE)
##Plotting the line corrsponding to these values
abline(v=mean,col="blue",lwd="2")
abline(v=median,col="red",lwd="2")
dev.off()
```

After imputig the missing value to the data, the variation of the total number
of steps does not change that much, comparing to the case when we have missing values. This is due to our strategy, as we filled the missing value with the
averaged steps per interval. We have however more values in the mean's inteval (10000-15000) which is normal since we add more variables.
The new variation is illustrated in "hist2.png". In fact, the mean is now equal to 1.0766189\times 10^{4} and the median to 1.0766189\times 10^{4}. We can see that both values are roughly the same.

## Are there differences in activity patterns between weekdays and weekends?


```r
library(plyr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
##Creating a factor of two levels "weekdays" and "weekend"
activity$date<-as.Date(activity$date)
u<-sapply(activity$date,weekdays)
u[!(u=="samedi") & !(u=="dimanche")]<-"weekdays"
u[(u=="samedi")|(u=="dimanche")]<-"weekend"
activity_df<-tbl_df(activity)
activity_df<-mutate(activity_df,"day"=as.factor(u))
##Plotting the graph showing the mean steps per weekdays/weekends
library(ggplot2)
library(ggthemes)
data<-ddply(activity_df,.(interval,day),summarise,mean=mean(steps))
g<-ggplot(data,aes(interval,mean))+geom_line(col="blue")
g<-g+facet_wrap( ~ day,nrow=2,ncol=1)
g<-g+theme_bw(base_size = 10)
g<-g+xlab("interval")+ylab("Number of steps")
ggsave("fig2.png",g,device="png")
```

```
## Saving 7 x 3 in image
```

The difference between activity in both weekdays and weekend is shown in "fig2.png". We can see that both graphics illustrate the same behaviour:
there is a low activity in the first periood of time (in the morning), it increases thereafter to reach its maximum at 2 pm and then decreases again in the evening and in the night.
