---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




## Loading and preprocessing the data


```r
zipFile<-"activity.zip"
csvFile<-"activity.csv"

if(!file.exists(csvFile)){
  unzip("activity.zip")
}

activity.monitoring.data<-read.csv(csvFile)
```

## What is mean total number of steps taken per day?

### Calculate the total number of steps taken per day.


```r
activity.monitoring.data.non.nan<-subset(activity.monitoring.data,!is.na(steps))

steps.per.day<- activity.monitoring.data.non.nan %>% 
  group_by(date) %>% 
  summarize(total.steps=sum(steps))

xtablePerSteps<-xtable(steps.per.day)
print(xtablePerSteps,type="html")
```

<!-- html table generated in R 4.0.2 by xtable 1.8-4 package -->
<!-- Mon Nov 16 15:37:18 2020 -->
<table border=1>
<tr> <th>  </th> <th> date </th> <th> total.steps </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> 2012-10-02 </td> <td align="right"> 126 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> 2012-10-03 </td> <td align="right"> 11352 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> 2012-10-04 </td> <td align="right"> 12116 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> 2012-10-05 </td> <td align="right"> 13294 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> 2012-10-06 </td> <td align="right"> 15420 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> 2012-10-07 </td> <td align="right"> 11015 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> 2012-10-09 </td> <td align="right"> 12811 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> 2012-10-10 </td> <td align="right"> 9900 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> 2012-10-11 </td> <td align="right"> 10304 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> 2012-10-12 </td> <td align="right"> 17382 </td> </tr>
  <tr> <td align="right"> 11 </td> <td> 2012-10-13 </td> <td align="right"> 12426 </td> </tr>
  <tr> <td align="right"> 12 </td> <td> 2012-10-14 </td> <td align="right"> 15098 </td> </tr>
  <tr> <td align="right"> 13 </td> <td> 2012-10-15 </td> <td align="right"> 10139 </td> </tr>
  <tr> <td align="right"> 14 </td> <td> 2012-10-16 </td> <td align="right"> 15084 </td> </tr>
  <tr> <td align="right"> 15 </td> <td> 2012-10-17 </td> <td align="right"> 13452 </td> </tr>
  <tr> <td align="right"> 16 </td> <td> 2012-10-18 </td> <td align="right"> 10056 </td> </tr>
  <tr> <td align="right"> 17 </td> <td> 2012-10-19 </td> <td align="right"> 11829 </td> </tr>
  <tr> <td align="right"> 18 </td> <td> 2012-10-20 </td> <td align="right"> 10395 </td> </tr>
  <tr> <td align="right"> 19 </td> <td> 2012-10-21 </td> <td align="right"> 8821 </td> </tr>
  <tr> <td align="right"> 20 </td> <td> 2012-10-22 </td> <td align="right"> 13460 </td> </tr>
  <tr> <td align="right"> 21 </td> <td> 2012-10-23 </td> <td align="right"> 8918 </td> </tr>
  <tr> <td align="right"> 22 </td> <td> 2012-10-24 </td> <td align="right"> 8355 </td> </tr>
  <tr> <td align="right"> 23 </td> <td> 2012-10-25 </td> <td align="right"> 2492 </td> </tr>
  <tr> <td align="right"> 24 </td> <td> 2012-10-26 </td> <td align="right"> 6778 </td> </tr>
  <tr> <td align="right"> 25 </td> <td> 2012-10-27 </td> <td align="right"> 10119 </td> </tr>
  <tr> <td align="right"> 26 </td> <td> 2012-10-28 </td> <td align="right"> 11458 </td> </tr>
  <tr> <td align="right"> 27 </td> <td> 2012-10-29 </td> <td align="right"> 5018 </td> </tr>
  <tr> <td align="right"> 28 </td> <td> 2012-10-30 </td> <td align="right"> 9819 </td> </tr>
  <tr> <td align="right"> 29 </td> <td> 2012-10-31 </td> <td align="right"> 15414 </td> </tr>
  <tr> <td align="right"> 30 </td> <td> 2012-11-02 </td> <td align="right"> 10600 </td> </tr>
  <tr> <td align="right"> 31 </td> <td> 2012-11-03 </td> <td align="right"> 10571 </td> </tr>
  <tr> <td align="right"> 32 </td> <td> 2012-11-05 </td> <td align="right"> 10439 </td> </tr>
  <tr> <td align="right"> 33 </td> <td> 2012-11-06 </td> <td align="right"> 8334 </td> </tr>
  <tr> <td align="right"> 34 </td> <td> 2012-11-07 </td> <td align="right"> 12883 </td> </tr>
  <tr> <td align="right"> 35 </td> <td> 2012-11-08 </td> <td align="right"> 3219 </td> </tr>
  <tr> <td align="right"> 36 </td> <td> 2012-11-11 </td> <td align="right"> 12608 </td> </tr>
  <tr> <td align="right"> 37 </td> <td> 2012-11-12 </td> <td align="right"> 10765 </td> </tr>
  <tr> <td align="right"> 38 </td> <td> 2012-11-13 </td> <td align="right"> 7336 </td> </tr>
  <tr> <td align="right"> 39 </td> <td> 2012-11-15 </td> <td align="right">  41 </td> </tr>
  <tr> <td align="right"> 40 </td> <td> 2012-11-16 </td> <td align="right"> 5441 </td> </tr>
  <tr> <td align="right"> 41 </td> <td> 2012-11-17 </td> <td align="right"> 14339 </td> </tr>
  <tr> <td align="right"> 42 </td> <td> 2012-11-18 </td> <td align="right"> 15110 </td> </tr>
  <tr> <td align="right"> 43 </td> <td> 2012-11-19 </td> <td align="right"> 8841 </td> </tr>
  <tr> <td align="right"> 44 </td> <td> 2012-11-20 </td> <td align="right"> 4472 </td> </tr>
  <tr> <td align="right"> 45 </td> <td> 2012-11-21 </td> <td align="right"> 12787 </td> </tr>
  <tr> <td align="right"> 46 </td> <td> 2012-11-22 </td> <td align="right"> 20427 </td> </tr>
  <tr> <td align="right"> 47 </td> <td> 2012-11-23 </td> <td align="right"> 21194 </td> </tr>
  <tr> <td align="right"> 48 </td> <td> 2012-11-24 </td> <td align="right"> 14478 </td> </tr>
  <tr> <td align="right"> 49 </td> <td> 2012-11-25 </td> <td align="right"> 11834 </td> </tr>
  <tr> <td align="right"> 50 </td> <td> 2012-11-26 </td> <td align="right"> 11162 </td> </tr>
  <tr> <td align="right"> 51 </td> <td> 2012-11-27 </td> <td align="right"> 13646 </td> </tr>
  <tr> <td align="right"> 52 </td> <td> 2012-11-28 </td> <td align="right"> 10183 </td> </tr>
  <tr> <td align="right"> 53 </td> <td> 2012-11-29 </td> <td align="right"> 7047 </td> </tr>
   </table>

### Make a histogram of the total number of steps taken each day.


```r
hist(xtablePerSteps$total.steps, 
     main="Histogram for total of steps.", 
     xlab="Steps", 
     border="white", 
     col="blue", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Calculate and report the mean and median of the total number of steps taken per day.


```r
stepsPerDateMedian<-median(xtablePerSteps$total.steps)
stepsPerDateMean<-mean(xtablePerSteps$total.steps)
```

Mean = 1.0766189\times 10^{4}  
Median = 10765

## What is the average daily activity pattern?

### Make a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the  5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
mean.steps.per.interval<- activity.monitoring.data.non.nan %>% 
  group_by(interval) %>% 
  summarize(mean.steps=mean(steps))

maxMean<-max(mean.steps.per.interval$mean.steps)

maxInterval<-mean.steps.per.interval[mean.steps.per.interval$mean.steps== maxMean,]

plot(mean.steps.per.interval$interval, 
     mean.steps.per.interval$mean.steps, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


Maximum interval:


```r
xtableMaxInterval<-xtable(maxInterval)
print(xtableMaxInterval,type="html")
```

<!-- html table generated in R 4.0.2 by xtable 1.8-4 package -->
<!-- Mon Nov 16 15:37:19 2020 -->
<table border=1>
<tr> <th>  </th> <th> interval </th> <th> mean.steps </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right"> 835 </td> <td align="right"> 206.17 </td> </tr>
   </table>

## Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA\color{red}{\verb|NA|}NAs)


```r
totalNAsa<-sum(is.na(activity.monitoring.data))
```

Total NAs=2304


### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I calculate the media and set it based on the interval, I think is a better aproach that using the weekday.

### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
altered.data<-mutate(activity.monitoring.data, imputingSteps=
                        ifelse(is.na(steps),
                               mean.steps.per.interval$mean.steps[mean.steps.per.interval$interval == activity.monitoring.data$interval]
                               ,steps))

altered.data$imputingSteps[which(is.na(altered.data$imputingSteps))]<-0
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps.per.day.imputed<- altered.data %>% 
  group_by(date) %>% 
  summarize(total.steps=sum(imputingSteps))

hist(steps.per.day.imputed$total.steps, 
     main="Histogram for total of steps.", 
     xlab="Steps", 
     border="white", 
     col="blue", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
stepsPerDateMedian<-median(steps.per.day.imputed$total.steps)
stepsPerDateMean<-mean(steps.per.day.imputed$total.steps)
```

Mean = 9530.7244046  
Median = 1.0439\times 10^{4}



## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
defineDayType<-function(day)
{
  date<-ymd(day)
  dayasWDay<-wday(date)
  ifelse(dayasWDay %in% c(7,1), "weekend","weekday")
}

altered.data.weekday<-mutate(altered.data,weekday=defineDayType(date))

altered.data.weekday.interval<- altered.data.weekday %>% 
  group_by(interval,weekday) %>% 
  summarize(mean.steps=mean(imputingSteps))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

### Make a panel plot containing a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
ggplot(altered.data.weekday.interval, aes(x=interval, y=mean.steps, colour=weekday)) +
  facet_grid(weekday ~ .)+
  geom_line() 
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

