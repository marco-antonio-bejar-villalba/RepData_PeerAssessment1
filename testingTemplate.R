
library(dplyr)
library(xtable)

######################

zipFile<-"activity.zip"
csvFile<-"activity.csv"

if(!file.exists(csvFile)){
  unzip("activity.zip")
}

activity.monitoring.data<-read.csv(csvFile)

##################################################

activity.monitoring.data.non.nan<-subset(activity.monitoring.data,!is.na(steps))

steps.per.day<- activity.monitoring.data.non.nan %>% 
  group_by(date) %>% 
  summarize(total.steps=sum(steps))

xtablePerSteps<-xtable(steps.per.day)

print(xtablePerSteps,type="html")

hist(xtablePerSteps$total.steps, 
     main="Histogram for total of steps.", 
     xlab="Steps", 
     border="white", 
     col="blue")

stepsPerDateMedian<-median(xtablePerSteps$total.steps)
stepsPerDateMean<-mean(xtablePerSteps$total.steps)


mean.steps.per.interval<- activity.monitoring.data.non.nan %>% 
  group_by(interval) %>% 
  summarize(mean.steps=mean(steps))

plot(mean.steps.per.interval$interval, 
     mean.steps.per.interval$mean.steps, type="l")

maxMean<-max(mean.steps.per.interval$mean.steps)

maxInterval<-mean.steps.per.interval[mean.steps.per.interval$mean.steps== maxMean,]

# activity.monitoring.data$steps[which(is.na(activity.monitoring.data$steps))] <-
#   mean.steps.per.interval$mean.steps[mean.steps.per.interval$interval ==
#                             activity.monitoring.data$interval]

########################################################
altered.data<-mutate(activity.monitoring.data, imputingSteps=
                        ifelse(is.na(steps),
                               mean.steps.per.interval$mean.steps[mean.steps.per.interval$interval == activity.monitoring.data$interval]
                               ,steps))

altered.data$imputingSteps[which(is.na(altered.data$imputingSteps))]<-0

steps.per.day.imputed<- altered.data %>% 
  group_by(date) %>% 
  summarize(total.steps=sum(imputingSteps))

hist(steps.per.day.imputed$total.steps, 
     main="Histogram for total of steps.", 
     xlab="Steps", 
     border="white", 
     col="blue")
