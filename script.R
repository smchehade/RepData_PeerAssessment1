#loading Libraries
library(dplyr)
library(lubridate)
library(ggplot2)

## Loading and processing the data
setwd("C:/Users/ecaeshh/Documents/R Programs DIrectory/RepData_PeerAssessment1")

###Checks if file is existing, if not download file & unzip
if(!file.exists("activity.csv")){
        if(!file.exists("activity.zip")) {
                fileurl <-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
                download.file(fileurl,getwd())
        }
        unzip("activity.zip",exdir = getwd())
}

activity_df<-tbl_df(read.csv("activity.csv"))
activity_df$date<-as.Date(activity_df$date) #all variables in correct type

## What is mean total number of steps taken per day?

dailyactivity_df <- activity_df %>% group_by(date) %>% summarise(dailysteps = sum(steps, na.rm = TRUE))
mean(dailyactivity_df$dailysteps,na.rm = TRUE)
g<-ggplot(dailyactivity_df, aes(date, dailysteps))
g+geom_bar(stat="identity")
dailyactivity_df <- activity_df %>% group_by(date) %>% summarise(dailysteps = sum(steps, na.rm = TRUE),dailymean= mean(steps,na.rm = TRUE),dailymedian= median(steps,na.rm = TRUE ))



## What is the average daily activity pattern?
intervalactivity_df <- activity_df %>% group_by(interval) %>% summarise(intervalsteps = sum(steps, na.rm = TRUE),intervalmean= mean(steps,na.rm = TRUE),intervalmedian= median(steps,na.rm = TRUE ))
with(intervalactivity_df, plot(interval,intervalmean, type="l", main = "Average number of steps across all days taken per 5-minute inerval", lwd=3, ylab = "Average steps", xlab="5-minute time interval", col="blue"))


xmax<-intervalactivity_df[which(intervalactivity_df$intervalmean == max(intervalactivity_df$intervalmean, na.rm = TRUE)),"interval"]
ymax <-intervalactivity_df[which(intervalactivity_df$intervalmean == max(intervalactivity_df$intervalmean, na.rm = TRUE)),"intervalmean"]

points(xmax,ymax, lwd=4, pch=18, col="red")


## Imputing missing values
missingsteps <-sum(is.na(activity_df$steps)) # count of missing values in steps
imputeactivity_df <- activity_df
                #impute over the day doesnot work, as whenever there is a missing value at that day, 
                #it mean no data collected on that specific day, and value would be zero. 
                #It makes more sense to take the mean on interval level, 
                #as it more reflects the steps that would have been walked at that interval of time.

for (i in 1:length(imputeactivity_df$steps)) { 
        if(is.na(imputeactivity_df[i,"steps"])) 
        {meanimpute <- intervalactivity_df[which(intervalactivity_df$interval %in% imputeactivity_df$interval[i]),"intervalmean"]
        imputeactivity_df[i,"steps"] <- as.integer(round(meanimpute))
        }
        
}

imputedailyactivity_df <- imputeactivity_df %>% group_by(date) %>% summarise(dailysteps = sum(steps, na.rm = TRUE),dailymean= mean(steps,na.rm = TRUE),dailymedian= median(steps,na.rm = TRUE ))
mean(imputedailyactivity_df$dailysteps,na.rm = TRUE)
g<-ggplot(imputedailyactivity_df, aes(date, dailysteps))
g+geom_bar(stat="identity")



## Are there differences in activity patterns between weekdays and weekends?
weekdaysvector<- c("Monday","Tuesday", "Wednesday","Thursday","Friday")
weekendvector<- c("Saturday","Sunday")
imputeactivity_df<-mutate(imputeactivity_df, daytype = "dataype")

for (i in 1:length(imputeactivity_df$steps)) {
        if (weekdays(imputeactivity_df$date[i]) %in% weekdaysvector) {
                imputeactivity_df$daytype[i]<-"Weekday"
        }else{
                if(weekdays(imputeactivity_df$date[i]) %in% weekendvector) {
                        imputeactivity_df$daytype[i]<-"weekend"
                }
        }
        
}
imputeactivity_df<-mutate(imputeactivity_df, daytype = as.factor(daytype))
imputeintervalactivity_df <- imputeactivity_df %>% group_by(interval,daytype) %>% summarise(intervalsteps = sum(steps, na.rm = TRUE),intervalmean= mean(steps,na.rm = TRUE),intervalmedian= median(steps,na.rm = TRUE ))
wk<-ggplot(imputeintervalactivity_df, aes(interval, intervalmean)) + facet_grid(daytype~.)
wk<- wk +labs(title = "Average number of steps across all days taken per 5-minute inerval")
wk<- wk+ labs(y = "Average steps", x="5-minute time interval")
wk  +geom_line()

