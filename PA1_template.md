

output: 
  html_document: 
    keep_md: yes


---
title: "PA1_template"

---
##Reproducible Research
###Loading and preprocessing the data


```r
data<-read.csv('activity.csv',header=T)


library(data.table)
data_table<-data.table(data)
data_table_summary<-data_table[,list(total_steps=sum(steps,na.rm=T)), 
                          by=date]
```




```r
#Plot histogram
hist(data_table_summary$total_steps, 
     breaks=20,
     main='Number of Steps Taken Per Day',
     xlab='Total Number of Steps', col='pink')
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

###What is mean total number of steps taken per day?

```r
mean_value<-mean(data_table_summary$total_steps)
median_value<-median(data_table_summary$total_steps)
mean_value
```

```
## [1] 9354.23
```

```r
median_value
```

```
## [1] 10395
```

Mean total numbers of steps per day is 9354.2 and median is 10395.

###What is the average daily activity pattern?

```r
#Summarize dataset by interval
data_interval<-data_table[,list(avg_steps=mean(steps,na.rm=T)), 
                          by=interval]
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
with(data_interval,{
        plot(interval,avg_steps,type='l',
             main='Average Steps by Time Interval',
             xlab='5 Minute Time Interval',
             ylab='Average Number of Steps')
        })
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
data_interval[data_interval$avg_steps==max(data_interval$avg_steps),]
```

```
##    interval avg_steps
## 1:      835  206.1698
```
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  A/ 835

###Imputing missing values

```r
sum(is.na(data))
```

```
## [1] 2304
```

```r
newdata<-data 
for (i in 1:nrow(newdata)) {
    if (is.na(newdata$steps[i])) {
        newdata$steps[i]<-data_interval[which(newdata$interval[i] == data_interval$interval), ]$avg_steps
    }
}

library(ggplot2)
hist1<-ggplot(newdata,aes(date,steps))+geom_bar(stat="identity")+labs(title="Histogram of Number of Steps",x="Date",y="Number of Steps")
hist1
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
steps_i<-tapply(newdata$steps,newdata$date,FUN=sum)
mean(steps_i)
```

```
## [1] 10766.19
```

```r
median(steps_i)
```

```
## [1] 10766.19
```


###Are there differences in activity patterns between weekdays and weekends?


