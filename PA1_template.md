# Reproducible Research: Peer Assessment 1
## set locale : English

```r
Sys.setlocale("LC_TIME","English")
```

```
## [1] "English_United States.1252"
```

## Loading and preprocessing the data

```r
# Load the data (i.e. read.csv())
data <- read.csv("activity.csv", 
                 stringsAsFactors=FALSE)
# Process/transform column(date) into date format suitable for analysis
data$date <- as.Date(data$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

```r
# Make a histogram of the total number of steps taken each day
x <- tapply(data$steps,data$date,sum)
hist(x,main="Histogram of the total number of steps per day",
     xlab="steps")
```

![plot of chunk histogram_01](figure/histogram_01.png) 

```r
#plot(x,type="l")
#plot(data$steps,type="l")
# Calculate and report the mean and median total number of steps taken per day
s_mean <- cbind(tapply(data$steps,data$date,mean,na.rm=TRUE))
colnames(s_mean) <- c("steps")
s1_mean <- mean(s_mean,na.rm = TRUE)

s_median <- cbind(tapply(data$steps,data$date,median,na.rm=TRUE))
colnames(s_median) <- c("steps")
s1_median <- median(s_median,na.rm = TRUE)
```
The mean and median of total number of steps taken per day are 37.3826 and 0 respectively.

## What is the average daily activity pattern?

```r
#  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
s_interval <- cbind(tapply(data$steps,data$interval,mean,na.rm=TRUE))
plot(s_interval,type="l",xaxt = "n",
     xlab="5 min interval",
     ylab="average steps")
axis_interval <- rownames(s_interval)
axis(1, 1:nrow(s_interval),axis_interval,tick = FALSE)
```

![plot of chunk plot](figure/plot.png) 

```r
#  Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
names <- rownames(s_interval)
```
In 835 interval, on average across all the days in 
the dataset, contains the maximun number of steps, which is 206.1698 steps.

## Imputing missing values

```r
# Calculate and report the total number of missing values in the dataset (i.e. 
# the total number of rows with NAs)
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use 
# the mean/median for that day, or the mean for that 5-minute interval, etc.

# strategy : assign 5-minute average to NAs

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
data2 <- data
for(i in 1:nrow(data2)) {
  if (is.na(data2$steps[i])) {
    # assign 5-minute average to NAs
    intv <- as.numeric(data2$interval[i])
    j <- (intv) %/% 100 * 5 + (intv %% 100) / 5 +1
    data2$steps[i] <- s_interval[j]
  }
}

# Make a histogram of the total number of steps taken each day and Calculate 
# and report the mean and median total number of steps taken per day. Do these 
# values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total 
# daily number of steps?

#x <- tapply(data$steps,data$date,sum)
hist(tapply(data2$steps,data2$date,sum),
     main="Histogram of the total number of steps per day",
     xlab="steps")
```

![plot of chunk histogram_02](figure/histogram_02.png) 

```r
s_mean2 <- cbind(tapply(data2$steps,data2$date,mean))
colnames(s_mean2) <- c("steps")
s1_mean2 <- mean(s_mean2,na.rm = TRUE)
#s_mean2

s_median2 <- cbind(tapply(data2$steps,data2$date,median))
colnames(s_median2) <- c("steps")
s1_median2 <- median(s_median2,na.rm = TRUE)
```
The mean and median of total number of steps taken per day are 37.155 and 0 respectively.


```r
s_interval <- cbind(tapply(data$steps,data$interval,mean,na.rm=TRUE))
l <- cbind(unique(data$interval),s_interval)
s_interval2 <- cbind(tapply(data2$steps,data2$interval,mean,na.rm=TRUE))
l2 <- cbind(unique(data$interval),s_interval2)
plot(l[,2],type="n",xaxt = "n",
     xlab="5 min interval",
     ylab="average steps")
axis_interval <- rownames(s_interval)
axis(1, 1:nrow(s_interval),axis_interval,tick = FALSE)
lines(l[,2],col="red")
#lines(l[,1],l[,2],col="blue")
lines(l2[,2],col="blue")
legend("topright",
       legend=c("original","filled-in missing values"),
       lty=c(1,1),cex=0.8,
       col=c("red","blue"))
```

![plot of chunk histogram_03](figure/histogram_03.png) 


## Are there differences in activity patterns between weekdays and weekends?

```r
# Create a new factor variable in the dataset with two levels weekday and 
# weekend indicating whether a given date is a weekday or weekend day.
data2$weekend <- as.factor(weekdays(data2$date)== "Sunday" | 
                           weekdays(data2$date) == "Saturday")

# Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis). The plot should look 
# something like the following, which was creating using simulated data:
data_weekend <- data2[data2$weekend == TRUE,]
data_weekday <- data2[data2$weekend == FALSE,]
s_interval1 <- cbind(tapply(data_weekday$steps,data_weekday$interval,mean,na.rm=TRUE))
l1 <- cbind(unique(data_weekday$interval),s_interval1)
s_interval2 <- cbind(tapply(data_weekend$steps,data_weekend$interval,mean,na.rm=TRUE))
l2 <- cbind(unique(data_weekend$interval),s_interval2)
par(mfrow=c(2,1))
par(mar=c(4,5,2,2))
plot(l1[,2],type="n",xaxt = "n",
     main="weekdays",
     xlab="",
     ylab="average steps")
axis_interval <- rownames(s_interval1)
axis(1, 1:nrow(s_interval1),axis_interval,tick = FALSE)
lines(l1[,2],col="red")

plot(l1[,2],type="n",xaxt = "n",
     main="weekends",
     xlab="5 min interval",
     ylab="average steps")
axis_interval <- rownames(s_interval2)
axis(1, 1:nrow(s_interval2),axis_interval,tick = FALSE)
lines(l2[,2],col="blue")
```

![plot of chunk plot2](figure/plot2.png) 

```r
par(mfrow=c(1,1))
```






