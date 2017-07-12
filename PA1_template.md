Loading and preprocessing the data
----------------------------------

1.Load the data (i.e. read.csv())

``` r
#Load the data (i.e. read.csv())
activity <- read.csv("activity.csv",header=TRUE)
View(activity)
activity$date <- as.Date(activity$date)
str(activity$date)
```

    ##  Date[1:17568], format: "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...

2.Process/transform the data (if necessary) into a format suitable for analysis

``` r
#classifying date into weekday or weekend
activity <- data.frame(steps=activity$steps,date=activity$date,interval=activity$interval,weekday=tolower(weekdays(activity$date)))
activity <- cbind(activity,daytype=ifelse(activity$weekday=="saturday"|activity$weekday=="sunday","weekend","weekday"))
activity_final <- data.frame(steps=activity$steps,date=activity$date,interval=activity$interval,weekday=activity$weekday,daytype=activity$daytype)
rm(activity)
#We display the first few rows of the activity data frame:
head(activity_final)
```

    ##   steps       date interval weekday daytype
    ## 1    NA 2012-10-01        0  monday weekday
    ## 2    NA 2012-10-01        5  monday weekday
    ## 3    NA 2012-10-01       10  monday weekday
    ## 4    NA 2012-10-01       15  monday weekday
    ## 5    NA 2012-10-01       20  monday weekday
    ## 6    NA 2012-10-01       25  monday weekday

What is mean total number of steps taken per day?
-------------------------------------------------

1.Make a histogram of the total number of steps taken each day

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
sum_data <- aggregate(activity_final$steps, by=list(activity_final$date), FUN=sum, na.rm=TRUE)
names(sum_data) <- c("date", "total")
# Compute the histogram of the total number of steps each day
hist(sum_data$total, breaks=seq(from=0, to=25000, by=2500),col="blue", xlab="Total number of steps", ylim=c(0, 20), main="Histogram of the total number of steps taken each day\n(NA removed)")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)

2.Calculate and report the mean and median total number of steps taken per day

``` r
p<-mean(sum_data$total)
q<-median(sum_data$total)
```

The mean and median are 9354.2295082 and 10395

What is the average daily activity pattern?
-------------------------------------------

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
# Compute the means of steps accross all days for each interval
mean_data <- aggregate(activity_final$steps,by=list(activity_final$interval),FUN=mean,na.rm=TRUE)
names(mean_data)<- c("interval","mean")
head(mean_data)
```

    ##   interval      mean
    ## 1        0 1.7169811
    ## 2        5 0.3396226
    ## 3       10 0.1320755
    ## 4       15 0.1509434
    ## 5       20 0.0754717
    ## 6       25 2.0943396

``` r
# Compute the time series plot
plot(mean_data$interval, mean_data$mean, type="l", col="blue", lwd=2, xlab="Interval [minutes]",ylab="Average number of steps", main="Time-series of the average number of steps per intervals\n(NA removed)")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png)

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
# We find the position of the maximum mean
maxpos <- which(mean_data$mean==max(mean_data$mean))
maxinterval <- mean_data[maxpos,]
```

The 5-minute interval that contains the maximum of steps, on average across all days, is 835.

Imputing missing values
-----------------------

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA???s)

``` r
# Find the NA positions
x <-which(is.na(activity_final$steps)==TRUE)
y <-length(x)
```

The number of NA's is 2304

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

``` r
# Create a vector of means
mean_vec <- rep(mean(activity_final$steps, na.rm=TRUE), times=length(x))
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
# Replace the NAs by the means
activity_final[x,"steps"]<- mean_vec
head(activity_final)  
```

    ##     steps       date interval weekday daytype
    ## 1 37.3826 2012-10-01        0  monday weekday
    ## 2 37.3826 2012-10-01        5  monday weekday
    ## 3 37.3826 2012-10-01       10  monday weekday
    ## 4 37.3826 2012-10-01       15  monday weekday
    ## 5 37.3826 2012-10-01       20  monday weekday
    ## 6 37.3826 2012-10-01       25  monday weekday

``` r
# Compute the total  number of steps each day (NA values removed)
sum_data1 <- aggregate(activity_final$steps, by=list(activity_final$date), FUN=sum, na.rm=TRUE)
names(sum_data1) <- c("date", "total")
```

4.Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
# Compute the histogram of the total number of steps each day
hist(sum_data1$total, breaks=seq(from=0, to=25000, by=2500),col="blue",  xlab="Total number of steps", ylim=c(0, 30), main="Histogram of the total number of steps taken each day\n(NA replaced)")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
r <-mean(sum_data1$total) 
s <-median(sum_data1$total) 
```

These formulas gives a mean and median of 1.076618910^{4} and 1.076618910^{4} respectively.

These values differ greatly from the estimates from the first part of the assignment. The impact of imputing the missing values is to have more data, hence to obtain a bigger mean and median value.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.Create a new factor variable in the dataset with two levels - ???weekdays??? and ???weekend??? indicating whether a given date is a weekday or weekend day.

``` r
# The new factor variable "daytype" was already in the activity data frame
head(activity_final)
```

    ##     steps       date interval weekday daytype
    ## 1 37.3826 2012-10-01        0  monday weekday
    ## 2 37.3826 2012-10-01        5  monday weekday
    ## 3 37.3826 2012-10-01       10  monday weekday
    ## 4 37.3826 2012-10-01       15  monday weekday
    ## 5 37.3826 2012-10-01       20  monday weekday
    ## 6 37.3826 2012-10-01       25  monday weekday

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5- minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

``` r
library(lattice)

# Compute the average number of steps taken, averaged across all daytype variable
 mean_data1 <- aggregate(activity_final$steps,  by=list(activity_final$daytype, activity_final$weekday, activity_final$interval), mean)
 names(mean_data1) <- c("daytype", "weekday", "interval", "mean")
 # Compute the time serie plot
xyplot(mean ~ interval | daytype, mean_data1, type="l", lwd=1, xlab="Interval", ylab="Average number of steps",layout=c(1,2))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-12-1.png)
