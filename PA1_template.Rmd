## load, unzip dan read activiti file
setwd("C:\\Users\\SHIKIN\\Documents\\Coursera saya")
#url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfile <- "step_data.zip"
download.file(url, destfile)
unzip(destfile)
activity <- read.csv("activity.csv", sep = ",")

## structure data taking away a rows with NA steps
names(activity)
str(activity)
head(activity[which(!is.na(activity$steps)), ])

## Mean "number of step" by day
library(reshape2)
activity_melt <- melt(activity[which(!is.na(activity$steps)), ], id.vars = c("date", "interval"))
head(activity_melt)
steps_sum <- dcast(activity_melt, date ~ variable, sum)
head(steps_sum)

summary(steps_sum$steps) # rumusan



## Histogram ~ number of step sans NA rows. also
## mean and median data.
hist(steps_sum$steps, main = "Histogram ~ Total Steps Per Day", 
     xlab = "Total steps per day", ylab = "Number of days", 
     breaks = 10, col = "green")
abline(v = mean(steps_sum$steps), lty = 1, lwd = 2, col = "yellow")
abline(v = median(steps_sum$steps), lty = 2, lwd = 2, col = "magenta")
legend(x = "topright", c("Mean", "Median"), col = c("yellow", "magenta"), 
       lty = c(1, 2), lwd = c(2, 2))

## Mean daily activity pattern  
stepsmeaninterval <- dcast(activity_melt, interval ~ variable, 
                           mean, na.rm = TRUE)
head(stepsmeaninterval)
plot(stepsmeaninterval$interval, stepsmeaninterval$steps, ty = "l",
     xlab = "time interval", ylab = "Average steps", 
     main = "Average steps taken per all days X time interval")
maxsteps_interval <- 
  stepsmeaninterval$interval[which.max(stepsmeaninterval$steps)]
maxsteps_interval



## imputing missing value
activity2 <- split(activity, activity$interval)
activity2 <- lapply(activity2, function(x) {
  x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
  return(x)
})
activity2 <- do.call("rbind", activity2)
row.names(activity2) <- NULL

activity2 <- split(activity2, activity2$date)
df <- lapply(activity2, function(x) {
  x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
  return(x)
})
activity2 <- do.call("rbind", activity2)
row.names(activity2) <- NULL
head(activity2)

library(reshape2)
activity_melt2 <- melt(activity2, id.vars = c("date", "interval"))
steps_sum <- dcast(activity_melt2, date ~ variable, sum, na.rm = TRUE)
head(steps_sum)

## Histogram ~ total step taken with imputing missing value
hist(steps_sum$steps, main = "Histogram of total steps taken per day", 
     xlab = "Total steps per day", ylab = "Number of days", 
     breaks = 10, col = "green")
abline(v = mean(steps_sum$steps), lty = 1, lwd = 2, col = "yellow")
abline(v = median(steps_sum$steps), lty = 2, lwd = 2, col = "magenta")
legend(x = "topright", c("Mean", "Median"), col = c("yellow", "magenta"), 
       lty = c(2, 1), lwd = c(2, 2))

## Number of row with NA value
sum(is.na(activity$steps))
sum(is.na(activity$steps))*100/nrow(activity) # Peratus row



## Different aktiviti pattern : weekdays x weekends
library(lubridate)
weekends <- which(weekdays(as.Date(activity2$date)) == "Saturday" |
                    weekdays(as.Date(activity2$date)) == "Sunday")
weekdays <- which(weekdays(as.Date(activity2$date)) != "Saturday" &
                    weekdays(as.Date(activity2$date)) != "Sunday")
temp <- c(rep("a", length(activity2)))
temp[weekends] <- "weekend"
temp[weekdays] <- "weekday"
length(temp)
activity2 <- cbind(activity2, temp)
head(activity2)
names(activity2)[4] <- "day"

activity2split <- split(activity2, activity2$day)
stepsmean_interval <- lapply(activity2split, function(x) {
  temp <- aggregate(x$steps, list(x$interval), mean)
  names(temp) <- c("interval", "steps")
  return(temp)
})

## no split stepsmean_interval
stepsmean_interval <- do.call("rbind", stepsmean_interval)
weekdays <- grep("weekday" ,row.names(stepsmean_interval))
weekends <- grep("weekend" ,row.names(stepsmean_interval))
temp <- c(rep("a", length(stepsmean_interval$steps)))
temp[weekdays] <- "weekdays"
temp[weekends] <- "weekends"
names(temp) <- "day"
stepsmean_interval <- cbind(stepsmean_interval, temp)
row.names(stepsmean_interval) <- NULL

head(stepsmean_interval)

library(ggplot2)
ggplot(stepsmean_interval, aes(interval, steps)) + geom_line() + facet_grid(temp ~ .) 

