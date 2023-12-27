# 1) Loading and preprocessing the data
# Install and load necessary packages
install.packages("knitr")
install.packages("rmarkdown")
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# Read the 'activity.csv' file into the 'activity' data frame
activity <- read.csv("activity.csv", as.is = TRUE)

# Remove rows with NA values in the 'steps' column
actwtoNA<-activity[!is.na(activity$steps),]

# 2) What is mean total number of steps taken per day?
# Calculate the mean total number of steps taken per day
act_by_date <- group_by(actwtoNA,date)
steps_per_day <- summarise(act_by_date,total=sum(steps))
# Display the total number of steps per day
steps_per_day

# Create a histogram of total steps per day
hist(steps_per_day$total,xlab = "Total No of Steps per day",col = "blue" , 
     main = "Histogram of total number of steps per day")

# Calculate the mean and median of total steps per day
mean(steps_per_day$total)
median(steps_per_day$total)

# 3) What is the average daily activity pattern?
# Calculate the average number of steps per interval across all days
step_interval=aggregate(steps ~ interval, actwtoNA, mean)

# Create a plot of the average number of steps per interval
plot(step_interval$interval, step_interval$steps,col=1 , type = 'l',
     main = "Average number of steps average across all days",
     xlab = "Interval", ylab = "Average number of steps")

# Find the interval with the maximum average number of steps
step_interval[which.max(step_interval$steps),]

# 4) Imputing missing values
# Identify rows with missing steps values
activityNA<-activity[is.na(activity$steps),]

# Calculate the number of rows with missing values
nrow(activityNA)

# Impute missing steps values based on the average for the corresponding interval
for ( i in 1:nrow(activity)) {
  if (is.na(activity$steps[i])) {
    act_interval <- activity$interval[i]
    row_no <- which(step_interval$interval == act_interval)
    activity$steps[i] <- step_interval$steps[row_no]
  }
}

# Calculate total steps per day after imputing missing values
steps_per_day_Imput_na<-aggregate(steps ~ date, activity, sum)

# Create a histogram of total steps per day with imputed missing values
hist(steps_per_day_Imput_na$steps, main = "Histogram of total number of steps with Imputed NA", xlab = "Step per Day")

# 5) Are there differences in activity patterns between weekdays and weekends?
# Add a 'day' column to categorize each day as 'weekday' or 'weekend'
activity['day']<-weekdays(as.Date(activity$date))
activity$day[activity$day %in% c('Saturday','Sunday')] <-"weekend"
activity$day[activity$day != "weekend"] <- "weekday"
activity$day<-factor(activity$day)

# Calculate the average number of steps per interval for weekdays and weekends
step_inter_day<- aggregate(steps ~ interval + day, activity, mean)

# Create a plot to visualize the differences in activity patterns between weekdays and weekends
print(ggplot(aes(interval, steps), data = step_inter_day, type = 'l') + geom_line(stat = "identity", aes(col = day)) + 
        facet_grid(~ day) + labs(y="No of Steps")
)