# Reproducible Research Week 2 Project

# Setting Up Environment
setwd("/Users/Chris/development/r/RepData_PeerAssessment1/")

# Loading Up Libraries/Packages
is.installed <- function(mypkg){
  is.element(mypkg, installed.packages()[,1])
}

if (!is.installed("data.table")){
  install.packages("data.table")
}

if (!is.installed("lubridate")){
  install.packages("lubridate")
}

if (!is.installed("ggplot2")){
  install.packages("ggplot2")
}

library(data.table)
library(lubridate)
library(ggplot2)

# Clear Workspace
rm(list=ls())

# Import And Process Data
activityData <- read.csv('activity.csv', stringsAsFactors = FALSE)
activityData$date <- as.POSIXct(as.Date(activityData$date), format = "%Y-%M-%D")
activityFrame <- data.frame(Date = activityData$date,
                              WeekDay = tolower(weekdays(activityData$date)),
                              Steps = activityData$steps,
                              interval = activityData$interval
                            )
activityFrame$DayType = ifelse(activityFrame$WeekDay == "Saturday" | activityFrame$WeekDay == "Sunday", "Weekend", "Weekday")
rm(activityData)

# Get Sum Of Days And Plot
stepsByDay <- aggregate(activityFrame$Steps, by=list(activityFrame$Date), FUN = sum, na.rm = TRUE)
names(stepsByDay) <- c("Day", "TotalSteps")
png("plot_totalstepsbyday.png", width=540, height=540)
hist(stepsByDay$TotalSteps, 
     xlab = "Total Steps", 
     ylab = "Frequency of Steps", 
     main = "Histogram - Total Steps By Day",
     ylim = range(1:30),
     col = "Light Grey"
)

dev.off()

# Get Mean/Median
stepsmean <- mean(stepsByDay$TotalSteps)
stepsmedian <- median(stepsByDay$TotalSteps)

# Make Time Series Plot Of Steps Over Interval
intervalTimeSeries <- aggregate(activityFrame$Steps, by=list(activityFrame$interval), FUN = mean, na.rm = TRUE)
names(intervalTimeSeries) <- c("Interval","AverageSteps")
png("plot_timeseries_meansteps_byinterval.png", width=540, height=540)
plot(x = intervalTimeSeries$Interval,
     y = intervalTimeSeries$AverageSteps,
     lwd = 4,
     type = "l",
     xlab = "Interval", 
     ylab = "Average Steps", 
     main = "Time Series -- Average Steps By Interval",
     col = "Black"
)

dev.off()

# Find The Interval Corresponding To Max Mean
maxPosition <- which(intervalTimeSeries$AverageSteps == max(intervalTimeSeries$AverageSteps))
intervalValue = intervalTimeSeries[maxPosition, 1]

# Find Number Of NA Values
naCount <- sum(is.na(activityFrame$Steps))

# Get Positions of NA Values
naPositions <- which(is.na(activityFrame$Steps))

# Replacement Values Creation
replacementVector <- rep(mean(activityFrame$Steps, na.rm=TRUE), times=length(naPositions))
activityFrame[naPositions, "Steps"] <- replacementVector

# Now compute all the numbers again
revisedStepsByDay <- aggregate(activityFrame$Steps, by=list(activityFrame$Date), FUN = sum, na.rm = TRUE)
names(revisedStepsByDay) <- c("Day", "TotalSteps")
png("plot_revised_totalstepsbyday.png", width=540, height=540)
hist(revisedStepsByDay$TotalSteps, 
     xlab = "Total Steps", 
     ylab = "Frequency of Steps", 
     main = "Histogram - Total Steps By Day, with Imputed Data",
     ylim = range(1:40),
     col = "Light Grey"
)

dev.off()

revised_stepsmean <- mean(revisedStepsByDay$TotalSteps)
revised_stepsmedian <- median(revisedStepsByDay$TotalSteps)

