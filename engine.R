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

library(data.table)
library(lubridate)

# Read In File
baseFile <- read.csv("activity.csv", sep=",")

# Process Data
baseTable <- data.table(baseFile)
baseTable$date <- as.Date(baseTable$date)
baseTable$day <- as.numeric(strftime(baseTable$date, format="%j"))
stepsByDay <-  baseTable[, sum(steps), keyby=day]
names(stepsByDay) <- c("Day", "TotalSteps")

# Plot Total Steps Per Day
png("_testplot.png", width=540, height=540)
with(stepsByDay, plot(x = Day, 
                      y = TotalSteps, 
                      type = "h",
                      xlab = "Day of Year",
                      ylab = "Total Steps Per Day",
                      main = "Total Steps By Numeric Day Of Year"
                      ))
dev.off()

