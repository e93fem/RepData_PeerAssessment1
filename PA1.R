setwd("C:/Users/Fredrik/Documents/Reproducible Research/Projects/RepData_PeerAssessment1")

file <- "activity.zip"
data <- read.csv(unz(file, "activity.csv"), header=TRUE, sep=",")
data$date <- as.Date(data$date, "%Y-%m-%d")

sum_aggr = aggregate(data$steps, by=list(Category=data$date), FUN=sum, na.rm = TRUE)

barplot(sum_aggr$x, names.arg=sum_aggr$Category)
mean(sum_aggr$x)
median(sum_aggr$x)
