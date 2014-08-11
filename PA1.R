setwd("C:/Users/Fredrik/Documents/Reproducible Research/Projects/RepData_PeerAssessment1")

substrLast4 <- function(x){
  substr(x, nchar(x)-3, nchar(x))
}

file <- "activity.zip"
data <- read.csv(unz(file, "activity.csv"), header=TRUE, sep=",")
data$date <- as.Date(data$date, "%Y-%m-%d")
tmp <- paste("000",data$interval, sep="")
time <- lapply(tmp, substrLast4)
data$time <- as.POSIXct(paste(data$date, time), format="%Y-%m-%d %H%M")


sum_aggr = aggregate(data$steps, by=list(Category=data$date), FUN=sum, na.rm = TRUE)
mean_array <- rep(mean(sum_aggr$x), length(sum_aggr$Category))
median_array <- rep(median(sum_aggr$x), length(sum_aggr$Category))

barplot(sum_aggr$x, names.arg=sum_aggr$Category)
abline(lm(mean_array~sum_aggr$Category),col="blue")
abline(lm(median_array~sum_aggr$Category),col="red")
legend("topleft", lwd = 2, col = c("blue", "red"), legend = c("mean", "median"))


mean_array <- rep(mean(data$steps, na.rm = TRUE), length(data$time))
plot(data$time, data$steps, type="l")
abline(lm(mean_array~data$time),col="blue")
legend("topleft", lwd = 2, col = c("blue"), legend = c("mean"))

# rename 
data$steps[is.na(data$steps)] <- -1
data[data$steps==max(data$steps, na.rm = TRUE) ,]
