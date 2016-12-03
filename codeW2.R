##Project week2

#1.1 Download data
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
ruta <- file.path(getwd(),"/repdata Fdata Factivity.zip")
download.file(url,ruta)

#1.1 Read data
unzip(ruta)
rutaF <- file.path(getwd(),"/activity.csv")
AMD <- read.csv(rutaF, header = TRUE, na.strings = "NA", colClasses= c("integer","Date","integer"), stringsAsFactors= FALSE)

#2. Histogram of the total number of steps taken each day
TSPD <- aggregate(steps~date,data= AMD,FUN= sum)
pdf(file= "histTSPD.pdf")
with(TSPD,hist(steps,
	main= "Histogram for Steps per day",
	xlab= "Total # Steps/day",
	border= "blue",
	col= "green",
	breaks= 5))
dev.off()

#3. Mean and median number of steps taken each day
meanTSPD <- mean(TSPD$steps,na.rm = TRUE)
medianTSPD <- median(TSPD$steps,na.rm = TRUE)

#4. Time series plot of the average number of steps taken
ADSPI <- aggregate(steps~interval,data= AMD,FUN= mean)
pdf(file= "plotADSPI.pdf")
with(ADSPI,plot(interval, steps,
	main= "Average Daily Activity",
	xlab= "5-minute interval (a day)",
	ylab= "Steps Average",
	col= "magenta",
	type= "l"
	))
dev.off()

#5. The 5-minute interval that, on average, contains the maximum number of steps
MAXint <- ADSPI$interval[ADSPI$steps==max(ADSPI$steps)]

#6. Code to describe and show a strategy for imputing missing data
TNNA <- nrow(AMD)-sum(complete.cases(AMD))
CCDS <- AMD[complete.cases(AMD),]
NADS <- AMD[!complete.cases(AMD),]
NADS$steps <- ADSPI[ADSPI$interval %in% NADS$interval,2]
AMDiNA <- rbind(CCDS,NADS)
AMDiNA <- AMDiNA[order(as.integer(row.names(AMDiNA))),]

#7. Histogram of the total number of steps taken each day after missing values are imputed
TSPD2 <- aggregate(steps~date,data= AMDiNA,FUN= sum)
pdf(file= "histTSPD2.pdf")
with(TSPD2,hist(steps,
	main= "Histogram for Total Steps per day (filling missings)",
	xlab= "Total # Steps/day",
	border= "blue",
	col= "green",
	breaks= 5))
dev.off()
meanTSPD2 <- mean(TSPD2$steps,na.rm = TRUE)
medianTSPD2 <- median(TSPD2$steps,na.rm = TRUE)

#8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
AMDiNA$FacWeek <- as.factor(ifelse(weekdays(AMDiNA$date) == "Sunday" | weekdays(AMDiNA$date) == "Saturday","weekend","weekday"))
ADSPINAWD <- aggregate(steps~interval,data= subset(AMDiNA,AMDiNA$FacWeek == "weekday"),FUN= mean)
ADSPINAWE <- aggregate(steps~interval,data= subset(AMDiNA,AMDiNA$FacWeek == "weekend"),FUN= mean)
ADSPINAWD$FacWeek <- "weekday"
ADSPINAWE$FacWeek <- "weekend"
ADSPINAW <- rbind(ADSPINAWD,ADSPINAWE)
library(lattice)
pdf(file= "plotADSPINA.pdf")
xyplot(steps~interval|FacWeek, data= ADSPINAW, xlab= "5-minute interval (a day)", ylab= "Steps Average", layout= c(1,2), type= "l")
dev.off()





