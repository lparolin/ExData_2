# Code to generate the first plot

totalPerYear <- function(year) {
    # Compute the total amount of Pm2.5 per year 
    print(paste("Got year:", as.character(year)))
    idxToUse <- (NEI$yearFactor == year)
    subValue <- NEI$Emissions[idxToUse]
    sum(subValue)
}

# Read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI$yearFactor <- as.factor(NEI$year)
NEI$SourceFactor <- as.factor(NEI$SCC)
year.level <- levels(NEI$yearFactor)
tonsPerYear<- sapply(year.level, totalPerYear)
myModel <- lm(tonsPerYear ~ as.numeric(year.level))

png("plot1.png", width=480, height=480)
plot(names(tonsPerYear), tonsPerYear, xlab="Year", ylab="PM 2.5 (Tons)", main="")
abline(myModel, col="red")
legend(2006,6.8e6, c("raw data", "intercept"), lty=c(0,1), pch=c(1,-1), col = c("black", "red"))
title("Total PM2.5 emission in the United States from 1999 to 2008")
dev.off()
