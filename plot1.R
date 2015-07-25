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
TonsPerYear<- sapply(year.level, totalPerYear)

png("plot1.png", width=480, height=480)
plot(names(TonsPerYear), TonsPerYear, xlab="Year", ylab="Emitted PM 2.5 (Tons)", main="")
dev.off()
