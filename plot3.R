# Code to generate the third plot

#load the library
library(ggplot2)

# Read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI$yearFactor <- as.factor(NEI$year)
NEI$SourceFactor <- as.factor(NEI$SCC)
idxToUse <- NEI$fips == 24510
Baltimore <- data.frame(NEI$Emissions[idxToUse], 
                        NEI$type[idxToUse],
                        as.factor(NEI$year[idxToUse]))
names(Baltimore) <- c("Emissions", "Type", "Year")
nYears <- length(levels(Baltimore$Year))
nTypes <- length(levels(Baltimore$Type))
Year <- rep(3, nTypes * nYears)
Type <- rep(3, nTypes * nYears)
Emissions <- rep(3, nTypes * nYears)
counter <- 1
for (iYear in levels(Baltimore$Year)) {
    idxYear <- Baltimore$Year == iYear
    for (iType in levels(Baltimore$Type)) {
        Type[counter] <- iType
        Year[counter] <- iYear
        idxType <- Baltimore$Type == iType
        Emissions[counter] <- sum(Baltimore$Emissions[idxType & idxYear])
        counter <- counter + 1
    }
}
BaltimoreSummary <- data.frame(Emissions, Type, Year)

png("plot3.png", width=480, height=480)
myPlot <-qplot(Year, Emissions, data = BaltimoreSummary, color = Type)
myPlot <- myPlot + labs(y="Total emissions (Tons)")
print(myPlot)
dev.off()
