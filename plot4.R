# Code for plot 4

totalPerYear <- function(year) {
    # Compute the total amount of Pm2.5 per year 
    print(paste("Using year:", as.character(year)))
    idx.year <- subData$year == year
    subValue <- subData$Emissions[idx.year]
    print(paste("Total:", as.character(sum(subValue))))
    sum(subValue)
}


#load the library
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# First find all indexes related with combusions
# these have EI Sector = "Fuel Comb - Comm/Institutional - Coal"  or 
# "Fuel Comb - Electric Generation - Coal" or 
# "Fuel Comb - Industrial Boilers, ICEs - Coal"
idx.ei.sector <- (SCC$EI.Sector == "Fuel Comb - Comm/Institutional - Coal") |
    (SCC$EI.Sector == "Fuel Comb - Electric Generation - Coal") | 
    (SCC$EI.Sector == "Fuel Comb - Industrial Boilers, ICEs - Coal")

# reduce data set
scc.coal <- SCC$SCC[idx.ei.sector]
idxToUse <- rep(FALSE, length(NEI$fips))
for (iScc in scc.coal) {
    idxToUse <- idxToUse | (NEI$SCC == iScc)
}

subData <- NEI[idxToUse, ]
Year <- levels(as.factor(subData$year))
Emissions <- sapply(Year, totalPerYear)
for (counter in 1 : length(Year)) {
    Emissions[counter] <- totalPerYear(Year[counter])
}

finalData <- data.frame(Year, Emissions)

# Plot
png("plot4.png", width=480, height=480)
myPlot <-qplot(Year, Emissions, data = finalData)
myPlot <- myPlot + labs(y="Total emissions (Tons)") + labs(title="Emissions from coal combustion-related sources")
print(myPlot)
dev.off()
