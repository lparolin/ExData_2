# Code for plot 5

totalPerYear <- function(year) {
    # Compute the total amount of Pm2.5 per year 
    print(paste("Using year:", as.character(year)))
    idx.year <- subData$year == year
    # we look for Baltimore fips == 24510
    idx.city <- subData$fips==24510
    idxToUse <- idx.year & idx.city
    subValue <- subData$Emissions[idxToUse]
    print(paste("Total:", as.character(sum(subValue))))
    sum(subValue)
}

inData <- function(inData) {
    
}

#load the library
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# First find all indexes related with combusions
# these have EI Sector = 
# "Mobile - On-Road Diesel Heavy Duty Vehicles"  or 
# "Mobile - On-Road Diesel Light Duty Vehicles" or 
# "Mobile - On-Road Gasoline Heavy Duty Vehicles" or 
# "Mobile - On-Road Gasoline Light Duty Vehicles" 
idx.ei.sector <- (SCC$EI.Sector == "Mobile - On-Road Diesel Heavy Duty Vehicles") |
    (SCC$EI.Sector == "Mobile - On-Road Diesel Light Duty Vehicles") | 
    (SCC$EI.Sector == "Mobile - On-Road Gasoline Heavy Duty Vehicles") | 
    (SCC$EI.Sector == "Mobile - On-Road Gasoline Light Duty Vehicles")
# reduce data set
scc.mobile <- as.vector(SCC$SCC)[idx.ei.sector]
idxToUse <- rep(FALSE, length(NEI$fips))
for (iElement in scc.mobile ) {
    idxToUse <- idxToUse | (NEI$SCC == iElement)
}

subData <- NEI[idxToUse, ]
Year <- levels(as.factor(subData$year))
Emissions <- sapply(Year, totalPerYear)
finalData <- data.frame(Year, Emissions)

# Plot
png("plot5.png", width=480, height=480)
myPlot <-qplot(Year, Emissions, data = finalData)
myPlot <- myPlot + labs(y="Total emissions (Tons)") + labs(title="Emissions from motor vehicle sources in Baltimore")
print(myPlot)
dev.off()
