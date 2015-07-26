# plot for question 2

makePlot2 <- function() {
  
  ## This first line will likely take a few seconds. Be patient!
  NEI <- readRDS("summarySCC_PM25.rds")
  # SCC <- readRDS("Source_Classification_Code.rds")
  
  library(dplyr)
  
  # filter for all row observations corresponding to Baltimore City, Maryland
  baltimoreSources <- NEI %>% filter(fips == "24510")
  
  # select for year and Emissions collumns
  baltimoreSources <- baltimoreSources %>% select(year,Emissions)
  
  # group by year
  baltimoreSources <- baltimoreSources %>% group_by(year)
  
  # obtain the sum total of PM2.5 emissions for each year in Baltomore City, Maryland
  baltimoreSources <- baltimoreSources %>% summarise_each(funs(sum(Emissions)))
  
  # plot line graph
  
  # generate a line plot as a png
  png("./plot2.png", width=480, height=480)
  
  # initialize line plot
  with(baltimoreSources, plot(year,Emissions,
                 type = "l", xlab = "Years", ylab = "PM2.5 (Tons)", yaxt = "n", xaxt = "n"))
  
  # give basic annotation
  title(main = "Baltimore City's Emissions of PM2.5 Over a Decade (1999 - 2008)")
  axis(1, at=c(1999,2002,2005,2008))
  axis(2, at=c(3500,3000,2500,2000,1500))
  
  dev.off()
  
}