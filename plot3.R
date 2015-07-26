# plot for question 3

makePlot3 <- function() {
  
  NEI <- readRDS("summarySCC_PM25.rds")
  # SCC <- readRDS("Source_Classification_Code.rds")
  
  library(dplyr)
  
  # filter for all row observations corresponding to Baltimore City, Maryland
  baltimoreSources <- NEI %>% filter(fips == "24510")
  
  # convert type and years to factor variables
  baltimoreSources <- baltimoreSources %>% mutate(type=factor(type)) %>% mutate(year=factor(year))
  
  
  require(ggplot2)
  
  plt <- qplot(year, Emissions, data = baltimoreSources,
               geom="bar", stat = "identity",
               ylab = "PM2.5 (Tons)", xlab = "Years", facets = .~type)
  
  ggsave(filename = "./plot3.png")
  
}