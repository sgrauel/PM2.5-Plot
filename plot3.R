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
               ylab = expression("PM"[2.5]*" (Tons)"), xlab = "Years", 
               facets = .~type, fill = type, main = expression("4 Types of PM"[2.5]*" Polluters in the Baltimore City, Maryland Area (1999 - 2008)")) + theme_bw(base_family = "merriweather", base_size = 12)
  
  ggsave(filename = "./plot3.png")
  
}