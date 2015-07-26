# plot for question 4

makePlot4 <- function() {
  
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  
  # merge two data frames by foreign key 'SCC', unique identifier for the source
  # **WARNING*** : expensive operation
  mergedNEIandSCC <- merge(NEI,SCC, by.x = "SCC", by.y = "SCC")
  
  library(dplyr)
  
  # filter for all observations who are coal-combusion related sources
  mergedNEIandSCC <- mergedNEIandSCC %>% filter(grepl(glob2rx("Fuel Comb - * - Coal"),EI.Sector))
  
  # make year factor variable
  mergedNEIandSCC <- mergedNEIandSCC %>% mutate(year=factor(year))
  
  library(ggplot2)
  
  plt <- qplot(year, Emissions, data = mergedNEIandSCC,
               geom="bar", stat = "identity",
               ylab = expression("PM"[2.5]*" (Tons)"), xlab = "Years", colour = EI.Sector, main = expression("PM"[2.5]*" from US Coal Combustion Sources (1999 - 2008)")) + theme_bw(base_family = "merriweather", base_size = 12)
  
  ggsave(filename = "./plot4.png")
}