
# plot for question 1
# TASK: generate a line plot showing a negative correlation
# between PM2.5 and years

makePlot1 <- function() {
  
  ## This first line will likely take a few seconds. Be patient!
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  # split data into groups by year and apply sum
  xs1 <- split(NEI$Emissions,NEI$year)
  xs2 <- lapply(xs1,sum)
  
  yrs <- names(xs2)
  xs2 <- as.numeric(xs2)
  
  # generate a line plot as a png
  png("./plot1.png", width=480, height=480)
  
  # initialize line plot
  bp <- barplot(xs2, col = "red", xlab = "Years", ylab = expression("PM"[2.5]*" (Millions of Tons)"), axisnames = TRUE, names.arg = yrs, yaxt = "n")
    
  # give basic annotation
  title(main = expression("US Emissions of PM"[2.5]*" Over a Decade (1999 - 2008)"))
  points(x = bp, y = xs2, pch = 20, col = "blue")
  lines(x = bp, y = xs2)
  axis(2, at=seq(from = 0e+00, to = 7e+06, by = 1e+06), labels=c(0:7))
  
  dev.off()
  
}