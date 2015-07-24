
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
  
  # formulate a data frame
  xs2 <- unlist(xs2)
  yrs <- names(xs2)
  names(xs2) <- 1:4
  df1 <- data.frame(yrs,xs2)
  
  # generate a line plot as a png
  
  png("./plot1.png", width=480, height=480)
  
  # initialize line plot
  with(df1, plot(as.numeric(as.character(yrs)),as.numeric(xs2), 
                 type = "l", xlab = "Years", ylab = "PM2.5 (Millions of Tons)", yaxt = "n", xaxt = "n"))
  
  # give basic annotation
  title(main = "US Emissions of PM2.5 Over a Decade (1999 - 2008)")
  axis(2, at=seq(from = 0, to = 7e+06, by = 1e+06), labels=c(0:7))
  axis(1, at=c(1999,2002,2005,2008))
  
  dev.off()
  
}
