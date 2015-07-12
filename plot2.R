Plot2 <- function(){
  library(plotrix)
# reading only the necessary records for 1/2/2007 & 2/2/2007
  dataset01 <- read.table("household_power_consumption.txt",sep = ";",header = TRUE,na.strings = "?",
                          skip=66636,nrow=2880,colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric"))

# getting rid of '?' if any  
  good <- complete.cases(dataset01)
  x <- dataset01[good,]

# formating the date time for x-axis
  d <- paste(as.Date(x[,1],format="%d/%m/%Y"),x[,2])
  dt <- as.POSIXct(d)

# ploting the desired graph
  plot(x[,3] ~ dt,type="l",ylab="Global Active Power(kilowatts)",xlab = "")

# copying the generated graph in the PNG device
  dev.copy(png,file = "plot2.png",width = 480,height = 480)
  dev.off()
}