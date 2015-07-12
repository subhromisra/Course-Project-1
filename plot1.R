Plot1 <- function(){
  library(datasets)
# reading only the necessary records for 1/2/2007 & 2/2/2007
  dataset01 <- read.table("household_power_consumption.txt",sep = ";",header = TRUE,
                          skip=66636,nrow=2880,colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric"))

# getting rid of '?' if any  
  good <- complete.cases(dataset01)
  x <- dataset01[good,]

# Plotting the histogram using base graphics
  hist(x[,3],col = "red",main = "Global Active Power",xlab = "Global Active Power(kilowats)")

# copying the generated graph in the PNG device
  dev.copy(png,file = "plot1.png",width = 480,height = 480)
  dev.off()
}