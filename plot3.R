Plot3 <- function(){
  library(plotrix)
# reading only the necessary records for 1/2/2007 & 2/2/2007
  dataset01 <- read.table("household_power_consumption.txt",sep = ";",header = TRUE,na.strings = "?",
                          skip=66636,nrow=2880
                          ,colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric")
                          )
names(dataset01) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
# getting rid of 'NA' if any  
  good <- complete.cases(dataset01)
  x <- dataset01[good,]

# formating the date time for x-axis
  d <- paste(as.Date(x[,1],format="%d/%m/%Y"),x[,2])
  dt <- as.POSIXct(d)

# ploting the desired graph
  with(x,{plot(x[,7]~dt,type="l",ylab="Energy sub metering",xlab="")
          lines(x[,8]~dt,col="red")
          lines(x[,9]~dt,col="blue")})
  legend("topright",col=c("black","red","blue"),lty = 1,lwd=2,
         legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

# copying the generated graph in the PNG device
  dev.copy(png,file = "plot3.png",width = 480,height = 480)
  dev.off()
}