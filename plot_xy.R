

library(plotly)

#for (i in seq(0.1,180,by=20)) {

minx <- 30
maxx <- 35
rotation <- 0 #degress
rot <- rotation/180*pi

yfiltt <- yfilt3[time3>minx & time3<maxx]
xfiltt <- xfilt3[time3>minx & time3<maxx]
timee <- time3[time3>minx & time3<maxx]

yfiltt <- yfilt3
xfiltt <- xfilt3
timee <- time3

x2 <- xfiltt*cos(rot) - yfiltt*sin(rot)
y2 <- xfiltt*sin(rot) + yfiltt*cos(rot)

directory <- "C:/Users/algis/Desktop/Algis/Algis_UoW_data/160521"; olddirectory <- directory
setwd(directory)

#tiff(paste(filename, as.character(i), "rot",rotation, ".tiff"), width = 15, height = 10, units = 'in', res = 800,compression="lzw")

#par(mfrow = c(1,2),mar=c(5,4,4,5)+.1)
#plot(timee, y2, type="l",main=dir, xlab="time, s", ylab="y, nm", ylim=c(0,75))
#lines(timee, x2-20, type="l",main=dir, xlab="time, s", ylab="y, nm", col = "blue")

directory <- "C:/Users/algis/Desktop/Algis/Algis_UoW/R"
setwd(directory) 
source("plot_colorByDensity.R")

#plot_colorByDensity(x2,y2,xlab="x, nm",ylab="y, nm",xlim=c(-40,40),ylim=c(30,100))
#plot_colorByDensity(x2,y2,xlab="x, nm",ylab="y, nm")

#dev.off()
#}

library("plot3D")
library("plot3Drgl")

scatter3D(xfiltt, yfiltt, timee, type="l",ticktype = "detailed")
plotrgl()

setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/weird_things_noticed/ATPyS_substep/half_back")
