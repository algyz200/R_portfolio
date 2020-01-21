#Algis Toleikis 2015
#plot optical trapping traces
library("zoo")

#filename0 <- c("12_cor","2_cor") #if only number, dont need "", but if 7_cor you need to put it like "7_cor"

filename0 <- c("5_cor") #if only number, dont need "", but if 7_cor you need to put it like "7_cor"


for (i in filename0) {

#directory <- "C:/Users/algis/Desktop/Algis/Algis_UoW_data/170317"; olddirectory <- directory
#if (i == filename0[2]) {
#directory <- "C:/Users/algis/Desktop/Algis/Algis_UoW_data/170112"; olddirectory <- directory
#}

directory <- "C:/Users/algis/Desktop/Algis/Algis_UoW_data/170307"; olddirectory <- directory

setwd(directory)

filename <- paste(i, ".txt", sep="")

n <- 20

#plotting parametres
#minx <- 15.5 #doesn't like if it is 0
#maxx <- 100

minx <- 35 #doesn't like if it is 0
maxx <- 120.5

int <- 8  #lines
rotation <- 0

#stepfinder parametres
stepfind <- 0
box <- 0.008
thr <- 30
minstep <- 5 #nm
minF <- 3 #pN, later recalc as mindisp in nm
stepdif <- 5 #nm

dir <- regmatches(directory, regexpr("data.+$", directory)); dir <- paste(dir, filename, sep =" ")


#how many lines to skip (all metadata)
a <- file(filename,open="r")
lines <- readLines(a)
lntoskip <- grep("Time", lines)
#read in time, x and y
myfile <- read.delim(filename, skip = lntoskip-1)
time <- myfile[,1]; y <- myfile[,3]; x <- myfile[,2]
#maxx <- max(time)

##############################
directory <- "C:/Users/algis/Desktop/Algis/Algis_UoW/my_papers"; olddirectory <- directory
setwd(directory)
tiff(paste(i, ".tiff"), width = 10, height = 10, units = 'in', res = 300,compression="lzw")
##############################

#rotate
rot <- rotation/180*pi
x <- x*cos(rot) - y*sin(rot)
y <- x*sin(rot) + y*cos(rot)

##filtering
ma <- function(x,n){filter(x,rep(1/n,n), sides=2)} #moving average
yfilt <- ma(y, n)
xfilt <- ma(x, n)

time3 <- time[time>minx & time<maxx]
yfilt3 <- yfilt[time>minx & time<maxx]

miny <- round(min(y[time>minx & time<maxx]))
maxy <- round(max(y[time>minx & time<maxx]))

miny <- -20
maxy <- 130

#stiffness
lnk <- grep("Stiffness", lines)
kw <- unlist ( regmatches(lines[lnk], regexec( "Stiffness: (0.[0-9]+)", lines[lnk])) ); kw <- as.numeric(kw[2])
lnlaser <- grep("Laser", lines)
laser <- unlist ( regmatches(lines[lnlaser], regexec( "Laser: ([0-9]+)", lines[lnlaser])) ); laser <- as.numeric(laser[2])
k <- kw*laser/1000; 
mindisp <- minF/k

force <- y*k 

#frequency
lnfreq <- grep("Hertz", lines)
freq <- unlist ( regmatches(lines[lnfreq], regexec( "Hertz =  ([0-9]+)", lines[lnfreq])) ); freq <- as.numeric(freq[2])

########### stepfinder

if (stepfind == 1) {

directory <- "C:/Users/algis/Desktop/Algis/Algis_UoW/R"
setwd(directory) 

source("stepfinder3.R")
b <- stepfinder(yfilt3, time3, box, minstep, mindisp, freq)

a5 <- abs(b$tscore)>thr
bthr <- b[a5,]

source("findpeaks.R")
bthr3 <- findpeaks(bthr, box, k, minstep, stepdif)

} #if stepfinder =1

#########plots

#if (stepfind == 1) { par(mfrow = c(2,1),mar=c(5,4,4,5)+.1) }

plot(time3, yfilt3, col = "black", xlab="Time, s", ylab="Displacement, nm",
type="l", xlim=c(minx,maxx), main=dir, ylim=c(miny,maxy), yaxt="n")
at <- seq(from = miny, to = maxy, by = int)
axis(side = 2, at = at)
abline(h=seq(from=miny, to=maxy,by=int), NULL, col="gray", lty=3)


# Plot steps




if (stepfind == 1) {
count <- 1;
for (i in bthr3$y) {
xx <- bthr3$time[count]
yy1 <- bthr3$mean1[count]
yy2 <- bthr3$mean2[count]
xxx <- c(xx-box, xx, xx, xx+box)
yyy <- c(yy2, yy2, yy1, yy1)
lines(xxx, yyy, type="l", col = "green", lwd=2)
count <- count+1
}
}

## Force
if (TRUE){
par(new=T)
plot(time, force, col = rgb(0,0,0,0),axes=F, xlab="",ylab="", xlim=c(minx,maxx),
ylim=c(miny*k,maxy*k) )
axis(4)
mtext("Force, pN", side=4, line=3)
}

## t score plot
if (stepfind == 1) {
if (FALSE) {
plot(b$time, b$tscore, type="p", xlab = "Time, s", ylab = "t score")
lines(bthr3$time,bthr3$tscore, type="p", col="red", lwd="2")
curve(x*thr/x, add=T)
curve(x*thr/x*(-1), add=T)
}
}

#output file
directory <- olddirectory
setwd(directory)
#if (stepfind == 1) {
if (FALSE) {
filename2 <- "steps"
	if (i == filename0[1]) {  #write out step detection parameters
	write(directory, filename2, append=TRUE)
	write(c("Settings: box, thr, minstep, mindisp", box, thr, minstep, mindisp), filename2, append = TRUE)
	}
bthr3[,2:7] <- round(bthr3[,2:7],2); bthr3$dwell <- round(bthr3$dwell,5)
bthr3$file <- i
write.table(bthr3,filename2, append=TRUE)
}

dev.off()
}
