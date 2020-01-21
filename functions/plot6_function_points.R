#Algis Toleikis 2015
#plot optical trapping traces
#~2016 step finder implemented
#2017 04 02 quick file read in

plot6_function <- function (minx, maxx, filename, directory, stepfind) {

#filename <- "6_cor.txt"; 
#directory <- "C:/Users/algis/Desktop/Algis/Algis_UoW_data/170318"; 
olddirectory <- directory

n <- 20

#plotting parametres
#minx <- 0 #doesn't like if it is 0
#maxx <- 5
int <- 8  #lines
rotation <- 0

#stepfinder parametres
#stepfind <- 0
box <- 0.008
thr <- 30
minstep <- 5 #nm
minF <- 1 #pN, later recalc as mindisp in nm
stepdif <- 5 # nm

dir <- regmatches(directory, regexpr("data.+$", directory)); dir <- paste(dir, filename, sep =" ")
setwd(directory)

#quick file read in
a <- file(filename,open="r")
lines <- readLines(a,n=10000)
lntoskip <- grep("Time", lines) #how many lines to skip (all metadata)

#frequency
lnfreq <- grep("Hertz", lines)
freq <- unlist ( regmatches(lines[lnfreq], regexec( "Hertz =  ([0-9]+)", lines[lnfreq])) ); freq <- as.numeric(freq[2])

init_time <- as.numeric(strsplit(lines[lntoskip+1],"\t")[[1]][1])
read_from <- minx*freq + lntoskip - init_time*freq	#open only from the certain line
read_to <- maxx*20161 + lntoskip - init_time*freq	#open file to the target line

#read in file as a table to get time, x and y
if (read_from > 0) {
myfile <- read.delim(filename, skip = read_from, header=T,stringsAsFactors=FALSE,
colClasses=c("numeric","numeric","numeric"),nrow=read_to )
} else {
myfile <- read.delim(filename, skip = lntoskip-1, header=T,stringsAsFactors=FALSE,
colClasses=c("numeric","numeric","numeric") )
}

time <- myfile[,1]; y <- myfile[,3]; x <- myfile[,2]

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
xfilt3 <- xfilt[time>minx & time<maxx]

miny <- round(min(y[time>minx & time<maxx]))
maxy <- round(max(y[time>minx & time<maxx]))

#stiffness
lnk <- grep("Stiffness", lines)
kw <- unlist ( regmatches(lines[lnk], regexec( "Stiffness: (0.[0-9]+)", lines[lnk])) ); kw <- as.numeric(kw[2])
lnlaser <- grep("Laser", lines)
laser <- unlist ( regmatches(lines[lnlaser], regexec( "Laser: ([0-9]+)", lines[lnlaser])) ); laser <- as.numeric(laser[2])
k <- kw*laser/1000; mindisp <- minF/k

force <- y*k 

########### stepfinder

if (stepfind == 1) {

directory <- "C:/Users/algis/Desktop/Algis/Algis_UoW/R"
setwd(directory) 

source("stepfinder3.R")
b <- stepfinder(yfilt3, time3, box, minstep, mindisp, freq)

a5 <- abs(b$tscore)>thr
bthr <- b[a5,]

source("findpeaks.R")
bthr3 <- findpeaks(bthr, box, k, minstep,stepdif)

} #if stepfinder =1

#########plots

#if (stepfind == 1) { par(mfrow = c(2,1),mar=c(5,4,4,5)+.1) }

plot(time3, yfilt3, col = "black", xlab="Time, s", ylab="Displacement, nm",
type="p", xlim=c(minx,maxx), main=dir, ylim=c(miny,maxy), yaxt="n",cex=0.5)
#lines(time3,xfilt3,col = "blue")
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
dir <- paste(directory, filename, sep ="/")
filename2 <- "170318_1_steps.txt"
write(dir, filename2, append=TRUE)
write(c("Settings: box, thr, minstep, mindisp", box, thr, minstep, mindisp), filename2, append = TRUE)
bthr3[,2:7] <- round(bthr3[,2:7],2); bthr3$dwell <- round(bthr3$dwell,5)
write.table(bthr3,filename2, append=TRUE)
}

#return(bthr3)
#return(yfilt3)
#print(data.frame(1:length(yfilt3),yfilt3))

} # funtion loop

