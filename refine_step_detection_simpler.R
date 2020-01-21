#    INSTRUCTIONS
#run plot8.R (or earlier version)
#run this code to refine steps: add missing steps or delete false positives
#to plot the specific segment, e.g. from 10 to 25 s choose nb <- 10 and incr <- 15
#rerun as many times as you need to add more steps or delete more than 1 step.
#can delete a few steps in a row

########### Part 1 : Plot
########### Part 2 : Ask
########### Part 3 A :  add a missing step (auto-adjust box and thr)
########### Part 3 B : delete FALSE Positive steps 
########### Part 4 : plot again - refined steps
########### Part 5 : re-calc dwell times
########### Part 6 : Save TIF (if enabled)
########### Part 7 : save refined steps .txt (if enabled)

########### Part 1 : Plot

nb <- 10
incr <- 15

#for (i in seq(0,15,by=5)) {
for (i in nb) {

#minx <- i
#maxx <- i+incr

minx <- nb
maxx <- nb+incr

miny <- min(y[time>minx & time < maxx])
maxy <- max(y[time>minx & time < maxx])

plot(time3, yfilt3, col = "black", xlab="Time, s", ylab="Displacement, nm",
type="l", xlim=c(minx,maxx), main=dir, ylim=c(miny,maxy), yaxt="n")
#lines(time3,xfilt3,col = "blue")
at <- seq(from = miny, to = maxy, by = int)
axis(side = 2, at = at)
abline(h=seq(from=miny, to=maxy,by=int), NULL, col="gray", lty=3)

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

########### Part 2 : Ask

message <- "Add missing steps? No - to delete steps. Cancel - do nothing"
ans2 <- winDialog(type = c("yesnocancel"), message)

if (ans2 != "CANCEL") {
	coord <- locator()[[1]]
	print(coord)

	f <- time3 > coord[1] & time3 < coord[2]

	yfilt4 <- yfilt3[f]
	time4 <- time3[f]

}

if (ans2 == "YES") {
 
########### Part 3 A :  add a missing step (auto-adjust box and thr) 

box2 <- 0.008
thr2 <- 30

b <- stepfinder(yfilt4, time4, box2, minstep, mindisp, freq)

while (sum(b$tscore) == 0 && box2 < 0.04) {
box2 <- box2+0.004
b <- stepfinder(yfilt4, time4, box2, minstep, mindisp, freq)
}


a5 <- abs(b$tscore)>thr2

while (sum(a5) == 0 && thr2 > 10) {
thr2 <- thr2-2
a5 <- abs(b$tscore)>thr2
}


bthr <- b[a5,]
refined <- 0
stepdif <- 4
refined <- findpeaks(bthr, box2, k, minstep,stepdif)

#plot
count <- 1;
for (i in refined$y) {
xx <- refined$time[count]
yy1 <- refined$mean1[count]
yy2 <- refined$mean2[count]
xxx <- c(xx-box, xx, xx, xx+box)
yyy <- c(yy2, yy2, yy1, yy1)
lines(xxx, yyy, type="l", col = "red", lwd=2)
count <- count+1
}

message <- "Happy?"
ans <- winDialog(type = c("yesno"), message)

#refined_steps <- refined

if (ans == "YES") {
#refined_steps <- rbind(refined_steps,refined)
bthr3 <- rbind(bthr3,refined)
}

} # ans2 == YES

########### Part 3 B : delete FALSE Positive steps 

if (ans2 == "NO") {
f <- bthr3$time > coord[1] & bthr3$time < coord[2]
false_steps <- bthr3[f,]
bthr3 <- bthr3[!f,]
}


########### Part 4 : plot again - refined steps
if (ans2 != "CANCEL") {

plot(time3, yfilt3, col = "black", xlab="Time, s", ylab="Displacement, nm",
type="l", xlim=c(minx,maxx), main=dir, ylim=c(miny,maxy), yaxt="n")
#lines(time3,xfilt3,col = "blue")
at <- seq(from = miny, to = maxy, by = int)
axis(side = 2, at = at)
abline(h=seq(from=miny, to=maxy,by=int), NULL, col="gray", lty=3)

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

} #ans


#} # for loop


} # while loop, repeat

########### Part 5 : re-calc dwell times

if (TRUE) {
#old_bthr3 <- bthr3
bthr3 <- bthr3[order(bthr3$time),]
bthr3$dwell <- NA
dwell <- diff(bthr3$time)

if (length(bthr3$tscore)>1) {

s <- abs( (bthr3$mean2[2:nrow(bthr3)] - bthr3$mean1[1:(nrow(bthr3)-1)]) ) < stepdif #5nm threshold

for (i in 1:(nrow(bthr3)-1)) {
	if (s[i] == 1) {
	bthr3$dwell[i+1] <- dwell[i]
	}
}
}#if

}#if false

########### Part 6 : Save TIF (if enabled)

if (FALSE) {
setwd(olddirectory)
tiff(paste(filename, "highres_refined.tiff",sep=""), width = 300, height = 10, units = 'in', res = 300,compression="lzw")

plot(time3, yfilt3, col = "black", xlab="Time, s", ylab="Displacement, nm",
type="l", xlim=c(0,183), main=dir, ylim=c(-10,200), yaxt="n", xaxt="n")
#lines(time3,xfilt3,col = "blue")
at <- seq(from = miny, to = maxy, by = int)
axis(side = 2, at = at)
bby <- 0.2
axis(side = 1, at = seq(0,182,bby))
abline(h=seq(from=0, to=200,by=int), NULL, col="gray", lty=3)
lines(c(-50,250),c(0,0),col="red")

if (stepfind == 1) {
count <- 1;
for (i in bthr3$y) {
xx <- bthr3$time[count]
yy1 <- bthr3$mean1[count]
yy2 <- bthr3$mean2[count]
xxx <- c(xx-box, xx, xx, xx+box)
yyy <- c(yy2, yy2, yy1, yy1)
lines(xxx, yyy, type="l", col = "green", lwd=2)
#add dwell
if ( !is.na(bthr3$dwell[count]) ) {
lines(c(xx-bthr3$dwell[count],xx),c(yy2-1,yy2-1),col="magenta1",lwd=2)
}
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

dev.off()

} # if tiff

########### Part 7 : save refined steps .txt (if enabled)

#output file
directory <- olddirectory
setwd(directory)

if (TRUE) {
dir_split <- unlist(strsplit(olddirectory,"/"))
dir <- paste(directory, filename, sep ="/")
filename2 <- paste( dir_split[length(dir_split)],unlist(strsplit(filename,"[.]"))[1],sep="_" )
filename2 <- paste(filename2, "_steps_refined.txt",sep="")
write(dir, filename2, append=FALSE)
write(c("Settings: box, thr, minstep, mindisp", box, thr, minstep, mindisp), filename2, append = TRUE)
bthr3[,2:7] <- round(bthr3[,2:7],2); bthr3$dwell <- round(bthr3$dwell,5)
write.table(bthr3,filename2, append=TRUE)
}

print(paste("box: ", box2))
print(paste("thr: ", thr2))

if (sum(b$tscore) == 0) {
print("No steps could be found")
}

