#opens original data file and plots the specified step
#run steps_compare.R first

#click on suspicious data point, then right click and STOP
#the raw data is then opened for that step

#How to identify a false dwell?
# 1. There are steps missing in between
# 2. If no doubt, just exclude that data point (clasify as false dwell)

bthr4 <- bthr3

##only some steps, like foresteps at 8 pN etc
#f <- bthr3$force > 4.5 & bthr3$force < 5.5 & !is.na(bthr3$dwell) & bthr3$amplitude < 0 & bthr3$amplitude > -12 # backsteps
#f <- bthr3$force > 2.5 & bthr3$force < 3.5 & !is.na(bthr3$dwell) & bthr3$amplitude > 0  # foresteps
#f <- bthr3$mean1 > 20 & bthr3$amplitude < 0 & bthr3$amplitude < -20 # big backsteps
#f <- bthr3$force > 3.5 & bthr3$force < 4.5 & !is.na(bthr3$dwell) & bthr3$amplitude < 0 & bthr3$amplitude > -12 # backsteps
#f <- bthr3$force > 5.5 & bthr3$force < 6.5 & !is.na(bthr3$dwell) & bthr3$amplitude < 0 & bthr3$amplitude > -12 # backsteps
#f <- bthr3$force > 4.5 & bthr3$force < 5.5 & !is.na(bthr3$dwell) & bthr3$amplitude < 0 & bthr3$mean1 < 20 # detachments?
f <- bthr3$force > 0 #all steps




bthr4 <- bthr3[f,]
##

aaa <- bthr4[order(bthr4$dwell),]

aaa1 <- aaa[!is.na(aaa$dwell),]

#load steps that have already been checked
setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/")
done <- read.table("dwells_checked.txt",row.names=NULL)
aaa2 <- aaa1[!((aaa1$time %in% done$time) & (aaa1$tscore %in% done$tscore)),] #already checked
#aaa2 <- aaa1 #look at the steps again (do not exclude the checked ones) 

#

n <- 50
zzz <- (length(aaa2$dwell)-n+1) : length(aaa2$dwell)
zzz <-  length(aaa2$dwell):(length(aaa2$dwell)-n+1)

last <- aaa2[zzz,]

for (i in zzz) {

#plot(bthr3$force,bthr3$dwell)

#q <- identify(bthr3$force,bthr3$dwell,label="x",col="green",offset=0,cex=1)

#qstep <- (bthr3)[q,]
qstep <- (aaa2)[i,]


file_dir <- qstep$file
file_dir2 <- regmatches(file_dir, regexpr("(.+/.+)/", file_dir))
setwd(file_dir2)
file <- regmatches(file_dir, regexpr(".+/.+/", file_dir),invert=T)
filename <- unlist(file)[2]

setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/R/functions")
source("plot7_function.R")

fff <- substr(file_dir2,1,nchar(file_dir2)-1)

steps <- plot_function (qstep$time-qstep$dwell-0.3, qstep$time+0.5, filename, file_dir2, 0)
lines(rep(qstep$time,200),1:200,col="red",lty=2)
xx <- qstep$time
yy2 <- qstep$mean2
lines(c(xx-qstep$dwell,xx),c(yy2-1,yy2-1),col="magenta1",lwd=2)
print(qstep)
print(length(aaa2$dwell)-i+1)
setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/weird_things_noticed/pombe")

ans <- "x"
skip <- 0

while (ans != "y" & ans != "n" & ans != "f" & ans != "s") {
ans <- readline("False dwell?, y - yes, n - no, f - completely false step, s - skip: ")
	if (ans == "y") {
	setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/")
	#write.table(qstep, "false_dwells.txt", append=T)
	write.table(qstep, "false_dwells.txt", append=T,col.names=F)
	}
	if (ans == "f") {
	setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/")
	#write.table(qstep, "false_steps.txt", append=T)
	write.table(qstep, "false_steps.txt", append=T,col.names=F)
	}
	if (ans == "s") {
	print("step skipped")
	skip <- 1
	}
}
setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/")

if (skip == 0) {
write.table(qstep, "dwells_checked.txt", append=T,col.names=F)
}

}

      
