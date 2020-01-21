setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis")
source("steps_and_dirs_compiled.R")


filelist <- c(sub)
directory <- c(d_sub)


title <- strsplit(directory,"/"); title <- unlist(title); title <- title[length(title)]

coll=1
#par(new=TRUE) #allows overlaying plots

#exclude false dwells
setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/")
falsedwells <- read.table("false_dwells.txt")
falsesteps <- read.table("false_steps.txt")

#

setwd(directory)

slopes <- c(); intercepts <- c(); c <- list(); dffit2 <- c()
minback <- -12
interval <- 1
ffrom <- 2
fto <- 9
d_lim <- 20 # how close the detachment has to be to zero position to count as detachment and not backslip

# read in
bthr3 <- read.table(filelist[1], skip = 6,sep=" ") 
bthr3$file <- rep(as.character(read.table(filelist[1],nrow=1)[1,1]),nrow(bthr3))

for (i in filelist[2:length(filelist)]) {

bthr3_temp <- read.table(i, skip = 6,sep=" ")
bthr3_temp$file <- rep(as.character(read.table(i,nrow=1)[1,1]),nrow(bthr3_temp))
bthr3 <- rbind(bthr3, bthr3_temp)

}

bthr3 <- bthr3[!((bthr3$time %in% falsedwells$time) & (bthr3$tscore %in% falsedwells$tscore)),]
bthr3 <- bthr3[!((bthr3$time %in% falsesteps$time) & (bthr3$tscore %in% falsesteps$tscore)),] #remove false steps


##### binning

avF <- c(); avD <- c(); avA <- c(); nfor <- c(); nback <- c(); ndrop <- c();
se <- c(); vel <- c(); avDf <- c(); avDb <- c(); avDd <- c(); nbackall <- c();nslip <- c();


for (i in seq(ffrom,fto,by=interval)) {

p <- bthr3$amplitude> minback & bthr3$force >= i-interval/2 & bthr3$force < i+interval/2 #back and for
a <- bthr3$amplitude>0 & bthr3$force >= i-interval/2 & bthr3$force < i+interval/2 #for
b <- bthr3$amplitude<0 & bthr3$amplitude> minback & bthr3$force >= i-interval/2 & bthr3$force < i+interval/2 #back
#d <- bthr3$amplitude < minback & bthr3$force >= i-interval/2 & bthr3$force < i+interval/2 #drop
d <- bthr3$mean1 < d_lim & bthr3$amplitude < 0 & bthr3$force >= i-interval/2 & bthr3$force < i+interval/2 # Detachments
bb <- bthr3$mean1 > d_lim & bthr3$amplitude < 0 & bthr3$amplitude > -48 & bthr3$force >= i-interval/2 & bthr3$force < i+interval/2 #all backsteps without detachment
bs <- bthr3$mean1 > d_lim & bthr3$amplitude < 0 & bthr3$amplitude < -12 & bthr3$force >= i-interval/2 & bthr3$force < i+interval/2 # more than -12 but no detachments

#avF <- c(avF, mean(round(bthr3$force[p]), na.rm=TRUE) )
avF <- c(avF, i)

dwell <- bthr3$dwell[p]; dwell <- dwell[!is.na(dwell)]
avD <- c(avD, mean(bthr3$dwell[p], na.rm=TRUE) )
avDf <- c(avDf, mean(bthr3$dwell[a], na.rm=TRUE) )
avDb <- c(avDb, mean(bthr3$dwell[b], na.rm=TRUE) )
avDd <- c(avDd, mean(bthr3$dwell[d], na.rm=TRUE) )
#se  <- c(se, sd(bthr3$dwell[p], na.rm=TRUE)/sqrt(length(bthr3$dwell[p])))

avA <- c(avA, mean(bthr3$amplitude[p], na.rm=TRUE) )
se <- c(se, sd( bthr3$amplitude[p]/bthr3$dwell[p], na.rm=TRUE ) / sqrt(length(dwell)))


nfor <- c( nfor, length(bthr3$amplitude[a]) )
nback <- c( nback, length(bthr3$amplitude[b]) )
nbackall <- c( nbackall, length(bthr3$amplitude[bb]) )
ndrop <- c( ndrop, length(bthr3$amplitude[d]) )
nslip <- c( nslip, length(bthr3$amplitude[bs]) )

}
#####

####################### plotting

#plot(avF,avD, type="p",col="red")
#plot(avF, avA)
#plot(avF, avA/avD, xlim=c(3,9), ylim=c(-50,250), xlab="Force, pN",
#ylab="Velocity, nm/s")


#plot(avF, nfor/nback, type="p", col = coll, xlim=c(ffrom,fto), log="y",ylim=c(0.1,200),
#xlab="Force, pN", ylab="Forestep / Backstep ratio", xaxt="n")
#axis(1,seq(0,10,1))

#plot(avF, nfor/(nback+nfor+ndrop), type="b", col = "black", xlim=c(ffrom,fto),ylim=c(0,1),
#xlab="Force, pN", ylab="Probability",lty=2)

all <- nfor+nback+nslip+ndrop

t <- matrix(nfor/all ,byrow=F,nrow=1,ncol=fto-ffrom+1)
t <- rbind(t,nback/all)
t <- rbind(t,nslip/all)
t <- rbind(t,ndrop/all)

par(mar=c(5,10,5,2))
layout(matrix(c(1,1,2), 3, 1, byrow=T))

barplot(t,names.arg=avF,xlab="Force, pN",ylab="Probability",
legend.text=c("Foresteps","Backsteps smaller than 12 nm","Backsteps bigger than 12 nm","Detachments"),
args.legend = list(x = 10, y=1.2,cex=1,border=F,bty="n"),
main=title,col=c(1,"orange","cyan","grey"),
border=F,cex.lab=2,cex.axis=2,cex.names=2)

################################## write out number of steps

par(mar=c(10,10,3,2))

a1 <- 2.5
a2 <- 8.5
nn <- 7

plot(avF,c(1,2,3,4,5,12,1,2), axes=F, xlab="",ylab="",col="white")
#plot(1:10, axes=F, xlab="",ylab="",col="white")

mmm <- matrix(c(seq(a1,a2,by=(a2-a1)/nn),rep(11,length(avF))),length(avF),2)
text(mmm,as.character(ndrop),col=1,adj=1)
mmm <- matrix(c(seq(a1,a2,by=(a2-a1)/nn),rep(8,length(avF))),length(avF),2)
text(mmm,as.character(nslip),col=1,adj=1)
mmm <- matrix(c(seq(a1,a2,by=(a2-a1)/nn),rep(5,length(avF))),length(avF),2)
text(mmm,as.character(nback),col=1,adj=1)
mmm <- matrix(c(seq(a1,a2,by=(a2-a1)/nn),rep(2,length(avF))),length(avF),2)
text(mmm,as.character(nfor),col=1,adj=1)

mmm <- matrix( c(rep(-0.1,4),2,5,8,11),4,2)
text(mmm, c("Foresteps","Backsteps smaller than 12 nm","Backsteps bigger than 12 nm","Detachments"),xpd=NA,adj=0)

####################################


#lines(avF, nfor/(nback+nfor+ndrop), type="l", lty=2, col = "black", xlim=c(3,8),ylim=c(0,1), main="Pombe")
#lines(avF, nback/(nback+nfor+ndrop), type="l",lty=2, col = "red", xlim=c(3,8),ylim=c(0,1))
#lines(avF, ndrop/(nback+nfor+ndrop), type="l",lty=2, col = "blue", xlim=c(3,8),ylim=c(0,1))


#plot(avF,nfor,type="l")
#lines(avF,nback, col="blue")

#plot(bthr3$force, bthr3$amplitude, ylim=c(-200,50),xlim=c(ffrom,fto),pch=16,cex=0.4,col=coll)
#plot(bthr3$force, bthr3$dwell, ylim=c(0.001,10),xlim=c(ffrom,fto),log="y",col=coll)
#plot(avF,avDf,type="p",col=coll,lwd=6)
#plot(avF,avDf, ylim=c(0.001,10),xlim=c(ffrom,fto),log="y",col=coll)

#######################

directory2 <- "C:/Users/algis/Desktop/Algis/Algis_UoW/R/functions"; setwd(directory2); source("fit_log.R");
source("fit_log_w.R") 


#fit_log(avF, nfor/nback, coll)
#fit_log_w(avF, nfor/nback, "pink",w) # w <- r/error

#fit_log(avF, nfor/(nback+ndrop), coll)

#text(6.5, 55, "Pig MT + GMPCPP + epothilone",col=1)
#text(6.5, 50, "round - F/B",col=coll)
#text(6.5, 30, "square - F/(B+D)",col=coll)

#lines(1:10,rep(1,10),col="grey")
 
################# tony's type of erros bars

r <- nfor/nback

error <- sqrt( r*(1+r)/nback )


lower <- r-error
upper <- r+error

#arrows(ffrom:fto, lower, ffrom:fto, upper, length=0.05, angle=90, code=3,
#col=coll)


setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/compare/cumultative_histograms/with numbers")



