#opens multiple steps analysis files extracted from time-series files and pools them together
#the resulting data frame is analysed with various statistics

setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis")
source("steps_and_dirs_compiled.R")

filelist <- c(sub)
directory <- c(d_sub)

layout(matrix(c(1, 1, 1, 2), 1, 1))

title <-
  strsplit(directory, "/")
title <- unlist(title)
title <- title[length(title)]

coll = "cadetblue3"
#coll=4

par(new = TRUE) #allows overlaying plots

#exclude false dwells
setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/")
falsedwells <- read.table("false_dwells.txt")
falsesteps <- read.table("false_steps.txt")

#

setwd(directory)

slopes <- c()
intercepts <- c()
c <- list()
dffit2 <- c()
minback <- -12
interval <- 1
ffrom <- 2
fto <- 9

# read in
bthr3 <- read.table(filelist[1], skip = 6, sep = " ")
bthr3$file <-
  rep(as.character(read.table(filelist[1], nrow = 1)[1, 1]), nrow(bthr3))

for (i in filelist[2:length(filelist)]) {
  bthr3_temp <- read.table(i, skip = 6, sep = " ")
  bthr3_temp$file <-
    rep(as.character(read.table(i, nrow = 1)[1, 1]), nrow(bthr3_temp))
  bthr3 <- rbind(bthr3, bthr3_temp)
  
}

bthr3 <-
  bthr3[!((bthr3$time %in% falsedwells$time) &
            (bthr3$tscore %in% falsedwells$tscore)), ] #remove steps with false dwells
bthr3 <-
  bthr3[!((bthr3$time %in% falsesteps$time) &
            (bthr3$tscore %in% falsesteps$tscore)), ] #remove false steps


##### binning

avF <-
  c()
avD <- c()
avA <- c()
nfor <- c()
nback <- c()
ndrop <- c()

se <-
  c()
vel <-
  c()
avDf <-
  c()
avDb <-
  c()
avDd <- c()
nbackall <- c()
nslip <- c()
nall <- c()
nDf <- c()
nDb <- c()
nDdbb <- c()



for (i in seq(ffrom, fto, by = interval)) {
  p <-
    bthr3$amplitude > minback &
    bthr3$force >= i - interval / 2 &
    bthr3$force < i + interval / 2 #back and for
  a <-
    bthr3$amplitude > 0 &
    bthr3$force >= i - interval / 2 & bthr3$force < i + interval / 2 #for
  b <-
    bthr3$amplitude < 0 &
    bthr3$amplitude > minback &
    bthr3$force >= i - interval / 2 & bthr3$force < i + interval / 2 #back
  dbb <-
    bthr3$amplitude < minback &
    bthr3$force >= i - interval / 2 &
    bthr3$force < i + interval / 2 # Detachments+big backsteps
  
  d <-
    bthr3$mean1 < 20 &
    bthr3$amplitude < 0 &
    bthr3$force >= i - interval / 2 &
    bthr3$force < i + interval / 2 # Detachments only
  bb <-
    bthr3$mean1 > 20 &
    bthr3$amplitude < 0 &
    bthr3$amplitude > -48 &
    bthr3$force >= i - interval / 2 &
    bthr3$force < i + interval / 2 #backsteps + backslips
  bs <-
    bthr3$mean1 > 20 &
    bthr3$amplitude < 0 &
    bthr3$amplitude < -12 &
    bthr3$force >= i - interval / 2 &
    bthr3$force < i + interval / 2 #backslips only
  all <-
    bthr3$force >= i - interval / 2 &
    bthr3$force < i + interval / 2 # all possible steps detected (incl detachments)
  
  avF <- c(avF, i)
  
  ccc <-
    dbb # a - foresteps, b - backsteps, d- detachments, dbb - detachments+big backsteps
  
  avD <- c(avD, mean(bthr3$dwell[ccc], na.rm = TRUE))
  #avD <- c(avD, median(bthr3$dwell[ccc], na.rm=TRUE) )
  
  dwell <- bthr3$dwell[p]
  dwell <- dwell[!is.na(dwell)]
  avDf <- c(avDf, mean(bthr3$dwell[a], na.rm = TRUE))
  avDb <- c(avDb, mean(bthr3$dwell[b], na.rm = TRUE))
  avDd <- c(avDd, mean(bthr3$dwell[d], na.rm = TRUE))
  se  <-
    c(se, sd(bthr3$dwell[ccc], na.rm = TRUE) / sqrt(length(bthr3$dwell[ccc])))
  
  avA <- c(avA, mean(bthr3$amplitude[p], na.rm = TRUE))
  
  
  nfor <- c(nfor, length(bthr3$amplitude[a]))
  nback <- c(nback, length(bthr3$amplitude[b]))
  nbackall <- c(nbackall, length(bthr3$amplitude[bb]))
  ndrop <- c(ndrop, length(bthr3$amplitude[d]))
  nslip <- c(nslip, length(bthr3$amplitude[bs]))
  #nall <- c(nall,length(bthr3$amplitude[all]) )
  nDf <- c(nDf, sum(!is.na(bthr3$dwell[a])))
  nDb <- c(nDf, sum(!is.na(bthr3$dwell[b])))
  nall <- nfor + nback + nslip + ndrop
  
  
  #plot(bthr3$force[ccc],bthr3$dwell[ccc],ylim=c(0.01,10),xlim=c(ffrom,fto),log="y", col=coll,type="p",cex=0.4,
  #,xlab="",ylab="")
  #par(new=TRUE) #allows overlaying plots
  
}
#####

####################### plotting

#plot(avF,avD, type="p",col="red")
#plot(avF, avA)
#plot(avF, avA/avD, xlim=c(3,9), ylim=c(-50,250), xlab="Force, pN",
#ylab="Velocity, nm/s")

ratio <- nfor / nback
#ratio <- nfor/(nback+ndrop+nslip)
#ratio <- ndrop/(nfor+nback+ndrop+nslip)


plot(
  avF,
  ratio,
  type = "p",
  col = coll,
  xlim = c(ffrom, fto),
  log = "y",
  ylim = c(0.01, 100),
  #plot(avF, ratio, type="p", col = coll, xlim=c(ffrom,fto),ylim=c(0,1),
  
  xlab = "Force, pN",
  ylab = "F/B",
  pch = 19,
  cex = 1.2
)
axis(1, seq(0, 10, 1))
#axis(2,c(0.01,0.1,1,10,100))

all <- nfor + nback + nslip + ndrop

t <- matrix(nfor / all ,
            byrow = F,
            nrow = 1,
            ncol = fto - ffrom + 1)
t <- rbind(t, nback / all)
t <- rbind(t, nslip / all)
t <- rbind(t, ndrop / all)



#barplot(t,names.arg=avF,xlab="Force, pN",ylab="Probability",
#legend.text=c("Foresteps","Backsteps","Backsteps < -12","Detachments"),
#args.legend = list(x = 10, y=1.18,cex=0.8),main=title)

#lines(avF, nfor/(nback+nfor+ndrop), type="l", lty=2, col = "black", xlim=c(3,8),ylim=c(0,1), main="Pombe")
#lines(avF, nback/(nback+nfor+ndrop), type="l",lty=2, col = "red", xlim=c(3,8),ylim=c(0,1))
#lines(avF, ndrop/(nback+nfor+ndrop), type="l",lty=2, col = "blue", xlim=c(3,8),ylim=c(0,1))


#plot(avF,nfor,type="l")
#lines(avF,nback, col="blue")

#plot(bthr3$force, bthr3$amplitude, ylim=c(-200,50),xlim=c(ffrom,fto),pch=16,cex=0.4,col=coll)
#plot(bthr3$force[a], bthr3$dwell[a], ylim=c(0.001,10),xlim=c(ffrom,fto),log="y",col=2)
#plot(avF,avDf,type="p",col=coll,lwd=6)
#plot(avF,avD, ylim=c(0.01,10),xlim=c(ffrom,fto),log="y",col=coll,type="b",pch=19,lwd=3)


################# tony's type of error bars


error <- sqrt(ratio * (1 + ratio) / (nback))


lower <- ratio - error
upper <- ratio + error
lower[lower < 0] <- 0.0001

arrows(
  ffrom:fto,
  lower,
  ffrom:fto,
  upper,
  length = 0.05,
  angle = 90,
  code = 3,
  col = coll,
  lwd = 3
)
#arrows(ffrom:fto, avD-se, ffrom:fto, avD+se, length=0.05, angle=90, code=3, col=coll)

#######################

directory2 <-
  "C:/Users/algis/Desktop/Algis/Algis_UoW/R/functions"
setwd(directory2)
source("fit_log.R")

source("fit_log_w.R")

#fit_log(avF, ratio, coll)
w <- ratio / error
fit_log_w(avF, ratio, coll, w)

#fit_log(avF, nfor/(nback+ndrop), coll)

#text(6.5, 55, "Pig MT + GMPCPP + epothilone",col=1)
#text(6.5, 50, "round - F/B",col=coll)
#text(6.5, 30, "square - F/(B+D)",col=coll)

lines(1:10, rep(1, 10), col = "grey")

setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/Dkin_PigMT/subtilisin")
