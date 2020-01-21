#run plot8_detect_runs
#then choose minx and maxx on this code and run it

minx <- 135
maxx <- 145


plot(time3, yfilt3, col = "black", xlab="Time, s", ylab="Displacement, nm",
type="l", xlim=c(minx,maxx), main=dir, ylim=c(miny,maxy), yaxt="n")
#lines(time3,xfilt3,col = "blue")
at <- seq(from = miny, to = maxy, by = int)
axis(side = 2, at = at)
abline(h=seq(from=miny, to=maxy,by=int), NULL, col="gray", lty=3)


fitwin0 <- 0.3 #s
minF <- 1
fitwin <- fitwin0*freq
shift <- 0.1

count <- 1
count2 <- 2

vel <- data.frame()
a1 <- as.numeric()
a2 <- as.numeric()
a3 <- as.numeric()


#for (i in 1:round((runs$t2[1]-runs$t1[1])*freq/fitwin)) {

#for (i in 1:nrow(runs)) {
for (i in 1) {


count <- (runs$t1[i]-0.2)*freq-min(time3)*freq
	
	for (ii in 1:round((runs$t2[i]-runs$t1[i]-fitwin0/1)/shift)) {
	
	
 
	timefit <- time3[count:(count+fitwin)]
	yfit <- yfilt3[count:(count+fitwin)]

	linefit <- lm(yfit ~ timefit)
	linefit <- coef(linefit)

	avF <- yfit*k

	#if (avF > minF & linefit[2] > 0) {
	lines(timefit,timefit*linefit[2]+linefit[1], col="red", add=T,lwd=3)
	#}

	a1[count2] <- mean(yfit)*k
	a2[count2] <- linefit[2]
	a3[count2] <- linefit[1]

	count <- count+shift*freq
	count2 <- count2+1
	}
}

## Force
if (TRUE){
par(new=T)
plot(time, force, col = rgb(0,0,0,0),axes=F, xlab="",ylab="", xlim=c(minx,maxx),
ylim=c(miny*k,maxy*k),type="l" )
axis(4)
mtext("Force, pN", side=4, line=3)
}


vel <- cbind(a1,a2,a3)

a1f <- a1 > minF & a2 > 0  # min y
vel2 <- cbind(a1[a1f], a2[a1f], a3[a1f])

#plot(vel2[,1], vel2[,2])


