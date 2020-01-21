#plot(bthr3$force, bthr3$amplitude, ylim=c(-200,50),xlim=c(2,9),pch=16,cex=0.4,col=coll,
#xlab="Force, pN", ylab="Amplitude, nm")

#ff <- bthr3$mean1 < 20 & bthr3$amplitude < 0 & bthr3$force/bthr3$amplitude < -0.0725
#lines(bthr3$force[ff], bthr3$amplitude[ff], ylim=c(-200,50),xlim=c(ffrom,fto),pch=16,cex=0.4,col="white",type="p")

#hist(bthr3$force/bthr3$amplitude)

ff <- bthr3$amplitude>0 
plot(bthr3$force[ff], bthr3$amplitude[ff], ylim=c(-130,25),xlim=c(2,9),pch=16,cex=0.4,col=1,type="p",
xlab="Force, pN", ylab="Amplitude, nm")

ff <- bthr3$amplitude<0 & bthr3$amplitude> minback#back
lines(bthr3$force[ff], bthr3$amplitude[ff], ylim=c(-200,50),xlim=c(ffrom,fto),pch=16,cex=0.4,col="orange",type="p")

ff <- bthr3$mean1 > 20 & bthr3$amplitude < 0 & bthr3$amplitude < -12 
lines(bthr3$force[ff], bthr3$amplitude[ff], ylim=c(-200,50),xlim=c(ffrom,fto),pch=16,cex=0.4,col=5,type="p")

ff <- bthr3$mean1 < 20 & bthr3$amplitude < 0 & bthr3$force/bthr3$amplitude > -0.0725
lines(bthr3$force[ff], bthr3$amplitude[ff], ylim=c(-200,50),xlim=c(ffrom,fto),pch=16,cex=0.4,col="grey",type="p")



