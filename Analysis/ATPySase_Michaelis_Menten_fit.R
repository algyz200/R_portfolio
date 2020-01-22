#Algirdas Toleikis 2020
#this code reads in enzymatic kinetics data and fits Michaelis-Menten model
#v~Vmax*S/(Km+S)

setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/Dkin_PigMT/ATPyS")
myfile <- read.table("ATPySase.txt", sep="\t", header = T)

S <- myfile[,1]
v <- myfile[,2]

kinData <- data.frame(S,v)

#model fitting
MMcurve<-formula(v~Vmax*S/(Km+S))
fit <- nls(MMcurve, kinData, start=list(Vmax=50,Km=2))

#prediction
predictRange <- seq(0,100,0.1)
predLine <- predict(fit,list(S=predictRange))
fitValues <- coef(fit)

plot (kinData, 
      xlab="MT tubulin concentration, uM)", 
      ylab="Rate, 1/s (monomer)", 
      title(main="ATP"), 
      pch=19, col="blue", cex=2, xlim=c(0,50), ylim=c(0,25))

lines(predictRange,predLine,col="red", lwd=2)

text(15,20, paste("Vmax =",round(fitValues[1],2)))
text(15,22, paste("Km =", round(fitValues[2],2)))

#save pdf
#dev.copy(pdf,"plot.pdf", useDingbats=FALSE)
#dev.off()