gaus <- function(x, y, c, mean, sdev) {

df <- data.frame(x, y)

fit <- nls(y ~ ( C1 * exp(-(x-mean1)**2/(2 * sigma1**2)) ), data=df,
                  start=list(C1=c, mean1=mean, sigma1=sdev) )

dffit <- data.frame(x=seq(min(x), max(x), max(x)/1000-min(x)/1000)); 
dffit$y <- predict(fit, newdata=dffit)

v <- summary(fit)$parameters[,"Estimate"]
gaus_mean <- v[2]
dffit[,3] <- gaus_mean
colnames(dffit)[3] <- "mean"
dffit
}

