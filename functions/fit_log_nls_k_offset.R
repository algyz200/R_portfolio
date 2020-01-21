fit_log_nls_k_offset <- function (x,y,col) {
if (sum(y)>0) {

#x <- h$mids
#y <- h$counts

x <- x[!is.na(y)]
y <- y[!is.na(y)]
x <- x[y!=0]
y <- y[y!=0]
x <- x[is.finite(y)]
y <- y[is.finite(y)]

df <- data.frame(x,y)

model <- nls(y ~ ( a1 * exp(-k*x)+b ), data=df, start=list(a1=max(y), k=1,b=0) )  

dffit <- data.frame(x=seq(0,20,0.01))
dffit$y <- predict(model,dffit)
lines(dffit$x, dffit$y,col=col,lwd=2)

k <- summary(model)$coefficients[,"Estimate"][2]

k
} else {
k <- 0
k
}
}