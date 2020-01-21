fit_log_segment <- function (x,y,col) {

x <- x[!is.na(y)]
y <- y[!is.na(y)]
x <- x[y!=0]
y <- y[y!=0]
x <- x[is.finite(y)]
y <- y[is.finite(y)]

model <- lm(log(y)~x)

dffit <- data.frame(x=seq(min(x)-0.5,max(x)+0.5,0.01))
dffit$y <- exp(predict(model,dffit))
#lines(dffit$x, dffit$y,col=col,lwd=3,xlim=c(3,7))
lines(c(dffit$x[1],dffit$x[length(dffit$x)]), c(dffit$y[1],dffit$y[length(dffit$y)]),col=col,lwd=3,xlim=c(3,7))


summary(model)


}