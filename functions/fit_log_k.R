fit_log_k <- function (x,y,col) {
if (sum(y)>0) {
x <- x[!is.na(y)]
y <- y[!is.na(y)]
x <- x[y!=0]
y <- y[y!=0]
x <- x[is.finite(y)]
y <- y[is.finite(y)]

model <- lm(log(y)~x)

dffit <- data.frame(x=seq(-20,20,0.01))
dffit$y <- exp(predict(model,dffit))
lines(dffit$x, dffit$y,col=col)

k <- summary(model)$coefficients[,"Estimate"][2]

k
} else {
k <- 0
k
}
}