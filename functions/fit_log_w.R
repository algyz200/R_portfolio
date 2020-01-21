fit_log_w <- function (x,y,coll,w) {

#x <- avF
#y <- nfor/nback


x <- x[!is.na(y)]
w <- w[!is.na(y)]
y <- y[!is.na(y)]


x <- x[y!=0]
w <- w[y!=0]
y <- y[y!=0]


x <- x[is.finite(y)]
w <- w[is.finite(y)]
y <- y[is.finite(y)]


model <- lm(log(y)~x, weights = w)

dffit <- data.frame(x=seq(-20,20,0.01))
dffit$y <- exp(predict(model,dffit))
lines(dffit$x, dffit$y,col=coll,lwd=3)

print(model)

}

