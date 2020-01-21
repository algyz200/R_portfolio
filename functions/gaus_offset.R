gaus_offset <- function(x, y, a, b, c,d) {

df <- data.frame(x, y)

fit <- nls(y ~ ( a + (b - a) * exp(-(x-c)^2/(2 * d^2)) ), data=df,
                  start=list(a=a, b=b, c=c, d=d) )

dffit <- data.frame(x=seq(min(x), max(x), max(x)/1000-min(x)/1000)); 
dffit$y <- predict(fit, newdata=dffit)

v <- summary(fit)$parameters[,"Estimate"]
a <- v[1]
b <- v[2]
c <- v[3]
d <- v[4]

list(dffit, c(a,b,c,d)) 

}
