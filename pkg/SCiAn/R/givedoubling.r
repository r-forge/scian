givedoubling = function (y, x = c(1:length(y)), bw = 10, mtime = 6) 
{
    list.of.coeff <- NULL
    for (i in 1:(length(x) - bw)) {
        part.x <- x[i:(i + bw)]
        part.y <- y[i:(i + bw)]
        cur.lm <- lm(part.y ~ part.x)$coeff[2]
        list.of.coeff[i] <- cur.lm
    }
    result <- max(list.of.coeff)
    pos <- which.max(list.of.coeff)
	mod <- lm(y[pos:(pos + bw)] ~ x[pos:(pos + bw)])
    fit <- mod$fitted.value
	coe <- mod$coeff
	est.t <- getDT(fit,(bw)*mtime)
	return(est.t)
}

getDT = function(cin,time)
{
	Nt <- last(cin)
	N0 <- cin[1]
	mu <- (log(Nt,10)-log(N0,10))/time
	t <- log(2,10)/mu
	return(t)
}