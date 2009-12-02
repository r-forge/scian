cleandata = function(x,y,bw=10,tresh=1)
{
	for (i in 1:(length(x) - bw))
	{
		part.x <- x[i:(i + bw)]
		part.y <- y[i:(i + bw)]
		LR <- lm(part.y ~ part.x)
		PR <- as.data.frame(predict(LR,interval='prediction',level=0.99))
		part.y[part.y>PR$upr] <- PR$fit[part.y>PR$upr]
		part.y[part.y<PR$lwr] <- PR$fit[part.y<PR$lwr]
		y[i:(i + bw)] <- part.y
	}
	return(y)
}