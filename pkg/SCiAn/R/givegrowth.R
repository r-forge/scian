givegrowth <- function(y,x=c(1:length(y)),bw=12)
{
	list.of.coeff <- NULL
	for(i in 1:(length(x)-bw))
	{
		part.x <- x[i:(i+bw)]
		part.y <- y[i:(i+bw)]
		cur.lm <- lm(part.y~part.x)$coeff[2]
		list.of.coeff[i] <- cur.lm
	}
	result <- max(list.of.coeff) 
	pos <- match(max(list.of.coeff),list.of.coeff)
	coeff <- lm(y[pos:(pos+bw)]~x[pos:(pos+bw)])$coeff
	return(as.numeric(coeff[2]))
}