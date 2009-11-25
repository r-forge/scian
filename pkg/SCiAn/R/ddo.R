ddo <- function(y,malthusian=TRUE)
{
	init <- y[1]
	final <- y[length(y)]
	delta <- ifelse(malthusian,log(final/init,10),final/init)
	return(delta)
}