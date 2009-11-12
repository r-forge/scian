read.cinetic <- function(file)
{
	return(read.csv(file,h=TRUE,sep=';',dec=',',skip=5))
}