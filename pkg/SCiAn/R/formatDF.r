formatDF <- function(data)
{
	Names <- paste(data[,3],data[,2],sep='')
	Values <- t(data[,c(4:(ncol(data)-1))])
	colnames(Values) <- Names
	rownames(Values) <- c(1:nrow(Values))
	return(as.data.frame(Values))
}