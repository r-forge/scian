foranova <- function(l)
{
	if(is.null(names(l)))
	{
		names(l) <- c(1:length(l))
	}
	df <- data.frame()
	factors <- names(l)
	for(le in 1:length(l))
	{
		cl <- l[[le]]
		cls <- length(cl)
		cf <- rep(factors[le],cls)
		t.df <- as.data.frame(cbind(cl,cf))
		df <- as.data.frame(rbind(df,t.df))
	}
	df[,2] <- as.factor(df[,2])
	df[,1] <- as.numeric(as.vector(df[,1]))
	colnames(df) <- c('val','fac')
	return(df)
}