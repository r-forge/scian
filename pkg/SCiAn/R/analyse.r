analyse <- function(file,layout,func,...)
{
	if(is.list(file))
	{
		DAT <- joincin(lapply(file,read.cinetic))
	} else {
		DAT <- formatDF(read.cinetic(file))
	}
	RES  <- apply(DAT,2,func,...)
	names(RES) <- colnames(DAT)
	results <- fromlayout(readlayout(layout),RES)
	return(results)
}