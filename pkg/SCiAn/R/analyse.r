analyse <- function(file,layout,func,...)
{
	if(is.list(file))
	{
		cins <- lapply(file,read.cinetic)
		DAT <- joincin(cins)
	} else {
		DAT <- formatDF(read.cinetic(file))
	}
	LAY <- readlayout(layout)
	RES <- vector(len=ncol(DAT))
	for(i in 1:length(RES))
	{
		RES[i] <- func(DAT[,i],...)
	}
	names(RES) <- colnames(DAT)
	results <- fromlayout(LAY,RES)
	return(results)
}