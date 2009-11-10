analyse <- function(file,layout,func,...)
{

	LAY <- readlayout(layout)
	DAT <- formatDF(read.cinetic(file))
	RES <- vector(len=ncol(DAT))
	for(i in 1:length(RES))
	{
		RES[i] <- func(DAT[,i],...)
	}
	names(RES) <- colnames(DAT)
	results <- fromlayout(LAY,RES)
	return(results)
}