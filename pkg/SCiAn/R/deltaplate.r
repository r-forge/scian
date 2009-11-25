deltaplate <- function(layout,file1,file2,malthusian=FALSE)
{
	L <- readlayout(layout)
	D1 <- formatDF(read.cinetic(file1))
	D2 <- formatDF(read.cinetic(file2))
	R1 <- fromlayout(L,D1)
	R2 <- fromlayout(L,D2)
	AA <- NULL
	for(i in 1:length(RA))
	{
		tempres <- as.numeric(R2[[i]]/R1[[i]])
		if(malthusian)
		{
			tempres <- log(tempres,10)
		}
		AA[[i]] <- tempres
	}
	names(AA) <- names(R1)
	return(AA)
}