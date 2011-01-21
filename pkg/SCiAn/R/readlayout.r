readlayout <- function(file)
{
	lay <- read.csv(file,sep=';',header=TRUE)
	cols <- lay[,1]
	lay <- lay[,c(2:ncol(lay))]
	rownames(lay) <- cols
	colnames(lay) <- c(1:ncol(lay))
	lay[is.na(lay)] <- ''
	SAMPLES <- unique(as.vector(as.matrix(lay)))
	samples <- SAMPLES[(!is.na(SAMPLES))&(SAMPLES!='')]
	FinalLayout <- matrix(ncol=2)
	for(col in 1:ncol(lay))
	{
		for(row in 1:nrow(lay))
		{
			if(as.vector(lay[row,col]) %in% samples)
			{
				FinalLayout <- rbind(FinalLayout,c(as.vector(lay[row,col]),paste(rownames(lay)[row],colnames(lay)[col],sep='')))
			}
		}
	}
	FinalLayout <- FinalLayout[2:nrow(FinalLayout),]
	return(FinalLayout)
}