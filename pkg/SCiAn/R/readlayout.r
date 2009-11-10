readlayout <- function(file)
{
	## Commandes générales
	layout <- read.csv(file,sep=';',header=T)
	cols <- layout[,1]
	layout <- layout[,c(2:ncol(layout))]
	rownames(layout) <- cols
	colnames(layout) <- c(1:ncol(layout))
	layout[is.na(layout)] <- ''
	
	## Construction des niveaux
	SAMPLES <- NULL
	for(col in 1:ncol(layout))
	{
		for(row in 1:nrow(layout))
		{
			if(as.vector(layout[row,col]) %in% SAMPLES)
			{ # Nothing!
			} else {
				SAMPLES <- c(SAMPLES,as.vector(layout[row,col]))
			}
		}
	}
	samples <- NULL
		## Quick and dirty
	for(i in 1:length(SAMPLES))
	{
		if(SAMPLES[i]==''){} else {samples <- c(samples,SAMPLES[i])}
	}
	## Fin des niveaux
	
	## Début d'assignation de la matrice
	FinalLayout <- matrix(ncol=2)
	for(col in 1:ncol(layout))
	{
		for(row in 1:nrow(layout))
		{
			if(as.vector(layout[row,col]) %in% samples)
			{
				FinalLayout <- rbind(FinalLayout,c(as.vector(layout[row,col]),paste(rownames(layout)[row],colnames(layout)[col],sep='')))
			}
		}
	}
	FinalLayout <- FinalLayout[2:nrow(FinalLayout),]
	## Fin d'assignation de la matrice
	return(FinalLayout)
}