formatDF <- function(data)
{
	nDF <- NULL
	well.id <- NULL
	col <- unique(data$Well.Col)
	row <- as.character(unique(data$Well.Row))
	for(i in 1:length(col))
	{
		for(j in 1:length(row))
		{
			well.id <- c(well.id,as.character(paste(row[j],col[i],sep='')))
			well.cin <- gCIN(data,col[i],row[j])
			nDF <- as.data.frame(cbind(nDF,well.cin))
		}
	}
	colnames(nDF) <- well.id
	return(nDF)
}