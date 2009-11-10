fromlayout <- function(layout,data)
{
	results <- list()
	samplenames <- unique(layout[,1])
	for(i in 1:length(samplenames))
	{
		pos.todo <- subset(layout[,2],layout[,1]==samplenames[i])
		partial.results <- as.vector(data[pos.todo])
		results[[length(results)+1]] <- partial.results
	}
	names(results) <- samplenames
	return(results)	
}