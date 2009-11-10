giverandomplatepositions <- function(ncols = 12 , nrows = 8 ,excludesides = TRUE, samples = 1 , plates = 1 , samplenames = '')
{
	rows <- LETTERS[2:nrows-1]
	cols <- c(2:(ncols-1))
	plateslayout <- matrix(0,ncol=plates,nrow=samples)
	if(samplenames == '')
	{
		rownames(plateslayout) <- paste(rep('Sample',samples),c(1:samples))
	} else {
		rownames(plateslayout) <- samplenames
	}
	colnames(plateslayout) <- paste(rep('Plate',plates),c(1:plates))
	if(excludesides == FALSE)
	{
		rows <- c(LETTERS[1],rows,LETTERS[nrows])
		cols <- c(1,cols,ncols)
	}
	possiblepositions <- NULL
	for(i in 1:length(rows))
	{
		for(j in 1:length(cols))
		{
			possiblepositions <- append(possiblepositions,paste(rows[i],cols[j],sep=':'))
		}
	}
	for(i in 1:plates)
	{
		plateslayout[,i] <- sample(possiblepositions,samples)
	}
	return(plateslayout)
}