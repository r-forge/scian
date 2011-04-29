formatPlate = function(path)
{
	DF <- NULL
	f <- readLines(path)
	NofSteps <- sum(f=='')
	pname <- last(unlist(strsplit(unlist(strsplit(strsplit(f[1],c(':'),fixed=TRUE)[[1]],'[',fixed=TRUE)),']',fixed=TRUE)))
	cat('\tReading plate\t',pname,'\n')
	mats <- f[c(2:(length(f)-1))]
	plateinf <- NULL
	plateinf$row <- ifelse(NofSteps > 1,(length(mats)-1)/NofSteps,length(mats))
	plateinf$col <- length(strsplit(mats[1],' ')[[1]])
	plateinf$cnames <- c(1:plateinf$col)
	plateinf$rnames <- LETTERS[c(1:plateinf$row)]
	mats <- mats[mats!='']
	for(step in 1:NofSteps)
	{
		Lines <- c(1:plateinf$row)+((step-1)*plateinf$row)
		LineDat <- c(mats[Lines])
		Values <- matrix(as.numeric(as.vector(unlist(strsplit(LineDat,' ')))),nrow=plateinf$row,byrow=TRUE)
		colnames(Values) <- plateinf$cnames
		rownames(Values) <- plateinf$rnames
		df <- NULL
        for(nr in 1:nrow(Values)) for(nc in 1:ncol(Values)) {
            wellname <- paste(rownames(Values)[nr],colnames(Values)[nc],sep='')
            df[[wellname]] <- Values[nr,nc]
        }
        DF <- rbind(DF,unlist(df))
	}
	return(as.data.frame(DF))
}