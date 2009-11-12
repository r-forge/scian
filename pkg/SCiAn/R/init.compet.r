init.compet <- function(reference,competitors,final.volume=500,comp.names=letters[1:length(competitors)],round=1)
{
	ref.toadd <- (reference/(reference+competitors))*final.volume
	com.toadd <- final.volume-ref.toadd
	ratio <- ref.toadd/com.toadd
	results <- matrix(c(ref.toadd,com.toadd,ratio),byrow=FALSE,ncol=3)
	rownames(results) <- comp.names
	colnames(results) <- c('Ref.','Comp.','R/C')
	return(results)
}