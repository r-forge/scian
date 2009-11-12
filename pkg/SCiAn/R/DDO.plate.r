DDO.plate <- function(p0,pfinal,log=TRUE)
{
	delta <- pfinal/p0
	if(log){return(log(delta))}else{return(delta)}
}