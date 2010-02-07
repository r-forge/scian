drawdata <- function(x,y,pch=23,bg='white',lty=1,col=bg,log='')
{
	if ("x" %in% strsplit(log, "")[[1]]) {
		xlg = TRUE
	}
	if ("y" %in% strsplit(log, "")[[1]]) {
		ylg = TRUE
	}
	
	lev <- sort(unique(x))
	means <- vector(mode='numeric',length=length(lev))
	sems	 <- vector(mode='numeric',length=length(lev))
	for(l in 1:length(lev))
	{
		T <- x == lev[l]
		Y <- y[T]
		means[l] <- mean(Y)
		sems[l]	<- sd(Y)
	}
	
	up <- means + sems
	low <- means - sems
	
	if(xlg) {
		lev <- log(lev,10)
	}
	if(ylg) {
		means <- log(means,10)
		up <- log(up,10)
		low <- log(low,10)
	}
	
	arrows(lev,up,lev,low,code=3,angle=90,length=0.02)
	lines(lev,means)
	points(lev,means,pch=pch,bg=bg,cex=1.2)	
}