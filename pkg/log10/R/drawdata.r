drawdata = function(x,y,pch=23,bg='white',lwd=1,lty=1,col='black',meanonly=FALSE)
{
	if(!exists('GLL')){GLL<-''}
	xlg <- FALSE
	ylg <- FALSE
	if ("x" %in% strsplit(GLL, "")[[1]]) {
		xlg = TRUE
	}
	if ("y" %in% strsplit(GLL, "")[[1]]) {
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
	
	if(!meanonly){arrows(lev,up,lev,low,code=3,angle=90,length=0.02)}
	lines(lev,means,lty=lty,col=col,lwd=lwd)
	points(lev,means,pch=pch,bg=bg,cex=1.2)	
}