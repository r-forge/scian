adv.barplot <- function(data,datanames=c(1:length(data)),col='lightgrey',bars=TRUE,barsborder=FALSE,forceylim=c(0,0),...,bartype=1)
{
	nbars	<-	length(data)
	yspan	<-	range(data)
	if(sum(forceylim)>0)
	{
		yspan <- forceylim
	}
	M 		<-	NULL
	SD		<-	NULL
	bcol	<-	ifelse(barsborder,1,NA)
	if(length(col)<length(data)){col <- rep(col,length(data))}
	## 
	plot(1,1,col=NA,xlim=c(0.5,nbars+0.5),ylim=yspan,xaxt='n',bty='n',...)
	for(current.data in 1:nbars)
	{
		M[current.data]		<-	mean(data[[current.data]])
		SD[current.data]	<-	sd(data[[current.data]])
		rect(current.data-0.4,min(yspan),current.data+0.4,M[current.data],col=col[current.data],border=bcol)
		if(bartype==1)
		{
			arrows(current.data,M[current.data],current.data,M[current.data]+SD[current.data],length=0.075,angle=90,col=1,lwd=2)
		}
		if(bartype==2)
		{
			arrows(current.data,M[current.data]-SD[current.data],current.data,M[current.data]+SD[current.data],length=0.075,angle=90,code=3,col=1,lwd=2)
		}
		
	}
	axis(side=1,tick=FALSE,labels=datanames,at=c(1:nbars))
	return(M)
}