drawlogaxis <- function(side,range)
{
	par(tck=0.02,las=1)
	d <- range
	mlog <- floor(min(d))
	Mlog <- ceiling(max(d))
	SeqLog <- c(mlog:Mlog)
	Nlog <- (Mlog-mlog)+1
	for(i in SeqLog)
	{
		if(i==0)
		{
			axis(side,at=i,labels=substitute(1^phantom(0)))
		}
		else{
			axis(side,at=i,labels=substitute(10^i))
		}
	}
	ats <- log(seq(from=2,to=9,by=1),10)
	mod <- NULL
	for(i in SeqLog)
	{
		mod <- c(mod,rep(i,length(ats)))
	}
	ats <- rep(ats,Nlog)
	ats <- ats+mod
	par(tck=0.02/3)
	axis(side,at=ats,labels=NA)
}

logplot <- function(x=c(1:length(y)),y,log='xy',forceylim=c(0,0),forcexlim=c(0,0),yint='r',xint='r',...)
{
	par(tck=0.02,xaxs=xint,yaxs=yint)
	xlg <- FALSE
	ylg <- FALSE
	if('x'%in%strsplit(log,'')[[1]]){x <- log(x,10);xlg=TRUE}
	if('y'%in%strsplit(log,'')[[1]]){y <- log(y,10);ylg=TRUE}
	yl <- ifelse(forceylim==c(0,0),range(y),forceylim)
	xl <- ifelse(forcexlim==c(0,0),range(x),forcexlim)
	plot(x,y,...,axes=FALSE,ylim=yl,xlim=xl)
	if(xlg){drawlogaxis(1,xl)}else{axis(1,at=pretty(xl),labels=pretty(xl))}
	if(ylg){drawlogaxis(2,yl)}else{axis(2,at=pretty(yl),labels=pretty(yl))}
	box()
}

addlog <- function(x=c(1:length(y)),y,log='xy',...)
{
	xlg <- FALSE
	ylg <- FALSE
	if('x'%in%strsplit(log,'')[[1]]){x <- log(x,10);xlg=TRUE}
	if('y'%in%strsplit(log,'')[[1]]){y <- log(y,10);ylg=TRUE}
	points(x,y,...)
	
}

logfill <- function(z,pal=cm.colors,f.nbins=100,c.nbins=10,log='xy',c.col='black',int=c('i','i'),labcex=0.8,...)
{
	x <- as.numeric(rownames(z))
	y <- as.numeric(colnames(z))
	if('x'%in%strsplit(log,'')[[1]])
	{
		fxl <- range(log(x,10))
		x <- log(x,10)
	} else {
		fxl <- range(x)
	}
	if('y'%in%strsplit(log,'')[[1]])
	{
		fyl <- range(log(y,10))
		y <- log(y,10)
	} else {
		fyl <- range(y)
	}
	logplot(1,1,forcexlim=fxl,forceylim=fyl,log=log,pch=NA,xint=int[1],yint=int[2],...)
	levels <- pretty(range(z),f.nbins)
	col <- pal(length(levels)-1)
	.Internal(filledcontour(
		as.double(x),
		as.double(y),
		z,
		as.double(levels),
		col = col)
		)
	if('x'%in%strsplit(log,'')[[1]])
	{
		drawlogaxis(1,fxl)
	}
	if('y'%in%strsplit(log,'')[[1]])
	{
		drawlogaxis(2,fyl)
	}
	if(c.nbins>0){contour(x,y,z,add=TRUE,col=c.col,n.levels=c.nbins,labcex=labcex)}
}