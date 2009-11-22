logaxis <- function(side,range)
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

logplot <- function(x,y,log='xy',yint='r',xint='r',xlim=NULL,ylim=NULL,...)
{
	if(missing(y))
	{
		y <- x
		x <- c(1:length(x))
	}
	if(is.null(xlim)){xlim=range(x)}
	if(is.null(ylim)){ylim=range(y)}
	par(tck=0.02,xaxs=xint,yaxs=yint)
	xlg <- FALSE
	ylg <- FALSE
	if('x'%in%strsplit(log,'')[[1]]){x <- log(x,10);xlg=TRUE}
	if('y'%in%strsplit(log,'')[[1]]){y <- log(y,10);ylg=TRUE}
	if(xlg){xlim=log(xlim,10)}else{xlim=xlim}
	if(ylg){ylim=log(ylim,10)}else{ylim=ylim}
	plot.default(x,y,axes=FALSE,ylim=ylim,xlim=xlim,...)
	if(xlg){logaxis(1,xlim)}else{axis(1,at=pretty(xlim),labels=pretty(xlim))}
	if(ylg){logaxis(2,ylim)}else{axis(2,at=pretty(ylim),labels=pretty(ylim))}
	box()
}

addlog <- function(x,y,log='xy',...)
{
	if(missing(y))
	{
		y <- x
		x <- c(1:length(x))
	}
	xlg <- FALSE
	ylg <- FALSE
	if('x'%in%strsplit(log,'')[[1]]){x <- log(x,10);xlg=TRUE}
	if('y'%in%strsplit(log,'')[[1]]){y <- log(y,10);ylg=TRUE}
	points(x,y,...)
	
}

logfill <- function(z,pal=hcp3,f.nbins=100,c.nbins=10,log='xy',c.col='black',int=c('i','i'),labcex=0.8,...)
{
	if(is.null(colnames(z))){colnames(z)<-c(1:ncol(z))}
	if(is.null(rownames(z))){rownames(z)<-c(1:nrow(z))}
	x <- as.numeric(rownames(z))
	y <- as.numeric(colnames(z))
	xl <- range(x)
	yl <- range(y)
	if('x'%in%strsplit(log,'')[[1]]){x <- log(x,10)}
	if('y'%in%strsplit(log,'')[[1]]){y <- log(y,10)}
	logplot(range(x),range(y),log=log,pch=NA,xint=int[1],yint=int[2],xlim=xl,ylim=yl,...)
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
		logaxis(1,log(xl,10))
	}
	if('y'%in%strsplit(log,'')[[1]])
	{
		logaxis(2,log(yl,10))
	}
	if(c.nbins>0){contour(x,y,z,add=TRUE,col=c.col,n.levels=c.nbins,labcex=labcex)}
}

loglm <- function(mod,log='xy',range=NULL,...)
{
	if(is.null(range)){
		predictors <- eval(attr(terms(mod),'variables'))[[1]]
		ampli <- diff(range(predictors))
		range <- c((min(predictors)-0.2*ampli),(max(predictors)+0.2*ampli))
	}
	cdir <- mod$coeff[2]
	orao <- mod$coeff[1]
	x <- seq(from=range[1],to=range[2],length.out=1e3)
	y <- x * cdir + orao
	addlog(x,y,type='l',log=log,...)
}

hcp1 <- colorRampPalette(c('darkblue','blue','turquoise','green','yellow','orange','red','darkred'))
hcp2 <- colorRampPalette(c("black","darkblue","blue","green","orange",'yellow',"red","darkred"))
hcp3 <- colorRampPalette(c(rgb(0,0,255,max='255'),rgb(15,251,240,max='255'),rgb(247,251,9,max='255'),rgb(246,150,9,max='255'),rgb(247,4,1,max='255'),rgb(128,4,0,max='255')))