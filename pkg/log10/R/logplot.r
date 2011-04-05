loggrid <- function(...){
  grid(...)
}

logaxis <- function(side,range,labels=TRUE)
{
	ctck <- par('tck')
	d <- range
	mlog <- floor(min(d))
	Mlog <- ceiling(max(d))
	SeqLog <- c(mlog:Mlog)
	Nlog <- (Mlog-mlog)+1
	for(i in SeqLog)
	{
		if(i==0)
		{
      if(labels)
      {
        axis(side,at=i,labels=substitute(1^phantom(0)))
      } else {
        axis(side,at=i,labels=FALSE)
      }
		}
		else{
      if(labels)
      {
        axis(side,at=i,labels=substitute(10^i))
      } else {
        axis(side,at=i,labels=FALSE)
      }
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
	par(tck=ctck/3)
	if(diff(range(SeqLog))<6)
	{
		axis(side,at=ats,labels=NA)
	}
}

logplot <- function(x,y,log='xy',yint='r',xint='r',xlim=NULL,ylim=NULL,tck=-0.015,las=1,grid=TRUE,...)
{
	GLL <<- log
	if(missing(y))
	{
		y <- x
		x <- c(1:length(x))
	}
	if(is.null(xlim)){xlim=range(x)}
	if(is.null(ylim)){ylim=range(y)}
	par(tck=tck,las=las,xaxs=xint,yaxs=yint)
	xlg <- FALSE
	ylg <- FALSE
	if('x'%in%strsplit(GLL,'')[[1]]){x <- log(x,10);xlg=TRUE}
	if('y'%in%strsplit(GLL,'')[[1]]){y <- log(y,10);ylg=TRUE}
	if(xlg){xlim=log(xlim,10)}else{xlim=xlim}
	if(ylg){ylim=log(ylim,10)}else{ylim=ylim}
	plot.default(x,y,axes=FALSE,ylim=ylim,xlim=xlim,...)
	rect(par()$usr[1],par()$usr[3],par()$usr[2],par()$usr[4],density=NA,col='white')
	if(grid)
	{
		loggrid()
	}
	points(x,y,...)
	if(xlg){logaxis(1,xlim)}else{axis(1,at=pretty(xlim),labels=pretty(xlim))}
	if(ylg){logaxis(2,ylim)}else{axis(2,at=pretty(ylim),labels=pretty(ylim))}
	box()
}

addlog <- function(x,y,...)
{
	if(!exists('GLL')){GLL<-''}
	if(missing(y))
	{
		y <- x
		x <- c(1:length(x))
	}
	xlg <- FALSE
	ylg <- FALSE
	if('x'%in%strsplit(GLL,'')[[1]]){x <- log(x,10);xlg=TRUE}
	if('y'%in%strsplit(GLL,'')[[1]]){y <- log(y,10);ylg=TRUE}
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
    logaxis(3,log(xl,10),FALSE)
	}
	if('y'%in%strsplit(log,'')[[1]])
	{
		logaxis(2,log(yl,10))
    logaxis(4,log(yl,10),FALSE)
	}
	if(c.nbins>0){contour(x,y,z,add=TRUE,col=c.col,n.levels=c.nbins,labcex=labcex)}
	box()
}

loglm <- function(mod,range=NULL,...)
{
	if(!exists('GLL')){GLL<-''}
	if(is.null(range)){
		predictors <- eval(attr(terms(mod),'variables'))[[1]]
		ampli <- diff(range(predictors))
		range <- c((min(predictors)-0.2*ampli),(max(predictors)+0.2*ampli))
	}
	cdir <- mod$coeff[2]
	orao <- mod$coeff[1]
	x <- seq(from=range[1],to=range[2],length.out=1e3)
	y <- x * cdir + orao
	addlog(x,y,type='l',log=GLL,...)
}


hcp1 <- colorRampPalette(c(rgb(122,255,109,maxColorValue=255),rgb(189,255,93,maxColorValue=255),rgb(252,255,83,maxColorValue=255),rgb(255,139,29,maxColorValue=255),rgb(255,69,15,maxColorValue=255)))
hcp2 <- colorRampPalette(c("black","darkblue","blue","green","orange",'yellow',"red","darkred"))
hcp3 <- colorRampPalette(c(rgb(0,0,255,max='255'),rgb(15,251,240,max='255'),rgb(247,251,9,max='255'),rgb(246,150,9,max='255'),rgb(247,4,1,max='255'),rgb(128,4,0,max='255')))
hcp4 <- colorRampPalette(c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"))
hcp5 <- colorRampPalette(c("white", "lightgrey", "black"))
hcp6 <- colorRampPalette(c('white','skyblue','darkblue'))
hcp7 <- colorRampPalette(c('darkblue','blue','turquoise','green','yellow','orange','red','darkred'))
hcp8 <- colorRampPalette(c('black',rgb(0.1,0.1,0.1),'darkorange','white'))

scale = function(v,m=0,M=1)
{
	v <- v-min(v)
	v <- v/max(v)
	v <- v*(M-m)
	v <- v+m
	return(v)
}

colgrid = function(mat,palette,cols=c('white','skyblue','darkblue'),gRes=1000,log='y')
{
	if(log=='y')
	{
		ym <- min(floor(log(mat,10)))
		yM <- max(ceiling(log(mat,10)))
		mat <- log(mat,10)
	}
	m.range <- range(mat)
	pal <- colorRampPalette(c(cols))(gRes)
	plot(0,xlim=c(-2,ncol(mat)+3),ylim=c(-2.5,nrow(mat)+1),xlab='',ylab='',xaxt='n',yaxt='n',bty='n',pch=NA)
	mat <- round(scale(mat,1,gRes),0)
	for(ro in 1:nrow(mat))
	{
		for(co in 1:ncol(mat))
		{
			rect(ro-0.5, co-0.5, ro+0.5, co+0.5, density=NA, col=pal[mat[ro,co]])
		}
	}
	coo.x <- c(1:(ncol(mat)+1))
	coo.y <- c(1:(nrow(mat)+1))
	
	text(c(1:ncol(mat)),0,colnames(mat),srt=90,adj=1)
	text(0,c(1:nrow(mat)),rev(rownames(mat)),adj=1)
	
	segments(coo.x-0.5,0.5,coo.x-0.5,nrow(mat)+0.5,lwd=1.2)
	segments(0.5,coo.y-0.5,ncol(mat)+0.5,coo.y-0.5,lwd=1.2)
	
	x1 <- ncol(mat)+1.5
	x2 <- ncol(mat)+2
	y1 <- 0.5
	y2 <- nrow(mat)+0.5
	
	ys <- seq(from=y1,to=y2,length=gRes)
	
	for(i in 1:(gRes-1))
	{
		rect(x1,ys[i],x2,ys[i+1],density=NA,col=pal[i])
	}
	
	rect(x1,y1,x2,y2,lwd=1.2)
	
	if(log=='y')
	{
		SeqLog <- c(ym:yM)
		print(SeqLog)
		Pos <- scale(SeqLog,0,ncol(mat))
		for (i in 1:length(SeqLog)){
			cp <- SeqLog[i]
			segments(x1,Pos[i]+0.5,x2,Pos[i]+0.5,lwd=1.1,lty=3)
			if (cp == 0) {
				text(ncol(mat)+2.5,Pos[i]+0.5,substitute(1^phantom(0)))
			} else {
				text(ncol(mat)+2.5,Pos[i]+0.5,substitute(10^cp))
			}
		}
	} else {
		PRETTY <- pretty(range(c(1:ncol(mat))))
		text(ncol(mat)+2.15,PRETTY+0.5,round(scale(PRETTY,m.range[1],m.range[2]),2),adj=0)
		segments(x1,PRETTY+0.5,x2,PRETTY+0.5,lwd=1.1,lty=3)
	}
}

logpoly <- function(x,y,...)
{
	if(!exists('GLL')){GLL<-''}
	if('x'%in%GLL){x<-log(x,10)}
	if('y'%in%GLL){y<-log(y,10)}
	polygon(x,y,...)
}