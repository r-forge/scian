readFLUOSTARtomatrix <- function(file)
{
	data <- read.csv(file,sep=';',dec=',',skip=5)
	colnames(data) <- c('S','Col','Row','Val')
	rows <- c('A','B','C','D','E','F','G','H')
	cols <- c(1:12)
	plate <- matrix(0,ncol=12,nrow=8)
	colnames(plate) <- cols
	rownames(plate) <- rows
	OD  <- data$'Val'
	Col <- data$'Col'
	Row <- as.vector(data$'Row')
	for(i in 1:length(OD))
	{
		plate[Row[i],Col[i]] <- OD[i] 
	}
	return(plate)
}

getFLUOSTARcinetic <- function(filenames,positions)
{
	OD <- NULL
	for(i in 1:length(filenames))
	{
		plate <- readFLUOSTARtomatrix(filenames[i])
		ROW <- as.character(positions$'Row')[i]
		COL <- as.character(positions$'Column')[i]
		OD[i] <- plate[ROW,COL]
	}
	return(OD)
}

meanvec <- function(list.of.vec)
{
	MEANVEC <- vector('numeric',length(list.of.vec[[1]]))
	for(i in 1:length(list.of.vec))
	{
		MEANVEC <- MEANVEC + list.of.vec[[i]]
	}
	return(MEANVEC/length(list.of.vec))
}

plotmv <- function(list.of.vec,x=0,...,coeff=1)
{
	if(length(x)==1)
	{
		x <- c(1:length(list.of.vec[[1]]))
	}
	vecmean <- NULL
	vecvari <- NULL
	temp.mat <- matrix(0,ncol=length(list.of.vec[[1]]),nrow=length(list.of.vec))
	for(i in 1:length(list.of.vec))
	{
		temp.mat[i,] <- list.of.vec[[i]]
	}
	for(col in 1:ncol(temp.mat))
	{
		vecmean[col] <- mean(temp.mat[,col])
		vecvari[col] <-  var(temp.mat[,col])
	}
	plot(x,vecmean,...)
	segments(x,vecmean+vecvari*coeff,x,vecmean-vecvari*coeff)
}

dMV <- function(data.frame,add=TRUE,...,col='black',pch=19,lty=1,colbars=NA,noplot=FALSE,pbg=col,sm=TRUE,sm.df=6)
{
	vMean <- NULL
	vSem  <- NULL
	lev <- unique(data.frame$x)
	for(l in 1:length(lev))
	{
		cVec <- subset(data.frame$y,data.frame$x == lev[l])
		vMean <- c(vMean,mean(cVec))
		vSem  <- c(vSem, sd(cVec))
	}
	if(sm)
	{
		vMean <- smooth.spline(lev,vMean,df=sm.df)$y
	}
	if(noplot == FALSE)
	{
		if(add)
		{
			lines(lev,vMean,type='l',col=col,lty=lty)
			arrows(lev,vMean+vSem,lev,vMean-vSem,angle=90,code=3,length=0.05,col=colbars)
			points(lev,vMean,pch=pch,bg=pbg,col=col)
		} else {
			plot(lev,vMean,type='l',col=col,...,lty=lty)
			arrows(lev,vMean+vSem,lev,vMean-vSem,angle=90,code=3,length=0.05,col=colbars)
			points(lev,vMean,pch=pch,bg=pbg,col=col)
		}
	}
	return(list(lev,vMean))
}

getreplicates <- function(filename,positions)
{
	data <- list()
	for(i in 1:length(positions))
	{
		pos <- findpos(rep(positions[i],length(h3)))
		data[[i]] <- getFLUOSTARcinetic(filename,pos)
	}
	return(meanvec(data))
}

grn <- function(data,row,col)
{
	return(as.integer(rownames(subset(data,(data$Well.Col==col)&(data$Well.Row==row)))))
}

gCIN <- function(data,col,row)
{
	dat <- subset(data,(data[,2]==col)&(data[,3]==row))
	return(as.numeric(dat[4:(length(dat)-1)]))
}

getComb <- function(vec1,vec2)
{
	comb <- NULL
	for(i in 1:length(vec1))
	{
		comb <- c(comb,paste(vec1[i],vec2,sep=''))
	}
	return(comb)
}

doforcin <- function(cin,fun,...,whole=FALSE)
{
	temp <- NULL
	for(i in 1:ncol(cin))
	{
		temp <- c(temp,fun(cin[,i],...))
	}
	if(whole){output <- c(mean(temp),sd(temp))}
	else{output<-temp}
	return(output)
}