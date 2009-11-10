deltaDO <- function(cinetic,t.log=TRUE)
{
	len.cin <- length(cinetic)
	delta <- cinetic[len.cin]/cinetic[1]
	if(t.log){delta <- log(delta)}
	return(delta)
}

giveDeltaDO <- function(vec,...,t.log=TRUE)
{
	return(deltaDO(vec,t.log))
}

selectdata <- function(data,row,col)
{
	set <- subset(data,(data$Well.Col==col)&(data$Well.Row==row))
	return(as.numeric(set[4]))
}

findpos <- function(vecofpos,separator=':')
{
	listofcol <- NULL
	listofrow <- NULL
	for(i in 1:length(vecofpos))
	{
		coord <- strsplit(vecofpos[i],separator)[[1]]
		listofcol[i] <- coord[2]
		listofrow[i] <- coord[1]
	}
	result <- as.data.frame(cbind(listofrow,listofcol))
	colnames(result) <- c('Row','Column')
	return(result)
}

getcinetic <- function(filenames,positions)
{
	OD <- NULL
	for(i in 1:length(filenames))
	{
		current.plate <- read.table(filenames[i])
		colnames(current.plate) <- c(1:ncol(current.plate))
		rownames(current.plate) <- LETTERS[1:nrow(current.plate)]
		ROW <- as.character(positions$'Row')[i]
		COL <- as.numeric(positions$'Column')[i]
		OD[i] <- current.plate[ROW,COL]
	}
	return(OD)
}

scalepoints <- function(points,low=0,up=1)
{
	new.points <- points - min(points)
	new.points <- new.points / max(new.points)
	new.points <- new.points * (up-low)
	new.points <- new.points + low
	return(new.points)
}

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

formatDF <- function(data)
{
	nDF <- NULL
	well.id <- NULL
	col <- unique(data$Well.Col)
	row <- as.character(unique(data$Well.Row))
	for(i in 1:length(col))
	{
		for(j in 1:length(row))
		{
			well.id <- c(well.id,as.character(paste(row[j],col[i],sep='')))
			well.cin <- gCIN(data,col[i],row[j])
			nDF <- as.data.frame(cbind(nDF,well.cin))
		}
	}
	colnames(nDF) <- well.id
	return(nDF)
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

read.cinetic <- function(file)
{
	return(read.csv(file,h=TRUE,sep=';',dec=',',skip=5))
}