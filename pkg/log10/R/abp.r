abp = function(data,loc=list(c('darkblue','skyblue')),names=NULL,ylim=NULL,gcol='grey',bg='white',groups=c(1:length(data)),al=0.05,ylab='',xlab='',...,log='y',add.pt=TRUE,pt.c='black',pt.p='.')
{
	#
	if (is.null(ylim)) {
	        ylim = range(data)
	    }
	if(log=='y')
	{
		ylim = log(ylim,10)
	}
	## Set BARSPACER
	BSP <- 0.4
	## Group position calculation
	xf <- 0
	xf[2] <- BSP*1.5
	for(i in 2:length(groups))
	{
		if(groups[i]==groups[(i-1)])
		{
			xf[(i+1)] <- xf[i]+BSP
		} else {
			xf[(i+1)] <- xf[i]+BSP*2
		}
	}
	xf[length(xf)+1] <- xf[length(xf)]+BSP*1.5
	## End group
	xlim=range(xf)
	plot(0,pch=NA,xlim=xlim,ylim=ylim,yaxt='s',bty='o',yaxt='n',xaxt='n',xaxs='i',bty='n',xlab=xlab,ylab=ylab,...)
	rect(par('usr')[1],par('usr')[3],par('usr')[2],par('usr')[4],density=NA,col=bg)
	abline(h=pretty(ylim),col=gcol,lty=3)
	for(i in 1:length(data))
	{
		data[[i]] <- data[[i]][!is.na(data[[i]])]
		if(log=='y'){data[[i]] <- log(data[[i]],10)}
		cd <- boxplot.stats(data[[i]])
		intcol <- ifelse(length(loc)==length(data),loc[[i]][2],loc[[1]][2])
		outcol <- ifelse(length(loc)==length(data),loc[[i]][1],loc[[1]][1])
		arrows(xf[(i+1)],cd$stats[1],xf[(i+1)],cd$stats[5],lwd=1.2,col=outcol,code=3,angle=90,length=al)
		rect(xf[(i+1)]-BSP/3.5,cd$stats[2],xf[(i+1)]+BSP/3.5,cd$stats[4],density=NA,col=intcol,border=outcol)
		points(xf[(i+1)],cd$stats[3],pch=20,col=outcol)
		if(length(cd$out)>0)
		{
			points(rep(xf[(i+1)],length(cd$out)),cd$out,pch=20,col=outcol)
		}
		if(add.pt)
		{
			xs <- jitter(rep(xf[(i+1)],length(data[[i]])),factor=0.7)
			points(xs,data[[i]],pch=pt.p,col=pt.c)
		}
	}
	if(is.null(names))
	{
		labs <- names(data)
	} else {
		labs <- names
	}
	if(log=='y')
	{
		logaxis(2,range(data))
	} else {
		axis(2,at=pretty(range(data)))
	}
	axis(1,at=xf[2:(length(xf)-1)],labels=labs,col.ticks='transparent',lwd=0,cex.axis=0.8)
	box()
}