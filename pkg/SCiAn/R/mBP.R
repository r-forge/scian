mBP <- function(list.of.lists,...,condnames=0,pal=colorRampPalette(c('darkgrey','white')),seriesnames=0,legendpos='topleft',legh=TRUE,do.pty='s',legt='')
{
	par(pty=do.pty,mgp=c(1.9,0.8,0),oma=c(0,0,0,0),mar=c(4,3,2,1),bg='transparent',bty='n',tck=0.02,yaxs='i')
	NofList <- length(list.of.lists)
	NofSubList <- length(list.of.lists[[1]])
	if(condnames==0){condnames=c(1:NofList)}
	if(seriesnames==0){seriesnames=c(1:NofSubList)}
	dim.mat.treat <- (NofList+1)*NofSubList
	pos <- c(1:dim.mat.treat)
	pos <- matrix(pos,nrow=(NofList+1))
	nbreaks <- dim.mat.treat+1
	plot(0,0,pch=NA,xlim=c(0,nbreaks),ylim=c(min(pretty(range(list.of.lists))),max(pretty(range(list.of.lists)))),xaxt='n',yaxt='n',...)
	calc.ylab <- pretty(range(list.of.lists))
	lab.ylab <- round(calc.ylab,2)
	abline(h=pretty(range(list.of.lists)),lty=3,col='grey')
	abline(h=0,col=1,lwd=1.4)
	palette <- pal(NofList)
	for(condition in 1:NofList)
	{
		for(treatment in 1:NofSubList)
		{
			CD <- list.of.lists[[condition]][[treatment]]
			CDm <- mean(CD)
			CDv <- sd(CD)
			x.coord <- pos[condition,treatment]
			rect(x.coord,0,x.coord+1,CDm,col=palette[condition])
			arrows(x.coord+0.5,CDm+CDv,x.coord+0.5,CDm-CDv,angle=90,code=3,length=0.05)
		}
	}
	axis(side=2,at=calc.ylab,labels=lab.ylab,tick=TRUE)
	fills <- c(1:NofList)
	legend(legendpos,legend=condnames,fill=palette[fills],bty='n',horiz=legh,cex=0.9,title=legt)
	cl.tab <- (c(1:NofSubList)-1)*NofSubList + NofList/2 +1
	lab.cond <- NULL
	int.lab.cond <- pos[c(1:(nrow(pos)-1)),]
	for(COL in 1:ncol(int.lab.cond))
	{
		lab.cond[COL] <- mean(int.lab.cond[,COL])+0.5
	}
	axis(side=1,at=lab.cond,labels=seriesnames,tick=FALSE)
	axis(side=4,at=pretty(range(list.of.lists)),labels=pretty(range(list.of.lists)),tick=TRUE)
	par(xaxs='r')
	if(min(pretty(range(list.of.lists)))<0){abline(h=0,lwd=1.1)}
	abline(h=min(pretty(range(list.of.lists))),lwd=1.1)
}