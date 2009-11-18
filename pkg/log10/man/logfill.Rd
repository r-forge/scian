\name{logfill} 
\Rdversion{1.1} 
\alias{logfill} 
\title{Decimal x-y filledcontour plot}
\description{}
\usage{ 
logfill(z,pal=cm.colors,f.nbins=100,c.nbins=10,log='xy',c.col='black',int=c('i','i'),labcex=0.8,...)
}

\arguments{ 
\item{z}{The matrix to be plotted. X and Y coordinates must be passed as rownames and colnames respectively} 
\item{pal}{The color palette : the package provides \kbd{hcp1} and \kbd{hcp2}} 
\item{f.nbins}{Number of color shades for the plot}
\item{c.nbins}{Number of lines for the overlapped contourplot : 0 suppresses the contourplot}
\item{log}{Axes to be log-transformed} 
\item{c.col}{Color for the contour plot}
\item{int}{A vector giving the type of the axes. Best not changed.}
\item{labcex}{Character expansion of the contourplot}
\item{...}{Further arguments to be passed to logplot}
}

\examples{ 
data(volcano)
par(mfcol=c(1,2),pty='s')
logfill(volcano)
logfill(volcano,log='')
}