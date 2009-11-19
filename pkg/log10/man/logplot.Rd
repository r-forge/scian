\name{logplot} 
\Rdversion{1.1} 
\alias{logplot} 
\title{Decimal x-y plot}
\description{}
\usage{ 
logplot(x,y,log='xy',yint='r',xint='r',xlim=NULL,ylim=NULL,...)
}

\arguments{ 
\item{x}{X-values} 
\item{y}{Y values, can be left blank (same behavior as \kbd{plot})} 
\item{log}{Dimensions to be converted into 10-log : 'x', 'xy' or 'y'}
\item{ylim}{Limit of the y axis}
\item{xlim}{Limit of the x axis} 
\item{yint}{The type of y axis : internal of regular. See \kbd{par} and the \kbd{yaxs} option}
\item{xint}{The type of x axis : internal of regular. See \kbd{par} and the \kbd{xaxs} option}
\item{...}{Further arguments to be passed to plot}
}
 
\note{When \kbd{(x|y)lim} is used, if the axis is in log, the user must supply a power of ten. Otherwise, the absolute values must be supplied.}

\examples{ 
a <- seq(from=1,to=100,by=0.1)^2
logplot(a,log='y')
}