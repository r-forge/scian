\name{drawdata} 
\Rdversion{1.1} 
\alias{drawdata} 
\title{Add mean and s.e.m to a log10 or natural plot}
\description{Add mean and s.e.m to a log10 or natural plot, where means are plotted as a dot and the range (sem) is plotted as an arrow. Whenever x values are found several time, the mean and sem of corresponding y values are calculated.}
\usage{ 
drawdata(x,y,pch=23,bg='white',lty=1,col=bg,log='')
}

\arguments{ 
\item{x}{The x points, as a vector} 
\item{y}{The y points, as a vector}
\item{pch}{The point used to represent mean}
\item{bg}{The mean point color}
\item{lty}{Type of the line to link mean points}
\item{col}{Color of the line}
\item{log}{Either 'x', 'y', 'xy', or ''}
}