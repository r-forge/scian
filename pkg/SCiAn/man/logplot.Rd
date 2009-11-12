\name{logplot} 
\Rdversion{1.1} 
\alias{logplot} 
\title{10-log plot}
\description{Modification of the plot function to add 10-log axis}
\usage{ 
logplot(x=c(1:length(y)),y,log='xy',forceylim=c(0,0),forcexlim=c(0,0),...)
}

\arguments{
\item{x}{x data (defaut by step of 1)}
\item{y}{y data}
\item{log}{Specificies the axis to be converted in 10-log}
\item{forceylim}{Changes the limit of the y axis. In powers of 10 if the axis is 10-log, in true values otherwise}
\item{forcexlim}{Changes the limit of the x axis. In powers of 10 if the axis is 10-log, in true values otherwise}
\item{...}{Supplementary arguments to be passed to \code{plot}}
}

\value{
	\item{}{A plot}
}