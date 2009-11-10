\name{givegrowth} 
\Rdversion{1.1} 
\alias{givegrowth} 
\title{Maximal growth rate}
\description{Give the maximal growth rate during a cinetic}
\usage{ 
givegrowth(y,x=c(1:length(y)),bw=12)
}

\arguments{ 
\item{y}{Optical density at each step of the cinetic} 
\item{x}{Time at which each y value is measured. Assumed to be evenly spaced} 
\item{bw}{Bandwidth of the analysis. Smaller values are subjected to artifacts, higher values requires more points} 
}
 
\value{
	\item{}{A single numeric value giving the maximal observed growth rate}
}
 
\note{R Craig Mac Lean provided the algorithm used in this function}

\examples{ 
## Read a cinetic fie
cin <- formatDF(read.cinetic(file.choose()))
## Get max. growth rate
givegrowth(cin$A1,bw=8)
} 