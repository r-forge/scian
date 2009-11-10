\name{doforcin} 
\Rdversion{1.1} 
\alias{doforcin} 
\title{Multi-cinetic analysis}
\description{Repeat a function for several wells}
\usage{ 
doforcin(cin,fun,...)
}

\arguments{ 
\item{cin}{A data.frame containing all the cinetics, with wells as columns} 
\item{fun}{A function to analyse each cinetic} 
\item{...}{Supplementary arguments to be passed to \code{fun}} 
}
 
\value{
	\item{}{A vector giving the \code{fun} results for each cinetic}
}

\examples{ 
## Read a cinetic fie
cin <- formatDF(read.cinetic(file.choose()))
## Get max. growth rate for all wells
results <- doforcin(cin,givegrowth)
} 