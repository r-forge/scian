\name{adv.barplot} 
\Rdversion{1.1} 
\alias{adv.barplot} 
\title{Advanced barplot}
\description{Returns a plot with means and s.e.m} 
\usage{ 
adv.barplot(data,datanames=c(1:length(data)),col='lightgrey',bars=TRUE,barsborder=FALSE,forceylim=c(0,0),...)
} 
\arguments{ 
\item{data}{A list of vectors containing all the replicates for a given condition}
\item{datanames}{A vector of conditions names}
\item{col}{Color to fill the inside of each bar. Can be a vector of the same length as data. Not recycled.}
\item{bars}{A logical to decide wether to plot s.e.m bars or just means} 
\item{barsborder}{A logical to decide wether to plot a border around the bars}
\item{forceylim}{A vector of two numeric, giving respectively the lower and upper y limit of the graph, in case the auto calculated range is not visually satisfying} 
\item{...}{Supplementary arguments to be passed to the plot (mostly axes names)}
\item{bartype}{Decides the type of lines for s.e.m to be drawn : 1 for up only, 2 for interval, any other value for no s.e.m}
} 

\examples{ 
## generate mock data
a <- runif(10,3,4)
b <- runif(10,4,5)
c <- runif(10,5,6)
d <- runif(10,5,7)
e <- runif(10,7,8)
dat <- list(a,b,c,d,e)
## Plot
adv.barplot(dat,forceylim=c(0,10))
} 