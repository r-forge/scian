\name{addlog} 
\Rdversion{1.1} 
\alias{addlog} 
\title{Add elements on a decimal log x-y plot}
\description{Add elements on a decimal log x-y plot via \kbd{points}}
\usage{ 
addlog(x,y,log='xy',...)
}

\arguments{ 
\item{x}{X-values} 
\item{y}{Y values, can be left blank (same behavior as \kbd{plot})} 
\item{log}{Dimensions to be converted into 10-log : 'x', 'xy' or 'y'}
\item{...}{Further arguments to be passed to logplot}
}

\examples{ 
a <- seq(from=1,to=100,by=1)^2
logplot(a,log='y')
addlog(a+1e3,log='y')
}