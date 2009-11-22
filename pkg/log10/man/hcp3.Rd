\name{hcp3} 
\Rdversion{1.1} 
\alias{hcp3} 
\title{High contrast palette 3}
\description{Provides a high contrast palette ranging from turquoise to darkred}
\usage{ 
hcp3(n)
}

\arguments{ 
\item{n}{The number of colors to generate} 
}

\examples{ 
data(volcano)
logfill(volcano,log='',pal=hcp3)
}