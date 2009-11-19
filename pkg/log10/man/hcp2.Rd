\name{hcp2} 
\Rdversion{1.1} 
\alias{hcp2} 
\title{High contrast palette 2}
\description{Provides a high contrast palette ranging from black to darkred}
\usage{ 
hcp2(n)
}

\arguments{ 
\item{n}{The number of colors to generate} 
}

\examples{ 
data(volcano)
logfill(volcano,log='',pal=hcp2)
}