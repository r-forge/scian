\name{hcp2} 
\Rdversion{1.1} 
\alias{hcp2} 
\title{High contrast palette 2}
\description{}
\usage{ 
hcp2(n)
}

\arguments{ 
\item{n}{The number of colors to generate} 
}

\examples{ 
data(volcano)
par(mfcol=c(1,2),pty='s')
logfill(volcano)
logfill(volcano,log='',pal=hcp2)
}