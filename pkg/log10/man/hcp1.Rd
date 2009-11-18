\name{hcp1} 
\Rdversion{1.1} 
\alias{hcp1} 
\title{High contrast palette 1}
\description{}
\usage{ 
hcp1(n)
}

\arguments{ 
\item{n}{The number of colors to generate} 
}

\examples{ 
data(volcano)
par(mfcol=c(1,2),pty='s')
logfill(volcano)
logfill(volcano,log='',pal=hcp1)
}