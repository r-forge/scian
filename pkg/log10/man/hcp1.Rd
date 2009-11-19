\name{hcp1} 
\Rdversion{1.1} 
\alias{hcp1} 
\title{High contrast palette 1}
\description{Provides a high contrast palette ranging from blue to red}
\usage{ 
hcp1(n)
}

\arguments{ 
\item{n}{The number of colors to generate} 
}

\examples{ 
data(volcano)
logfill(volcano,log='',pal=hcp1)
}