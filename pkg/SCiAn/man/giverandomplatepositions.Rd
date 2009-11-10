\name{giverandomplatepositions} 
\Rdversion{1.1} 
\alias{giverandomplatepositions} 
\title{Random position generators}
\description{Returns a R object with the randomized positions of several samples} 
\usage{ 
giverandomplatepositions(ncols = 12 , nrows = 8 ,excludesides = TRUE, samples = 1 , plates = 1 , samplenames = '')
} 
\arguments{ 
\item{ncols}{Number of columns of the plate}
\item{nrows}{Number of rows of the plate}
\item{excludesides}{Use (or not) the sides of the plate}
\item{samples}{Number of samples to test}
\item{plates}{Number of plates to generate (for multi-file cinetics)}
\item{samplenames}{Optional specification of each sample name}
} 

\examples{ 
## generate mock data
} 