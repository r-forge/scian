\name{init.compet}
\Rdversion{1.1} 
\alias{init.compet}
\title{Initial volume for a 50:50 competition}
\description{Returns the initial volume to inoculate for a 50:50 competition, based on OD measures}
\usage{ 
init.compet(reference,competitors,final.volume=500,comp.names=letters[1:length(competitors)],round=1)
}

\arguments{
\item{reference}{OD of the reference strain}
\item{competitors}{A vector of OD for the competitors strains}
\item{final.volume}{The final volume of the total inoculum}
\item{comp.names}{The names of the competitors}
\item{round}{The round to apply to the volumes}
}
 
\value{
	\item{}{}
}