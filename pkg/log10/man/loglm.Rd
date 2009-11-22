\name{loglm} 
\Rdversion{1.1} 
\alias{loglm} 
\title{Add a linear model to a plot}
\description{Plot the regression line of a linear model to a plot}
\usage{ 
loglm(mod,log='xy',range=c(1e-1,1e2),...)
}

\arguments{ 
\item{mod}{The matrix to be plotted. X and Y coordinates must be passed as rownames and colnames respectively} 
\item{log}{The axes to be transformed to log} 	
\item{range}{The interval on which the regression line is to be plotte}
\item{...}{Further arguments to be passed to addlog}
}

\examples{ 
a <- seq(from=1,to=10,by=0.1)
b <- a + abs(rnorm(length(a),0,5))
c <- 2*a + abs(rnorm(length(a),0,5))
logplot(a,b,log='xy',pch=19,ylim=c(1,100))
addlog(a,c,log='xy',pch=19,col='grey')
model <- lm(b~a)
model.2 <- lm(c~a)
loglm(model,col='red',log='xy')
loglm(model.2,col='blue',log='xy',range=c(2,8),lty=3)
}