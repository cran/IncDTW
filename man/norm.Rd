\name{norm}
\alias{norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Time Series Normalization
}
\description{
normalizes a time series per dimension
}
\usage{
norm(x, type = c('z', '01'),
     xmean = NULL, xsd = NULL, xmin = NULL, xmax = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{time series as vector or matrix
}
\item{type}{character, describes the method of to normalize the time series (per column if x is multivariate). The parameter type is either 'z' for z-normalization or '01' for max-min normalization.
}
\item{xmean}{mean used for z-normalization
}
\item{xsd}{standard deviation used for z-normalization
}
\item{xmin}{minimum used for 0-1 normalization
}
\item{xmax}{maximum used for 0-1 normalization
}
}


\details{
For a vector \code{x} the z-normalization subtracts the mean and devides by the standard deviation:  of \code{(x-mean(x))/sd(x)}. The min-max normalization performs \code{(x-min(x))/(max(x)-min(x))}.
}

\value{
\item{x}{the normalized vector}
}
\author{
Maximilian Leodolter
}

\examples{
x <- cumsum(rnorm(100, 10, 5))
y <- norm(x, "01")
z <- norm(x, "z")
}

\keyword{ classif }% use one of  RShowDoc("KEYWORDS")
\keyword{ cluster }% use one of  RShowDoc("KEYWORDS")
\keyword{ ts }% __ONLY ONE__ keyword per line

