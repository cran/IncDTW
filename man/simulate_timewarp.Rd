\name{simulate_timewarp}
\alias{simulate_timewarp}
%%\alias{insert_const}
%%\alias{insert_linear_interp}
%%\alias{insert_norm}
%%\alias{insert_linear_norm}

\title{
Simulate time warp
}
\description{
Simulate a time warp for a given time series. 
}
\usage{
simulate_timewarp(x, stretch = 0, compress = 0, 
                  stretch_method = insert_linear_interp,
                  p_index = "rnorm", p_number = "rlnorm", 
                  p_index_list = NULL, p_number_list = NULL, 
                  seed = NULL, ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{time series, vector or matrix}
\item{stretch}{numeric parameter for stretching the time series. \code{stretch >= 1}, see details}
\item{compress}{numeric parameter for compressing the time series. \code{compress >= 0 & compress < 1}, see details}
\item{stretch_method}{function, either one of (insert_const, insert_linear_interp,
insert_norm, insert_linear_norm), or any user defined function that needs the parameters \code{x} (univariate time series as vector), \code{ix} (index where to insert), \code{N} (number of observations to insert) and any other arguments required for that function. See Details. }
\item{p_index}{string, distribution for simulating the indices where to insert simulated observations, e.g. "rnorm", "runif", etc.}
\item{p_number}{string, distribution for simulating the number of obervations to insert per index, e.g. "rnorm", "runif", etc.}
\item{p_index_list}{list of named parameters for the distribution \code{p_index}}
\item{p_number_list}{list of named parameters for the distribution \code{p_number}}
\item{seed}{set a seed for reproducible results}
\item{...}{named parameters for the \code{stretch_method}}
}



\details{
The different distributions \code{p_index} and \code{p_number} also determine the behaviour of the warp. A uniform distribution for \code{p_number} more likely draws high number than e.g. log-normal distributions with appropriate parameters. So, a uniform distribution more likely simulates fewer, but longer warps, that is points of time where the algorithm inserts simulations.


The algorithm stretches by randomly selecting an index between 1 and the length of the time series. Then a number of observations to be inserted is drawn out of the range 1 to the remaining number of observations to be inserted. These observations are inserted. Then the algorithm starts again with drawing an index, drawing a number of observations to be inserted, and proceeds until the requested time series length is achieved.

The algorithm for compressing works analogous, except it simply omits observations instead of linear interpolation.


The parameter \code{stretch} describes the ratio how much the time series \code{x} is stretched:
e.g. if \code{compress = 0} and ...
\itemize{
\item \code{stretch = 0} then \code{length(x_new) = length(x)}, or
\item \code{stretch = 0.1} then \code{length(x_new) = length(x) * 1.1}, or             
\item \code{stretch = 1} then \code{length(x_new) = length(x) * 2}                        
}

The parameter \code{compress} describes the ratio how much the time series \code{x} is compressed:
e.g. if \code{stretch = 0} and ...
\itemize{
\item \code{compress = 0} then \code{length(x_new) = length(x)}, or
\item \code{compress = 0.2} then \code{length(x_new) = length(x) * 0.8}
}


There are four functions to chose from to insert new simulated observations. You can also define yur own function and apply this one. The four functions to chose from are:
\itemize{
\item insert a constant, either a constant defined by the user via the iniput parameter \code{const}, or if \code{const = NULL}, then the last observation of the time series where the insertion starts is set as \code{const}
\item insert linear interpolated observations (default)
\item insert a constant with gaussian noise 
\item insert linear interpolated observations and add gaussian noise.
}

For the methods with gaussian noise the parameters \code{mean} and \code{sd} for \code{rnorm} can be set at the function call of \code{simulate_timewarp()}.

}
\value{
\item{}{time warped time series}
}

\author{
Maximilian Leodolter
}

\examples{
\dontrun{
set.seed(123)
x <- cumsum(rnorm(100))
x_new <- simulate_timewarp(x, stretch = 0.1, compress = 0.2, seed = 123) 
plot(x, type = "l")
lines(x_new, col = "red")

y <- matrix(cumsum(rnorm(10^3)), ncol = 2)


# insert NA with uniform distributions
y_warp <- simulate_timewarp(y, stretch = 0.2, p_number = "runif", p_index = "runif",
                          stretch_method = insert_const, 
                          const = NA)
matplot(y_warp, type = "l")


# insert NA with log-normal distribution
y_warp <- simulate_timewarp(y, stretch = 0.2, p_number = "rlnorm", 
                          p_number_list = list(meanlog = 0, sdlog = 1),
                          stretch_method = insert_const, 
                          const = NA)
matplot(y_warp, type = "l")


# insert linear interpolation
y_warp <- simulate_timewarp(y, stretch = 0.2, p_number = "rlnorm", 
                          stretch_method = insert_linear_interp)
matplot(y_warp, type = "l")


# insert random walk with gaussian noise
y_warp <- simulate_timewarp(y, stretch = 0.2, p_number = "rlnorm", 
                          stretch_method = insert_norm,
                          sd = 1, mean = 0)
matplot(y_warp, type = "l")


# insert constant, only 1 observation per random index
y_warp <- simulate_timewarp(y, stretch = 0.2, p_number = "runif", p_index = "runif",
                            p_number_list = list(min=1, max=1),   
                            stretch_method = insert_const)
matplot(y_warp, type = "l")

}
}

\keyword{ ts }% __ONLY ONE__ keyword per line

