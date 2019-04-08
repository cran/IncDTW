\name{dtw_dismat}
\alias{dtw_dismat}
\alias{dtw_disvec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DTW Distance Matrix
}
\description{
Calculates a matrix of pairwise DTW distances for many multivariate time series, or a vector of DTW distances all relative to one query time series. A parallel computation is possible. }
\usage{
dtw_dismat(lot, dist_method = c("norm1", "norm2", "norm2_square"),
         step_pattern = c("symmetric2", "symmetric1"), normalize = TRUE,
         ws = NULL, threshold = NULL,
         return_matrix = TRUE, ncores = NULL, useRcppParallel = TRUE)

dtw_disvec(Q, lot, dist_method = c("norm1", "norm2", "norm2_square"),
         step_pattern = c("symmetric2", "symmetric1"), normalize = TRUE,
         ws = NULL, threshold = NULL, ncores = NULL)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Q}{time series, vector or matrix}
\item{lot}{List of time series. Each entry of the list is a time series as described in \code{\link{dtw2vec}}.}
\item{dist_method}{character, describes the method of distance measure. See also \code{\link{dtw}}.}
\item{step_pattern}{character, describes the step pattern. See also \code{\link{dtw}}.}
\item{normalize}{logical, whether to return normalized pairwise distances or not. If \code{step_pattern == 'symmetric1'} only non-normalized distances can be returned (default = TRUE)}
\item{ws}{
integer, describes the window size for the sakoe chiba window. If NULL, then no window is applied. (default = NULL)
}
\item{threshold}{numeric, the threshold for early abandoning. In the calculation of the global cost matrix a possible path stops as soon as the threshold is reached. Facilitates faster calculations in case of low threshold. (default = FALSE)
}
\item{return_matrix}{boolean, If TRUE (default) a symmetric matrix of differences is returned. If FALSE the distance matrix is returned as \code{dist} object. 
}
\item{ncores}{integer, number of cores to be used for parallel computation of the distance matrix. If \code{ncores = NULL} (default) then ncores is set to the number of available cores minus 1. If \code{ncores = 0} then no parallel computation is performed and standard \code{sapply} instead of \code{parallel::parSapply} is applied.}
\item{useRcppParallel}{boolean, if the package RcppParallel (TRUE, default) or parallel  (FALSE) is used for parallel computation}
}


\details{
no matrices are allocated, no matrices are returned
}
\value{
\item{input}{the function input parameters}
\item{dismat}{the matrix of pairwise DTW distances, either as matrix or dist object}
\item{disvec}{the vector DTW distances}
}

\author{
Maximilian Leodolter
}

\examples{
lot <- lapply(1:10, function(i){matrix(rnorm(100), ncol=2)})

\dontrun{
# per default all minus 1 available cores are used
result <- dtw_dismat(lot = lot, dist_method = "norm2", ws = 20, 
                  return_matrix = FALSE)
}

# for few time series you can set ncores=0 for NON-parallel computation
result <- dtw_dismat(lot = lot, dist_method = "norm2", ws = 20, 
                  return_matrix = FALSE, ncores = 1)

}

\keyword{ classif }% use one of  RShowDoc("KEYWORDS")
\keyword{ cluster }% use one of  RShowDoc("KEYWORDS")
\keyword{ ts }% __ONLY ONE__ keyword per line

