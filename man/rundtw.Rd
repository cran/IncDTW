\name{rundtw}
\alias{rundtw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
rundtw
}
\description{
Detect recurring patterns similar to given query pattern by measuring the distance with DTW. A window of the length of the query pattern slides along the longer time series and calculates computation-time-efficiently the DTW distance for each point of time. The function incrementally updates the normalization of the sliding window, incrementally updates the cost matrix, applies the vector-based implementation of the DTW algorithm, early abandons and applies lower bounding methods to decrease the calculation time.
}
\usage{
rundtw(Q, C, dist_method = c("norm1", "norm2", "norm2_square"), 
      step_pattern = c("symmetric1", "symmetric2"), k = NULL, 
      normalize = c("01", "z", "none"), 
      ws = NULL, threshold = NULL, lower_bound = TRUE, overlap_tol = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Q}{
vector or matrix, the query time series
}
  \item{C}{
vector or matrix (equal number of columns as Q), the longer time series which is scanned for multiple fits of the query time series. C can also be a list of time series (either all vectors, or all matrices of equal number of columns) with varying lengths. See Details.
}
  \item{dist_method}{
see \code{\link{dtw}}
}
  \item{step_pattern}{
see \code{\link{dtw}}, only for "symmetric1" the lower bounding is implemented yet
}
  \item{k}{
integer >= 0. If \code{k > 0}, then the k-nearest neighbors to the query pattern that are found in all possible sub-sequences of the long time series C are returned. Per default the found fits don't overlap, except the \code{overlap_tol} parameter is adjusted (this should be done with care!). If \code{k > 0} then \code{lowerbound} is set to \code{TRUE}.
}
  \item{normalize}{
character, one of c("01", "z", "none") (default = "01"), if not "none" then \code{Q} (once at the start) and \code{C} (running normalization) are normalized. Either min-max ("01") or the z-normalization ("z")  is applied. TRUE (identical to '01') and FALSE (identical to 'none') are deprecated and will be dropped in the next package version.
}
  \item{ws}{
see \code{\link{dtw}}
}
  \item{threshold}{
numeric >= 0, global threshold for early abandoning DTW calculation if this threshold is hit. (also see \code{\link{dtw}}). If NULL (default) no early abandoning is applied.  
}
\item{lower_bound}{
logical, (default = TRUE) If TRUE (default) then lower bounding is applied (see Details).
}
\item{overlap_tol}{
integer between 0 and length of Q, (default = 0) gives the number of observations that two consecutive fits are accepted to overlap. 
}
}


\value{
\item{dist}{vector of DTW distances}
\item{counter}{named vector of counters. Gives information how the algorithm proceeded. see Details}
\item{knn_indices}{indices of the found kNN}
\item{knn_values}{DTW distances of the found kNN}
\item{knn_list_indices}{indices of list entries of C, where to find the kNN. Only returned if C is a list of time series. See examples.}
}


\details{
This function and algorithm was inspired by the work of [3] and refined for running min-max normalization and lower bounding.

Lower Bounding: The following methods are implemented:
\itemize{
\item \code{LB_Keogh} [1] for univariate time series 
\item \code{LB_MV} [2] for multivariate time series with the \code{dist_method = "norm2_square"}
\item Adjusted for different distance methods "norm1" and "norm2", inspired by [2].
}


Counter vector:
\itemize{
\item "norm_reset" counts how many times the min and max of the sliding window and the normalization need to be reset completely
\item "norm_new_extreme" how many times the min or max of the sliding window are adjusted incrementally and the normalization need to be reset completely
\item "norm_1step" how many times only the new observation in the sliding window needs to be normalized based on the current min and max
\item "cm_reset" how many times the cost matrix for the sliding window needs to be recalculated completely
\item "cm_1step" how many times only the front running column of the cost matrix is calculated
\item "early_abandon" how many times the early abandon method aborts the DTW calculation before finishing
\item "lower_bound" how many times the lower bounding stops the initialization of the DTW calculation
\item "completed" for how many subsequences the DTW calculation finished
}



C is a list of time series:
If C is a list of time series, the algorithm concatenates the list to one long time series to apply the logic of early abandoning, lower bounding, and finding the kNN. Finally the results are split to match the input. The 
}




\references{
\itemize{
\item [1] Keogh, Eamonn, and Chotirat Ann Ratanamahatana. "Exact indexing of dynamic time warping." Knowledge and information systems 7.3 (2005): 358-386.
\item [2] Rath, Toni M., and R. Manmatha. "Lower-bounding of dynamic time warping distances for multivariate time series." University of Massachusetts Amherst Technical Report MM 40 (2002).
\item [3] Sakurai, Yasushi, Christos Faloutsos, and Masashi Yamamuro. "Stream monitoring under the time warping distance." Data Engineering, 2007. ICDE 2007. IEEE 23rd International Conference on. IEEE, 2007.
}

}
\author{
Maximilian Leodolter
}

\examples{

\dontrun{
#--- univariate case

noise <- function(nr) cumsum(rnorm(nr))
set.seed(123)
nx <- 500
nh <- 40
nfits <- 5

nn <- nx - nfits * nh# nnoise
nn <- nn/nfits

h <- sin(1:nh)
hnorm <- IncDTW::norm( h , type="01")
x <- noise(0)
for(i in 1:nfits){
   x <- c(x, noise(nn)  , 
          h * abs(rnorm(1, 10,10))+ rnorm(1, 0, 10))
}

# DO norm and DO lower bound
result <- rundtw(h, x, normalize = '01', ws = 10, threshold = Inf, lower_bound = TRUE)

## have a look into the result and get best indices with lowest distance
minima <- find_peaks(result$dist, w = nh, get_min = TRUE)
par(mfrow=c(2,1))
plot(x, type = "l")
for(i in minima) lines(x = i:(i+nh-1), y = x[i:(i+nh-1)], col="red")
plot(result$dist, xlim=c(1, nx))
points(x = minima, y = result$dist[minima], col="red", pch=16)


# DO min-max-norm and DO lower bound
rundtw(h, x, normalize = '01', ws = 10, threshold = Inf, lower_bound = TRUE)


# DO z-norm and DO lower bound
rundtw(h, x, normalize = 'z', ws = 10, threshold = NULL, lower_bound = TRUE)

# DONT norm and DONT lower bound
rundtw(h, x, normalize = 'none', ws = 10, threshold = NULL, lower_bound = FALSE)


# kNN: DO z-norm and DO lower bound 
rundtw(h, x, normalize = '01', ws = 10, k = 3)


#--- multivariate case
noise <- function(nr, nc) matrix(cumsum(rnorm(nr * nc)), nrow = nr, ncol = nc)

nx <- 500
nh <- 50
nc <- 2
nfits <- 5

nn <- nx - nfits * nh# nnoise
nn <- nn/nfits

h <- noise(nh, nc)
hnorm <- IncDTW::norm( h , type="01")
x <- matrix(numeric(), ncol=nc)
for(i in 1:nfits){
   x <- rbind(x, noise(nn, nc), h)
}

# DO min-max-norm and DO lower bound
rundtw(h, x, normalize = '01', ws = 10, threshold = Inf, lower_bound = TRUE)


# DO z-norm and DO lower bound
rundtw(h, x, normalize = 'z', ws = 10, threshold = NULL, lower_bound = TRUE)

# DONT norm and DONT lower bound
rundtw(h, x, normalize = 'none', ws = 10, threshold = NULL, lower_bound = FALSE)



#--- C is a list of multivariate time series
noise <- function(nr, nc) matrix(cumsum(rnorm(nr * nc)), nrow = nr, ncol = nc)

nx <- 500
nh <- 50
nc <- 2

h <- noise(nh, nc)
hnorm <- IncDTW::norm( h , type="01")
x1 <- rbind(noise(100, nc), h, noise(20, nc))
x2 <- rbind(noise(10, nc), h, noise(50, nc))
x3 <- rbind(noise(200, nc), h, noise(30, nc))
x <- list(x1, x2, x3)

# DO min-max-norm and DO lower bound
rundtw(h, x, normalize = '01', ws = 10, threshold = Inf, lower_bound = TRUE, k = 3)

# DO z-norm and DO lower bound
rundtw(h, x, normalize = 'z', ws = 10, threshold = Inf, lower_bound = TRUE, k = 3)

# DONT norm and DONT lower bound
rundtw(h, x, normalize = 'none', ws = 10, threshold = Inf, lower_bound = TRUE, k = 3)

}
}
