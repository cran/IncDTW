\name{dtw_partial}
\alias{dtw_partial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Partial Dynamic Time Warping
}
\description{
Get the cheapest partial open end alignment of two time series
}
\usage{
dtw_partial(x, partial_Q = TRUE, partial_C = TRUE, reverse = FALSE)
                     
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{result object of either \code{\link{dtw}()} or \code{\link{idtw2vec}()}
}
\item{partial_Q}{
logical (default = TRUE), whether Q is aligned completely to C or open ended.  
}
\item{partial_C}{
logical (default = TRUE), whether C is aligned completely to Q or open ended. 
}
\item{reverse}{
logical (default = FALSE), whether Q and C are in original or reverse order. 
}
}


\details{
Q is the time series that describes the vertical dimension of the global cost matrix, so \code{length(Q)} is equal to \code{nrow(x$gcm)}. So C describes the horizontal dimension of the global cost matrix, \code{length(C)} is equal to \code{ncol(x$gcm)}.

\code{dtw_partial()} returns the open-end alignment of Q and C with the minimal normalized distance.
If \code{partial_Q} and \code{partial_C} both are \code{TRUE} the partial alignment with the smaller normalized distance is returned.

If Q and C are in reverse order, then the optimal solution for the reverse problem is found, that is the alignment with minimal normalized distance allowing and open-start alignment.

}

\value{
\item{rangeQ}{Vector of initial and ending index for best alignment}
\item{rangeC}{Vector of initial and ending index for best alignment}
\item{normalized_distance}{the normalized DTW distance (see details in \code{\link{dtw}}).}
}

\author{
Maximilian Leodolter
}

\examples{
# open-end alignment for multivariate time series
# first simulate a 2-dim time series Q
Q <- matrix(cumsum(rnorm(100)), ncol=2)

# simulate C as noisy and warped version of Q, and append noise at the end
C <- Q[sort(sample(50, 40, replace=FALSE)), ] + rnorm(80)
C <- rbind(C, matrix(rnorm(30), ncol=2))

tmp <- dtw(Q = Q, C = C, ws = 30, return_QC = TRUE, return_wp = TRUE)
par <- dtw_partial(tmp, partial_C = TRUE)
par
plot(tmp, partial = par, type = "QC")
plot(tmp, partial = par, type = "warp")
plot(tmp, partial = par, type = "QC", selDim = 2)



# open-start is possible as well (open-end is the regular case) since DTW is reversible:
Q <- sin(1:100)
C <- c(rnorm(50), Q)
tmp <- dtw(Q = rev(Q), C = rev(C))
dtw_partial(tmp, reverse = TRUE)

}
\keyword{ cluster }% use one of  RShowDoc("KEYWORDS")
\keyword{ ts }% __ONLY ONE__ keyword per line

