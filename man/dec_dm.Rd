\name{dec_dm}
\alias{dec_dm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dec_dm
}
\description{
  wrapper function for the C++ implementation of an decremental calculation of the warping path subject to a given direction matrix for dynamic time warping
}
\usage{
dec_dm(dm, Ndec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{dm}{direction matrix, output from dtw(Q=Q, C=C, ws=ws)}
\item{Ndec}{integer, number of observations (columns) to be reduced}
}
\value{
\item{wp}{warping path}
\item{ii}{indices of C of the optimal path}
\item{jj}{indices of Q of the optimal path}
}
\references{
Sakoe, H.; Chiba, S., Dynamic programming algorithm optimization for spoken word recognition, Acoustics, Speech, and Signal Processing [see also IEEE Transactions on Signal Processing], IEEE Transactions on , vol.26, no.1, pp. 43-49, Feb 1978.
http://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=1163055
}
\author{
Maximilian Leodolter
}

\examples{
Q <- cos(1:100)
C <- cumsum(rnorm(80))
# the ordinary calculation
result_base <- dtw(Q=Q, C=C, return_diffM = FALSE) 

# the ordinary calculation without the last 4 observations
result_decr <- dtw(Q=Q, C=C[1:(length(C) - 4)], return_diffM = FALSE) 
# the decremental step: reduce C for 4 observation
result_decr2 <- dec_dm(result_base$dm, Ndec = 4) 

# compare ii, jj and wp of result_decr and those of 
result_decr$ii
result_decr2$ii
identical(result_decr$ii, result_decr2$ii)

result_decr$jj
result_decr2$jj
identical(result_decr$jj, result_decr2$jj)

result_decr$wp
result_decr2$wp
identical(result_decr$wp, result_decr2$wp)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cluster }% use one of  RShowDoc("KEYWORDS")
\keyword{ ts }% __ONLY ONE__ keyword per line