\name{dtw}
\alias{dtw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dtw
}
\description{
wrapper function for the C++ calculations and the BACKTRACK_cpp that finds the cheapest warping path.
}
\usage{
dtw(Q, C, ws = NULL, return_diffM = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Q}{
one dimensional numeriv vector
}
\item{C}{
one dimensional numeriv vector
}
\item{ws}{
integer, describes the window size for the sakoe chiba window. If NULL, then no window is applied. (default = NULL)
}
\item{return_diffM}{
boolean, if TRUE then the Matrix of differences (not the absolute value) is returned. (default = FALSE)
}
}
\details{
The dynamic time warping distance is the element in the last row and last column of the global cost matrix.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{gcm}{global cost matrix}
\item{dm}{direction matrix (3=up, 1=diagonal, 2=left)}
\item{wp}{warping path}
\item{ii}{indices of C of the optimal path}
\item{jj}{indices of Q of the optimal path}
\item{diffM}{Matrix of differences}
}
\references{
Sakoe, H.; Chiba, S., Dynamic programming algorithm optimization for spoken word recognition, Acoustics, Speech, and Signal Processing [see also IEEE Transactions on Signal Processing], IEEE Transactions on , vol.26, no.1, pp. 43-49, Feb 1978. http://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=1163055
}
\author{
Maximilian Leodolter
}

\examples{
Q <- cumsum(rnorm(100))
C <- Q[11:100] + rnorm(90, 0, 0.5)
dtw(Q=Q, C=C, ws = 15, return_diffM = FALSE)
dtw(Q=Q, C=C, ws = 15, return_diffM = TRUE)
}
\keyword{ cluster }% use one of  RShowDoc("KEYWORDS")
\keyword{ ts }% __ONLY ONE__ keyword per line

