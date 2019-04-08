\name{find_peaks}
\alias{find_peaks}
\title{find_peaks}
\usage{
find_peaks(x, w, get_min = TRUE, strict = TRUE)
}
\arguments{
\item{x}{vector}

\item{w}{window, at least w-many values need to be in-between two consecutive
peaks to find both, otherwise only the bigger one is returned}

\item{get_min}{logical (default TRUE) if TRUE, then minima are returned, else maxima}

\item{strict}{logical, if TRUE (default) then a local minimum needs to be smaller then all neighbors. 
If FALSE, then a local minimum needs to be smaller or equal all neighbors.}
}
\value{
integer vector of indices where x has local extreme values
}
\description{
Find negative or positive peaks of a vector in a predefined neighborhood w
}
\examples{

# finds also the border peak maximum at index 29
x <- c(1:10, 9:1, 2:11)
peak_indices <- find_peaks(x, w=3, get_min=FALSE)
peak_indices
x[peak_indices]

# and the border peak at index 1
peak_indices <- find_peaks(x, w=3, get_min=TRUE)
peak_indices
x[peak_indices]


# what means the neigbohood parameter w:
# at least w-many values need to be inbetween two consecutive peaks
x <- -c(1:10, 9, 9, 11, 9:8, 7)
peak_indices <- find_peaks(x, w=3)
peak_indices
x[peak_indices]

x <- -c(1:10, 9, 9,9, 11, 9:8, 7)
peak_indices <- find_peaks(x, w=3)
peak_indices
x[peak_indices]


# what means the strict parameter:
x <- c(10:1,  1:10)
peak_indices <-  find_peaks(x, w=3, strict = TRUE)
peak_indices
x[peak_indices]

peak_indices <-  find_peaks(x, w=3, strict = FALSE)
peak_indices
x[peak_indices]
   
}

\author{
Maximilian Leodolter
}
