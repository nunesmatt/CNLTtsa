\name{smooth.over.time}
\alias{smooth.over.time}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to perform smoothing over time of spectral quantities
%%  ~~function to do ... ~~
}
\description{
This function uses a running mean (box kernel) smoother to smooth spectra over time, with potentially different smoothing parameters used for each scale of the spectra
}
\usage{
smooth.over.time(x, spec, M, fact = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{A vector corrsponding to the sampling grid of a component of a series.}
  \item{spec}{A spectral object (matrix), with rows corrsponding to different scales.}
  \item{M}{The smoothing parameter (binwidth) or vector of smoothing parameters (one for each scale) for the smoothing method.}
  \item{fact}{If \code{length(M)==1}, a factor indicating how the smoothing parameter (binwidth) in the time-domain kernel smoothing method should increase from one scale to the next.}
}
\details{
The function takes in a matrix and performs a kernel smoother on row \code{i} of the matrix, using a bandwidth of \code{M[i]} if \code{length(M)==nrow(spec)}, and \code{M * fact^{i-1}} if \code{length(M)==1}.  Thus if the scaling factor, \env{fact}, is chosen to be greater than one, a wider kernel is used for the smoothing for later scales. 
}
\value{
\item{smooth.spec}{A matrix of same dimension  as \env{spec}, containing smoothed spectral values.}
}
\references{
Hamilton, J., Nunes, M. A., Knight, M. I. and Fryzlewicz, P. (2017) Complex-valued wavelet lifting and applications.
\emph{Technometrics (to appear)} DOI 10.1080/00401706.2017.1281846.
}
\author{
Jean Hamilton
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cnlt.spec}},
\code{\link{pre.per}}
}
\examples{

x<-sort(runif(100))

y <-sin(2*pi*(1/25)*x) + sin(2*pi*(1/50)*x)

\dontrun{
xy.dec<-cnlt.univ(x,y,P=300)

# compute the real part of the spectrum (real details^2) and smooth over scale, then over time
ReS <- smooth.over.scale(x, sapply(xy.dec$det1,Re), sapply(xy.dec$det1,Re), xy.dec$lre, 
xy.dec$lreA, positive = TRUE)

ReS.smooth <- smooth.over.time(x,ReS$spec,5,1.05)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}% use one of  RShowDoc("KEYWORDS")
