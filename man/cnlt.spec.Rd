\name{cnlt.spec}
\alias{cnlt.spec}
\alias{cnlt.spec.SG}
\alias{cnlt.spec.DG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to compute CNLT spectral quantities for univariate and bivariate series
}
\description{
The function takes a nondecimated complex lifting decomposition of a univariate or bivariate series, and uses
smoothing before computing spectral quantities such as the complex periodograms, coherence and phase
}
\usage{
cnlt.spec(x, ...)
\method{cnlt.spec}{SG}(x, M = 50, fact = 1, ...)
\method{cnlt.spec}{DG}(x, M = 50, fact = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class \code{cnlt}, i.e. the output from either \code{\link{cnlt.univ}} or \code{\link{cnlt.biv}}.}
  \item{M}{The smoothing parameter (binwidth) or vector of smoothing parameters (one for each scale) for the time-domain kernel smoothing method, see \code{\link{smooth.over.time}}.}
  \item{fact}{If \code{length(M)==1}, a factor indicating how the smoothing parameter (binwidth) in the time-domain kernel smoothing method should increase from 
one scale to the next, see \code{\link{smooth.over.time}}.}
  \item{\dots}{Any other parameters to be passed to the scale smoothing function, see the documentation for \code{\link{smooth.over.scale}} for univariate \code{cnlt} objects, or \code{\link{pre.per}} for bivariate \code{cnlt} objects.}
}
\details{
For univariate series, the nondecimated complex lifting object can be used to form a spectral object by smoothing the squared details over scale (with \code{\link{smooth.over.scale}}), and then smoothing over time (using \code{\link{smooth.over.time}}).  Smoothing over scale is done via \code{smooth.spline}; smoothing over time is done with a kernel smoother (e.g. a "box" kernel for a moving average).
See Hamilton et al. (2018) for more details.	
}
\value{
An object of class \code{cnlt.spec} (subclasses: \code{DG}, \code{SG}, \code{univ}, \code{biv}).  \cr

For subclass \code{univ}, a list with components:\cr
\item{S1}{A spectral object (matrix) of dimension \code{length(mscale) x length(mtime)}, corresponding to the spectrum of the univariate series.}
\item{mscale}{A vector of scales corresponding to the rows of the spectrum \code{S1} (after smoothing the periodogram), see \code{\link{smooth.over.scale}}.}
\item{mtime}{The vector \code{cnltobj$x}, the vector of times corresponding to the columns of the spectrum \code{S1}.}

For subclass \code{biv}, a list with components:\cr
\item{coh}{A matrix of dimension \code{length(mscale) x length(mtime)}, corresponding to the coherence between the two components of the bivariate series.}
\item{phase}{A matrix of dimension \code{length(mscale) x length(mtime)}, corresponding to the phase between the two components of the bivariate series.}
\item{C}{A matrix of dimension \code{length(mscale) x length(mtime)}, corresponding to the co-periodogram of the bivariate series.}
\item{Q}{A matrix of dimension \code{length(mscale) x length(mtime)}, corresponding to the quadrature periodogram of the bivariate series.}
\item{S1}{A matrix of dimension \code{length(mscale) x length(mtime)}, corresponding to the spectrum of the first component of the bivariate series.}
\item{S2}{A matrix of dimension \code{length(mscale) x length(mtime)}, corresponding to the spectrum of the second component of the  bivariate series.}
\item{mscale}{A vector of scales corresponding to the rows of the spectrum \code{S1} (after smoothing the periodogram), see \code{\link{smooth.over.scale}}.}
\item{mtime}{A vector of times corresponding to the columns of the spectrum \code{S1}.  If the class of \code{cnlt.obj} is \code{SG}, this is \code{cnlt.obj$x1}, else this is a vector formed by binning detail coefficients within equal intervals of time, see \code{\link{pre.per}} for more details.}

}
\references{
Hamilton, J., Nunes, M. A., Knight, M. I. and Fryzlewicz, P. (2018) Complex-valued wavelet lifting and applications.
\emph{Technometrics}, \bold{60} (1), 48-60, DOI 10.1080/00401706.2017.1281846.
}
\author{
Matt Nunes, Jean Hamilton
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cnlt.biv}},
\code{\link{cnlt.univ}},
\code{\link{cnltspec.plot}}
}
\examples{

# read some data in (a bivariate series)

\dontrun{

data(Baidu)
data(Google)

BaiGoo<-cnlt.biv(Baidu$Seconds[1:100], Google$Seconds[1:100], Baidu$Return[1:100], 
Google$Return[1:100], P = 500)

specobj<-cnlt.spec(BaiGoo,M=10,fact=1.05, Tstar=20)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}% use one of  RShowDoc("KEYWORDS")
