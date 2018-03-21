\name{pre.per}
\alias{pre.per}
\alias{pre.per.sample}
\alias{pre.per.comb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to form periodogram objects with a common time and scale bins for bivariate series with different sampling grids for each component
}
\description{
The CNLT forms detail coefficients for each component of a bivariate series.  Due to the two components having different sampling grids, the details (and associated scales) won't have a common association for both series.  Hence the details are sampled and mapped to a common timescale and a common set of scales via binning and averaging.  These functions compute spectral objects on these common times / scales
}
\usage{
pre.per(x, det, lre, lreA, scale.range = NULL, time.range = NULL, Arange = NULL, 
Jstar = 20, Tstar = 50)
pre.per.comb(spec1, spec2)
pre.per.sample(spec1, spec2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector corrsponding to the sampling grid of a component of a series.}
  \item{det}{A list of (real or imaginary parts of) the detail coefficients from a CNLT decomposition, such as from the output of \code{\link{cnlt.biv}}.}
  \item{lre}{A list of scales (removed integral lengths) corresponding to \env{det} from a CNLT decomposition, such as from the output of \code{\link{cnlt.biv}}.}
  \item{lreA}{A list of asymmetry values from a CNLT decomposition, such as from the output of \code{\link{cnlt.biv}}.}
  \item{scale.range}{An optional two-vector specifying the range of scales to be considered in the resulting output spectrum.}
  \item{time.range}{An optional two-vector specifying the range of times to be considered in the resulting output spectrum.}
  \item{Arange}{An optional two-vector specifying whether the values used in forming the output spectrum should be limited to those from a specific range of asymmetry values,
 see Sanderson (2010), chapter 6.2.}
  \item{Jstar}{The number of artificial scales in the output spectrum.}
  \item{Tstar}{The number of artificial times in the output spectrum.}
  \item{spec1}{A periodogram corresponding to the first component of a bivariate series.}
  \item{spec2}{A periodogram corresponding to the second component of a bivariate series.}
}
\details{
	For a bivariate series where the two components have different sampling grids, the co- /quadrature periodogram values are first formed using \code{pre.per}, 
	using a vector of \env{Tstar} equal time intervals, specified by setting \env{Tstar} and optionally \env{time.range}; they are also binned into \env{Jstar} 
	equal artificial levels by setting \env{Jstar} and optionally \env{scale.range}.  The details are sampled using a common sampling vector with \code{pre.per.sample}, 
	and then combined using \code{pre.per.comb}. The periodogram is then smoothed over time. See Hamilton et al (2017), section 2.3 for more details. 
}
\value{
For \code{pre.per}, a list with components:\cr
\item{spec}{A matrix of dimension \code{Jstar x Tstar} corrsponding to a quadrature periodogram / co-periodogram of a bivariate series.}
\item{mscale}{A vector of scales (of length \env{Jstar}) corresponding to the rows of the spectrum \code{spec}.}
\item{mtime}{A vector of times (of length \env{Tstar}) corresponding to the columns of the spectrum \code{spec}.}

For \code{pre.per.sample}, a list with components:\cr
\item{spec1}{A matrix of dimension \code{Jstar x Tstar} corrsponding to a periodogram of the first component of a bivariate series.}
\item{spec2}{A matrix of dimension \code{Jstar x Tstar} corrsponding to a periodogram of the second component of a bivariate series.}

For \code{pre.per.comb}, a list with components:\cr
\item{spec}{A matrix of dimension \code{Jstar x Tstar} corrsponding to a periodogram / quadrature periodogram / co-periodogram of a bivariate series.}

}
\references{
Hamilton, J., Nunes, M. A., Knight, M. I. and Fryzlewicz, P. (2017) Complex-valued wavelet lifting and applications.
\emph{Technometrics (to appear)} DOI 10.1080/00401706.2017.1281846.
}
\author{
Jean Hamilton, Matt Nunes
}
\note{
Note that these functions aren't intended to be used directly, but are called internally from the function \code{\link{cnlt.spec.DG}}.  \cr
Note also that the argument \env{Tstar} should be chosen small enough so that the range of the sampling grid \env{x} can be divided into equally spaced intervals, 
with *at least one* gridpoint in an interval.  
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cnlt.spec.DG}},
\code{\link{smooth.over.time}}
}
\examples{

# simulate data, e.g. two sinusoids
dat <- seq(from=1, to=3, by=0.1)
x1 <- cumsum(sample(dat, 200, TRUE))
x2 <- cumsum(sample(dat, 200, TRUE))

y1 <-sin(2*pi*(1/25)*x1) + sin(2*pi*(1/50)*x1)+ 1*sin(2*pi*(1/10)*x1)+ rnorm(length(x1), 0,0.2)
y3 <- sin(2*pi*(1/25)*x2[97:196]) + rnorm(length(x2), 0,0.1)

\dontrun{
y1y3.dec<-cnlt.biv(x1, f1=y1, f2=y3, P = 500)

# compute the co-periodogram for the first component...

C1 <- pre.per(x1, sapply(y1y3.dec$det1,Re), y1y3.dec$lre1, y1y3.dec$lreA1, Jstar = 10)

# .. and for the second component
C2 <- pre.per(x1, sapply(y1y3.dec$det2,Re), y1y3.dec$lre2, y1y3.dec$lreA2, Jstar = 10)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}% use one of  RShowDoc("KEYWORDS")
