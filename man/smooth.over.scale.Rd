\name{smooth.over.scale}
\alias{smooth.over.scale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to perform smoothing over scale of spectral quantities
}
\description{
This function uses simple averaging or smoothing splines to smooth spectra over scale
}
\usage{
smooth.over.scale(x, det1, det2, lre, lreA, scale.range = NULL, Arange = NULL, 
Jstar = 20, splines = FALSE, positive = FALSE, dfS = 10, interpolate = FALSE)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{A vector corrsponding to the sampling grid of the component of a univariate series, or both components of a bivariate series with identical sampling grids.}
  \item{det1}{A list of (real or imaginary parts of) the component 1 detail coefficients from a CNLT decomposition, such as from the output of \code{\link{cnlt.biv}}.}
  \item{det2}{A list of (real or imaginary parts of) the component 2 detail coefficients from a CNLT decomposition, such as from the output of \code{\link{cnlt.biv}}.}
  \item{lre}{A list of scales (removed integral lengths) corresponding to \env{det} from a CNLT decomposition, such as from the output of \code{\link{cnlt.biv}}.}
  \item{lreA}{A list of asymmetry values from a CNLT decomposition, such as from the output of \code{\link{cnlt.biv}}.}
  \item{scale.range}{An optional two-vector specifying the range of scales to be considered in the resulting output spectrum.}
  \item{Arange}{An optional two-vector specifying whether the values used in forming the output spectrum should be limited to those from a specific range of asymmetry values,  
 see Sanderson (2010), chapter 6.2.}
  \item{Jstar}{The number of artificial scales in the output spectrum.}
  \item{splines}{An indicator variable whether smoothing splines should be used for the scale-based smoothing, or simple averaging (\code{splines = FALSE}).}
  \item{positive}{An indicator variable whether the smoothing should ensure that the resulting output is positive or not (e.g. for spectra).}
  \item{dfS}{An argument, if \code{splines = TRUE}, specifying the number of degrees of freedom for the smoothing spline.}
  \item{interpolate}{An indicator variable for whether interpolation should be used in the smoothing spline method for predicting values outside the range of the data.}
}

\details{
        For a univariate series or a bivariate series where the two components have the same sampling grids, the co- /quadrature periodogram values are first formed. 
        They are then smoothed over scale (per timepoint), to give spectral values corresponding to equal artificial levels by setting \env{Jstar} and optionally 
	\env{scale.range}.  
}
\value{
A list with the following components:\cr
\item{spec}{A matrix of dimension \code{Jstar x length(x)} corrsponding to a periodogram / co-periodogram / quadrature periodogram.}
\item{mscale}{A vector of scales (of length \env{Jstar}) corresponding to the rows of the spectrum \code{spec}.}
}
\references{
Hamilton, J., Nunes, M. A., Knight, M. I. and Fryzlewicz, P. (2017) Complex-valued wavelet lifting and applications.
\emph{Technometrics (to appear)} DOI 10.1080/00401706.2017.1281846.
}
\author{
Jean Hamilton, Matt Nunes
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cnlt.spec.SG}}
}
\examples{
x<-sort(runif(100))

y <-sin(2*pi*(1/25)*x) + sin(2*pi*(1/50)*x)

\dontrun{
xy.dec<-cnlt.univ(x,y,P=300)

# compute the real part of the spectrum (real details^2) and smooth over scale
ReS <- smooth.over.scale(x, sapply(xy.dec$det1,Re), sapply(xy.dec$det1,Re), xy.dec$lre, 
xy.dec$lreA, positive = TRUE)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}% use one of  RShowDoc("KEYWORDS")
