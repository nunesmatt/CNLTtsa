\name{cnlt.univ}
\alias{cnlt.univ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Performs 'nondecimated' complex-valued wavelet lifting for univariate time series analysis}
\description{
The forward complex-valued lifting transform for decomposing a signal of interest is dependent on the trajectory (lifting order) used in the forward lifting transform.  
This procedure uses trajectory bootstrapping to provide (complex-valued) time-scale information at all times and scales
}
\usage{
cnlt.univ(x, f, P = 100, nkeep = 2, verbose = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector of grid values. Can be of any length, not necessarily equally spaced.}
  \item{f}{A vector of function values corresponding to \env{x}. Must be of the same length as \env{x}.}
  \item{P}{Number of trajectories to be used in the nondecimated lifting transform.}
  \item{nkeep}{Number of scaling points we want at the end of the transform. The usual choice is \code{nkeep}=2.}
\item{verbose}{Indicates whether useful messages should be printed to the console during the procedure.}
  \item{\dots}{Any other arguments to be passed to \code{\link[CNLTreg]{fwtnppermC}}, see the function documentation for more details.}
}
\details{
Essentially, this function applies the forward complex wavelet lifting transform \code{\link[CNLTreg]{fwtnppermC}} \env{P} times, each with a different random lifting 
trajectory.  This results in \env{P} sets of complex-valued detail coefficients, along with their associated scales.  This information is stored in order to compute the 
periodogram for (\env{x},\env{f}).  The ``degree of asymmetry" in the prediction is also recorded.  
This is the ratio between the maximum distance to the removed point to one of its neighbours and the minimum distance from the removed point to one of its neighbours, 
see Chapter 5.3 in Sanderson (2010) for more details.
}
\value{
An object of class \code{cnlt} (subclasses: \code{univ}, \code{SG}).  A list with components:\cr
\item{x}{The sampling grid corresponding to \env{f} used for the decomposition.}
  \item{det1}{A list, entry \code{i} corresponding to detail coefficients associated to point \code{x_i}.}
\item{lre}{A list, entry \code{i} corresponding to the scales (integrals) when lifting point \code{x_i}.}
\item{lreA}{A list, entry \code{i} corresponding to the degree of asymmetry of the neighbourhood used in the prediction step of point \code{x_i}, see description above.}
%\item{scalelength}{The maximum scale for the time-scale representation of the data, essentially \code{max{log_2(lre)}}.}
%\item{P}{Number of trajectories used in the nondecimated lifting transform.}
}
\references{
Hamilton, J., Nunes, M. A., Knight, M. I. and Fryzlewicz, P. (2018) Complex-valued wavelet lifting and applications.
\emph{Technometrics}, \bold{60} (1), 48-60, DOI 10.1080/00401706.2017.1281846.\cr\cr
Sanderson, J. (2010) Wavelet methods for time series with bivariate observations and irregular sampling grids. PhD Thesis, University of Bristol, UK.\cr\cr
For the real-valued equivalent procedure, see also\cr
Knight, M. I., Nunes, M. A. and Nason, G. P. (2012) Spectral Estimation for Locally Stationary Time Series with Missing Observations. \emph{Stat. Comput.}
\bold{22} (4), 877--895.
}
\author{
Jean Hamilton, Matt Nunes
}
\section{Warning}{Using a large number of trajectories for long datasets could take a long time!}
\seealso{
\code{\link[CNLTreg]{fwtnppermC}},
\code{link{get.percoeffsC.biv}}
}
\examples{

x<-sort(runif(100))

y <-sin(2*pi*(1/25)*x) + sin(2*pi*(1/50)*x)

\dontrun{
xy.dec<-cnlt.univ(x,y,P=300)

xy.dec$det[[1]][1:10]
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}% use one of  RShowDoc("KEYWORDS")
