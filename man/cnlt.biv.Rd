\name{cnlt.biv}
\alias{cnlt.biv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Performs 'nondecimated' complex-valued wavelet lifting for bivariate time series analysis
}
\description{
The forward complex-valued lifting transform for decomposing a signal of interest is dependent on the trajectory (lifting order) used in the forward lifting transform.
This procedure uses trajectory bootstrapping to provide (complex-valued) time-scale information at all times and scales for bivariate series
}
\usage{
cnlt.biv(x1, x2 = NULL, f1, f2, P = 100, nkeep = 2, use.same.trajectories = FALSE, 
verbose = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{A vector of grid values. Can be of any length, not necessarily equally spaced.}
  \item{x2}{An optional vector of grid values corresponding to \env{f2}. Can be of any length, not necessarily equally spaced.  If not specified (\code{NULL}), then the same grid is used for \code{f2} as \code{f1}, i.e. \env{x1}.}
  \item{f1}{A vector of function values of the first component of a bivariate series, corresponding to \env{x}. Must be of the same length as \env{x}.}
  \item{f2}{A vector of function values of the second component of a bivariate series, corresponding to \env{x}. Must be of the same length as \env{x}.}
  \item{P}{Number of trajectories to be used in the nondecimated lifting transform.}
  \item{nkeep}{Number of scaling points we want at the end of the transform. The usual choice is \code{nkeep}=2.}
\item{use.same.trajectories}{A boolean variable indicating whether the same set of trajectories should be used for both components of the bivariate signal.}
\item{verbose}{Indicates whether useful messages should be printed to the console during the procedure.}
  \item{\dots}{Any other arguments to be passed to \code{\link[CNLTreg]{fwtnppermC}}, see the function documentation for more details.}
}
\details{
Essentially, this function applies the forward complex wavelet lifting transform \code{\link[CNLTreg]{fwtnppermC}} \env{P} times on both (\env{x},\env{f1}) and (\env{x},\env{f2}), each with a different random lifting
trajectory.  This results in \env{P} sets of complex-valued detail coefficients, along with their associated scales.  This information is stored in order to compute the
cross-periodograms for the bivariate series (\env{x},\env{f1},\env{f2}).  The ``degree of asymmetry" in the prediction is also recorded.
This is the ratio between the maximum distance to the removed point to one of its neighbours and the minimum distance from the removed point to one of its neighbours,
see Chapter 5.3 in Sanderson (2010) for more details.
}
\value{
An object of class \code{cnlt} (subclasses \code{biv} and either \code{SG} or \code{DG}).\cr
If both components have the same grid (subclass \code{SG}), a list with components:\cr
\item{x1}{The sampling grid corresponding to \env{f1} used for the decomposition.}
\item{x2}{The sampling grid corresponding to \env{f2} used for the decomposition.  If the object is of subclass \code{SG}, \code{x1} is the same as \code{x2}.}
  \item{det1}{A list, entry \code{i} corresponding to detail coefficients associated to point \code{x_i} and \env{f1}.}
  \item{det2}{A list, entry \code{i} corresponding to detail coefficients associated to point \code{x_i} and \env{f2}.}
\item{lre}{A list, entry \code{i} corresponding to the scales (integrals) when lifting point \code{x_i} and \env{f1}.}
\item{lreA}{A list, entry \code{i} corresponding to the degree of asymmetry of the neighbourhood used in the prediction step of point \code{x_i}, see description above.}

If both components have different sampling grids, the additional following list components are returned:\cr

\item{lre2}{A list, entry \code{i} corresponding to the scales (integrals) when lifting point \code{x_i2} and \env{f2}.}
\item{lreA2}{A list, entry \code{i} corresponding to the degree of asymmetry of the neighbourhood used in the prediction step of point \code{x_i2} with \env{f2}, see description above.}

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
Matt Nunes, Jean Hamilton
}
\section{Warning}{Using a large number of trajectories for long datasets could take a long time!}
\seealso{
\code{\link[CNLTreg]{fwtnppermC}},
\code{link{cnlt.univ}}
}
\examples{

# a bivariate series example with same grids

# simulate data, e.g. two sinusoids
dat <- seq(from=1, to=3, by=0.1)
x1 <- cumsum(sample(dat, 200, TRUE))

y1 <-sin(2*pi*(1/25)*x1) + sin(2*pi*(1/50)*x1)+ 1*sin(2*pi*(1/10)*x1)+ rnorm(length(x1), 0,0.2)
y3 <- c(sin(2*pi*(1/25)*x1[1:100]),sin(2*pi*(1/25)*x1[97:196]))+ rnorm(length(x1), 0,0.1)

\dontrun{
y1y3.dec<-cnlt.biv(x1, f1=y1, f2=y3, P = 500)

# the complex detail coefficients corresponding to the first timepoint are:

y1y3.dec$det1[[1]]

}

# a bivariate series example with different grids

# load some data in

data(Baidu)
data(Google)

\dontrun{
BaiGoo<-cnlt.biv(Baidu$Seconds[1:100], Google$Seconds[1:100], Baidu$Return[1:100], 
Google$Return[1:100], P = 500)

# now look at some of the coefficients from the decomposition
# (the complex detail coefficients corresponding to the first timepoint:

BaiGoo$det1[[1]]
BaiGoo$det2[[1]]
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}% use one of  RShowDoc("KEYWORDS")



