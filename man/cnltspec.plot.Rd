\name{cnltspec.plot}
\alias{cnltspec.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to plot CNLT spectral quantities of interest
}
\description{
The function takes a spectral quantity and plots it according to user inputted graphical options
}
\usage{
cnltspec.plot(spec, timevec, scalevec, zrange = NULL, xtitle = "Time", ytitle = "Scale", 
col.scale = tim.colors(64)[1:45], SFratio = 2, dt = 1, parsw = 3, axis4 = FALSE, 
frequencies = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{A spectral quantity, i.e. contained in a \code{cnlt.spec} object.  For example, this could be the coherence between components of a bivariate series.}
  \item{timevec}{A vector corresponding to the x-axis of the spectral object, often time or a sampling grid.}
  \item{scalevec}{A vector of scales corresponding to the y-axis of the spectral object.}
  \item{zrange}{An optional length two vector specifying the range of the z-axis of the plot.}
  \item{xtitle}{A label for the x-axis of the plot.}
  \item{ytitle}{A label for the y-axis of the plot.}
  \item{col.scale}{a color palette to use for the spectral plot.}
  \item{SFratio}{A number specifying the relationship between scale and Fourier frequency, see \env{frequencies} argument, and Sanderson (2010), chapter 6.2.}
  \item{dt}{A sampling interval, used to compute the relationship between scale and Fourier frequency, see Sanderson (2010), chapter 6.2.}
  \item{parsw}{A number from 1 to 3, specifying different spacings between the plot and the legend.  This is useful if you want to do call \code{cnltspec.plot} multiple times for e.g. multi-panel plots.}
  \item{axis4}{An optional boolean variable indicating whether a 4th (right) axis should be added to the plot.}
  \item{frequencies}{If \code{axis4 = TRUE}, an optional vector for the ticks on the 4th axis.  If these are not specified, then a vector of Fourier frequencies are plotted, with the relationship between scale and frequency specified by \env{SFratio}, see Sanderson (2010), chapter 6.2.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A spectral quantity is plotted.
}
\references{
Hamilton, J., Nunes, M. A., Knight, M. I. and Fryzlewicz, P. (2017) Complex-valued wavelet lifting and applications.
\emph{Technometrics (to appear)} DOI 10.1080/00401706.2017.1281846.\cr\cr
Sanderson, J. (2010) Wavelet methods for time series with bivariate observations and irregular sampling grids. PhD Thesis, University of Bristol, UK.
}
\author{
Jean Hamilton, Matt Nunes
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{cnlt.spec}}
}
\examples{

# simulate data, e.g. two sinusoids
dat <- seq(from=1, to=3, by=0.1)
x1 <- cumsum(sample(dat, 200, TRUE)) 

y1 <-sin(2*pi*(1/25)*x1) + sin(2*pi*(1/50)*x1)+ 1*sin(2*pi*(1/10)*x1)+ rnorm(length(x1), 0,0.2)
y3 <- c(sin(2*pi*(1/25)*x1[1:100]),sin(2*pi*(1/25)*x1[97:196]))+ rnorm(length(x1), 0,0.1)

\dontrun{

y1y3.dec<-cnlt.biv(x1, f1=y1, f2=y3, P = 500)

y1y3spec<-cnlt.spec(y1y3.dec)

cnltspec.plot(y1y3spec$coh,y1y3spec$mtime,y1y3spec$mscale)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}% use one of  RShowDoc("KEYWORDS")
