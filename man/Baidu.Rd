\name{Baidu}
\alias{Baidu}
\docType{data}
\title{
End of second returns for Google from 1st March 2011
}
\description{
Often several trades per second of a stock occur; this dataset consists of the last quoted value for each second for 1st March 2011.
Thus the finest sampling interval is one second, but as there are seconds with no trades, the data have an unequally spaced sampling regime.
}
\usage{data("Baidu")}
\format{
  A data frame with 7984 observations on the following 3 variables.
  \describe{
    \item{\code{Time}}{A variable with the time of the trade.}
    \item{\code{Seconds.index}}{An index representing the time (in seconds) from the start of the data, representing the sampling regime of the series.}
    \item{\code{Return}}{The return price of the stock.}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Hamilton, J., Nunes, M. A, Knight, M. I. and Fryzlewicz, P. (2018) Complex-valued wavelet lifting and applications.
\emph{Technometrics}, \bold{60} (1), 48-60, DOI 10.1080/00401706.2017.1281846.
}
\examples{
data(Baidu)

plot(Baidu$Seconds.index,Baidu$Return,type="l")
}
\keyword{datasets}
