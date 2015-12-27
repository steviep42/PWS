\name{myWeatherHist-class}
\Rdversion{1.1}
\docType{class}
\alias{myWeatherHist-class}
\alias{getHistInfo,myWeatherHist-method}
\alias{plot,myWeatherHist-method}
\alias{show,myWeatherHist-method}
\alias{summary,myWeatherHist-method}

\title{Class "myWeatherHist"}
\description{
This class supports the myWeather class by providing a structure for
storing historic data for a list of stations contained in a companion
weather object.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("myWeatherHist", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} ~~ }
    \item{\code{timeframe}:}{Object of class \code{"vector"} ~~ }
    \item{\code{baseStat}:}{Object of class \code{"vector"} ~~ }
    \item{\code{dataDf}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{names}:}{Object of class \code{"character"} ~~ }
    \item{\code{row.names}:}{Object of class \code{"data.frameRowLabels"} ~~ }
    \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{data.frame}"}, directly.
Class \code{"\linkS4class{list}"}, from data part.
Class \code{"\linkS4class{oldClass}"}, by class "data.frame", distance 2.
Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
}
\section{Methods}{
  \describe{
    \item{getHistInfo}{\code{signature(object = "myWeatherHist")}: ... }
    \item{plot}{\code{signature(x = "myWeatherHist")}: ... }
    \item{show}{\code{signature(object = "myWeatherHist")}: ... }
    \item{summary}{\code{signature(object = "myWeatherHist")}: ... }
	 }
}
\references{
STATS 290 Website
}
\author{
Steve Pittard <wsp@emory.edu>
}
\note{
None
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
#showClass("myWeatherHist")
}
\keyword{classes}
