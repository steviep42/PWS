\name{myWeatherObj-class}
\Rdversion{1.1}
\docType{class}
\alias{myWeatherObj-class}
\alias{getHist,myWeatherObj-method}
\alias{getLocsByRadius,myWeatherObj-method}
\alias{getWobjData,myWeatherObj-method}
\alias{plot,myWeatherObj-method}
\alias{show,myWeatherObj-method}
\alias{summary,myWeatherObj-method}

\title{Class "myWeatherObj"}
\description{
This is a S4 class to accommodate information on Personal Weather Stations existing within
a specified radius of a given city. It serves as a catalogue of such information that
can then be used for subsequent mining.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("myWeatherObj", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} ~~ }
    \item{\code{origin}:}{Object of class \code{"vector"} ~~ }
    \item{\code{radius}:}{Object of class \code{"numeric"} ~~ }
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
    \item{getHist}{\code{signature(object = "myWeatherObj")}: ... }
    \item{getLocsByRadius}{\code{signature(object = "myWeatherObj")}: ... }
    \item{getWobjData}{\code{signature(object = "myWeatherObj")}: ... }
    \item{plot}{\code{signature(x = "myWeatherObj")}: ... }
    \item{show}{\code{signature(object = "myWeatherObj")}: ... }
    \item{summary}{\code{signature(object = "myWeatherObj")}: ... }
	 }
}
\references{
This is part of the work towards the final project in STATS290
}
\author{
Steve Pittard <wsp@emory.edu>
}
\note{
STATS290 website
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
}
\examples{
#showClass("myWeatherObj")
}
\keyword{classes}
