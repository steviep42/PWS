\name{getWeatherData}
\alias{getWeatherData}
\title{
A user function for getting a list of weather reports for some weather stations
}
\description{
A user provides a city name and a radius. This function then goes and finds
weather stations within the specified radius.
}
\usage{
getWeatherData(locale = "Montreal,CA", distance = 20, statLimit = 150)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locale}{
A city/state/country name
}
  \item{distance}{
A distance/radius in miles
}
  \item{statLimit}{
A limit on the number of stations to be returned
}
}
\details{
None
}
\value{
An S4 object containing weather information for each personal weather station found
}
\references{
STATS290 Website
}
\author{
Steve Pittard <wsp@emory.edu>
}
\note{
None
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
None
}
\examples{
## The function is currently defined as
function (locale = "Montreal,CA", distance = 20, statLimit = 150) 
{
    .onLoad = function(libname, pkgname) {
        .libPaths("~wpittard/Library")
        require("svMisc")
    }
    myLocs = getLatLon(locale, distance)
    myStats = getStations(myLocs$locs, myLocs$iradius, statLimit)
    if (length(myStats) == 0) {
        stop("Sorry. No stations found within the specified radius")
    }
    baseStat = myStats[1]
    myWeatherObj = getWeather(myStats, myLocs$mlat, myLocs$mlon, 
        myLocs$iradius, myLocs$origCity, baseStat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ GetWeatherData }
