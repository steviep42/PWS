\name{getStations}
\alias{getStations}
\title{
A function to get a list of Personal Weather Station names
}
\description{
This function looks on Wunderground to see if there are any PWS at a
given lat/lon.
}
\usage{
getStations(lldf, radius = 10, numofStatLimit = 150)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lldf}{
A data frame of locations specified in lat/lon
}
  \item{radius}{
A distance / radius specified in miles
}
  \item{numofStatLimit}{
A cap on the number of stations
}
}
\details{
This function is not meant to be called directly by users
}
\value{
A list of PWS stations
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
function (lldf, radius = 10, numofStatLimit = 150) 
{
    library(XML)
    library(RCurl)
    library(svMisc)
    dflength = length(lldf)
    if (dflength == 0) {
        stop("Hey. You passed an empty data frame. Unable to generate a list of stations !")
    }
    rlist = list()
    wunderString = "http://api.wunderground.com/auto/wui/geo/GeoLookupXML/index.xml?query="
    nrowdf = nrow(lldf)
    pStr = paste("Now checking Wunderground to find stations")
    print(pStr)
    for (ii in 1:nrowdf) {
        progress(ii, nrowdf - 1)
        lat = lldf[ii, c("geo_latitude")]
        lon = lldf[ii, c("geo_longitude")]
        coords = paste(lat, lon, sep = ",")
        urlstr = paste(wunderString, coords, sep = "")
        if (ii == (nrowdf - 1)) 
            cat("Done!\n")
        if (radius < 10) {
            halfradius = 3
        }
        else {
            halfradius = radius/6
        }
        qstring = paste("//location/nearby_weather_stations/pws/station[distance_mi<", 
            halfradius, "]/ancestor-or-self::station/id", sep = "")
        txt2parse = getURL(urlstr)
        hold = xmlTreeParse(txt2parse, useInternalNodes = TRUE)
        stationsXml = getNodeSet(hold, qstring)
        stations = sapply(stationsXml, xmlValue)
        rlist[[ii]] = stations
    }
    finalStationList = unique(unlist(rlist))
    if (length(finalStationList) == 0) {
        stop("No stations found")
    }
    else {
        if (length(finalStationList) > numofStatLimit) {
            length(finalStationList) = numofStatLimit
        }
        pStr = paste("Found", length(finalStationList), "Personal Weather Stations within", 
            radius, "miles", sep = " ")
        print(pStr)
        return(finalStationList)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ getPlaces }
