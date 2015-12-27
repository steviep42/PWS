\name{getPlaces}
\alias{getPlaces}
\title{
Support function for getLatLon
}
\description{
Does the actual work of finding locations within a given distance of a lat/lon
}
\usage{
getPlaces(lat, lon, distance, locLimit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
A latitude
}
  \item{lon}{
A longitude 
}
  \item{distance}{
A distance in miles
}
  \item{locLimit}{
A limit on the number of locations
}
}
\details{
Note that this function is not meant to be called by the user
}
\value{
A list of locations
}
\references{
STATS 290 Website
}
\author{
Steve Pittard
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
function (lat, lon, distance, locLimit) 
{
    library(RCurl)
    library(RMySQL)
    library(XML)
    print("Querying the local Geonames database to see whats nearby")
    con = dbConnect(MySQL(), user = "wpittard", password = "t41tt", 
        dbname = "wpittard")
    str1 = "SELECT geo_latitude, geo_longitude, geo_name, "
    str2 = paste("(3959*acos(cos(radians(", lat, ")) * cos(radians(geo_latitude)) * cos(radians(geo_longitude) - radians(", 
        lon, "))", sep = "")
    str3 = paste("+ sin(radians(", lat, ")) * sin( radians( geo_latitude)))) AS distance FROM allcountries where ((geo_feature_code like '\%ADM\%')", 
        sep = "")
    str4 = paste("or (geo_feature_code like '\%PPL\%') )  having distance < ", 
        distance, ";", sep = "")
    str5 = paste(str1, str2, str3, str4)
    rs = dbSendQuery(con, statement = str5)
    data = fetch(rs, n = -1)
    dbDisconnect(con)
    if (nrow(data) >= locLimit) {
        newdata = data[sample(nrow(data), size = locLimit), ]
    }
    else {
        newdata = data
    }
    return(newdata)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~GetPlaces }
