\name{getWeather}
\alias{getWeather}
\title{
A function for getting weather data for a list of weather stations 
}
\description{
A user function for getting weather data for a list of weather stations 
}
\usage{
getWeather(slist, lat, lon, origRadius, origCity, baseStation)
}
\arguments{
  \item{slist}{
A list of station names
}
  \item{lat}{
A latitude
}
  \item{lon}{
A longitude
}
  \item{origRadius}{
A radius in miles
}
  \item{origCity}{
Initial city name
}
  \item{baseStation}{
First PWS station name in list
}
}
\details{
Not intended for direct use by users
}
\value{
A weather object containing weather information for a list of PWS
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ getWeather }
