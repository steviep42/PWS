\name{getLatLon}
\alias{getLatLon}
\title{
Given a City name return a lat lon
}
\description{
Given a City name return a lat lon
}
\usage{
getLatLon(locale = "PaloAlto,CA", dist = 10, locLimit = 50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locale}{
A city name or a vector with multiple cities
}
  \item{dist}{
A distance specified in miles
}
  \item{locLimit}{
A limit on the number of locations returned
}
}
\details{
This function isn't normally called by the user.
}
\value{
A data frame with locations within the specifed distance of the given city
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ getLatLon }
