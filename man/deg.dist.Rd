\name{deg.dist}
\alias{deg.dist}
\title{
Function to compute the distance between two points - lat/lon pairs
}
\description{
Compute the distance between two points
}
\usage{
deg.dist(long1, lat1, long2, lat2)
}
\arguments{
  \item{long1}{
Longitude at point of origin
}
  \item{lat1}{
Latitude at point of origin
}
  \item{long2}{
Longitude at destination point
}
  \item{lat2}{
Latitude at destination point
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A distance in kilometers
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
From fossil package
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(PWS)
myDist = deg.dist(-84.3880556, 33.7488889, -83.3779358, 33.960948)
## The function is currently defined as
function (long1, lat1, long2, lat2) 
{
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- long1 * rad
    b1 <- lat2 * rad
    b2 <- long2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 40041.47/(2 * pi)
    d <- R * c
    return(d)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ deg.dist }
