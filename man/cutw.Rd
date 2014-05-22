\name{cutw}
\alias{cutw}

\title{
Extract Shorter Wave Objects from other Wave Objects
}
\description{
Extract shorter Wave objects from other Wave objects.  Extracted wave object will be between the \code{from} and \code{to} boundaries.
}
\usage{
cutw(wave, from = NULL, to = NULL)
}

\arguments{
  \item{wave}{Object of class \code{\link[tuneR]{Wave}}.}
  \item{from}{Start extracted segment from this point, in seconds from beginning of \code{Wave} object.}
  \item{to}{End of extracted segment, in seconds from beginning of \code{Wave} object.}
}

\details{
This function is a simplified version of \code{\link[seewave]{cutw}}
}

\value{
An object of class \code{Wave}.
}

\author{
Sasha D. Hafner
}


\examples{
data(survey)
event1 <- cutw(wave = survey, from = 1.5, to = 4.75)
}

\keyword{manip}
\keyword{utilities}
