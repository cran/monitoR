\name{templateNames}
\alias{templateNames}
\alias{templateNames<-}

\title{
  Names of Templates
}

\description{
  Functions to check or change the names of templates within an acoustic template list.
}

\usage{
  templateNames(object)
  templateNames(object)<-value
}

\arguments{
  \item{object}{An acoustic template list, i.e., a \code{\linkS4class{corTemplateList}} or \code{\linkS4class{binTemplateList}} object.}
  \item{value}{A character vector of names. May be named.}
}

\details{
This function is analogous to the function \code{\link[base]{names}}.
}

\value{
For \code{names}, \code{NULL} or a character vector of the same length as \code{object}. For \code{names<-}, the updated template list, i.e., the original template list with only the names changed.
}

\author{
Sasha D. Hafner
}

\seealso{
\code{\link{makeCorTemplate}},
\code{\link{makeBinTemplate}},
\code{\link{templateComment}},
\code{\link{templateCutoff}}
}

\examples{
# Load example Wave objects
data(btnw)
data(oven)

# Write Wave objects to file 
writeWave(btnw,"btnw.wav")
writeWave(oven,"oven.wav")

# Create four correlation templates
wct1<-makeCorTemplate("btnw.wav",name="w1")
wct2<-makeCorTemplate("btnw.wav",t.lim=c(1.5,2.1),frq.lim=c(4.2,5.6),name="w2")
oct1<-makeCorTemplate("oven.wav",t.lim=c(1,4),frq.lim=c(1,11),name="o1")
oct2<-makeCorTemplate("oven.wav",t.lim=c(1,4),frq.lim=c(1,11),dens=0.1,name="o2")

# Combine all of them
ctemps<-combineCorTemplates(wct1,wct2,oct1,oct2)
ctemps

# To check template names
templateNames(ctemps)

# Change the first two
templateNames(ctemps)[1:2]<-c("warbler 1","warbler 2")

# Change all
templateNames(ctemps)<-c("a","b","c","d")



}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
