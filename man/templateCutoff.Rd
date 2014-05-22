\name{templateCutoff}
\alias{templateCutoff}
\alias{templateCutoff<-}

\title{
Query or Set Template Cutoffs
}

\description{
Use this function to check or change the values of score cutoff in template lists (\code{\linkS4class{corTemplateList}} or \code{\linkS4class{binTemplateList}} objects), scores (\code{\linkS4class{templateScores}} objects), or detections list (\code{\linkS4class{detectionList}} objects).
}

\usage{
templateCutoff(object)
templateCutoff(object)<-value
}

\arguments{
  \item{object}{
    A binary or correlation template list (class \code{binTemplateList} or \code{corTemplateList}).
  }

  \item{value}{
    A numeric vector with the new score cutoff.
  }
}

\details{
\code{templateCutoff} is an accessor function and \code{templateCutoff<-} is a replacement function.\cr
For replacement, the \code{value} object should be as long as the number of templates in \code{object} (or the number selecting via indexing) unless it is a named vector (see Examples).
}

\value{
For extraction, a numeric vector of the same length as \code{object} with score cutoffs.  
For replacement, the updated object.
}

\author{
  Sasha D. Hafner
}

\seealso{
\code{\link{templateNames}}, \code{\link{templateComment}}
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

# Check cutoffs
templateCutoff(ctemps)

# Change all like this 
templateCutoff(ctemps)<-c(0.35,0.35,0.35,0.35)
# or this
templateCutoff(ctemps)<-c(default=0.35)

# Change select ones like this
templateCutoff(ctemps)<-c(o1=0.45,o2=0.45)
# or this
templateCutoff(ctemps)[c(3,4)]<-0.45

# Could combine these two steps
templateCutoff(ctemps)<-c(default=0.35,o1=0.45,o2=0.45)

}
\keyword{manip}
\keyword{attribute}
