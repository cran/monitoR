\name{templateComment}
\alias{templateComment}
\alias{templateComment<-}

\title{
Query or Set Template Cutoffs
}
\description{
Use this function to add or check comments to templates within template lists (\code{\linkS4class{corTemplateList}} or \code{\linkS4class{binTemplateList}} objects), scores (\code{\linkS4class{templateScores}} objects), or detection list (\code{\linkS4class{detectionList}} objects).
}
\usage{
templateComment(object)
templateComment(object)<-value

}

\arguments{
  \item{object}{A binary or correlation template list (class \code{binTemplateList} or \code{corTemplateList}).}
  \item{value}{A character vector with the new comment.}
}
\details{
\code{templateComment} is an accessor function and \code{templateComment<-} is a replacement function.\cr
For replacement, the \code{value} object should be as long as the number of templates in \code{object} (or the number selecting via indexing) unless it is a named vector (see Examples).
}
\value{
For extraction, a numeric vector of the same length as \code{object} with comments.  
For replacement, the updated object.
}
\author{
  Sasha D. Hafner
}

\seealso{
\code{\link{templateNames}}, 
\code{\link{templateCutoff}},
\code{\link{getTemplates}}
}
\examples{
# Load example Wave objects
data(btnw)
data(oven)

# Write Wave objects to file 
writeWave(btnw,"btnw.wav")
writeWave(oven,"oven.wav")

# Make a set of templateLists
wct1<-makeCorTemplate("btnw.wav",name="w1")
oct1<-makeCorTemplate("oven.wav",name="o1")

wbt1<-makeBinTemplate("btnw.wav",amp.cutoff=-20,name="w1")
obt1<-makeBinTemplate("oven.wav",amp.cutoff=-20,name="o1")

# Combine the pairs
corTemps<-combineCorTemplates(wct1,oct1)
binTemps<-combineBinTemplates(wbt1,obt1)

# Add a comment for each template
templateComment(corTemps)<-c(w1="This is the best template so far.",
o1="This template should be remade without the lowest syllable.")

# Add a default comment to all templates
templateComment(binTemps)<-c(default="These templates have not been tested.")

# View comments
templateComment(corTemps)
templateComment(binTemps)

}

\keyword{manip}
\keyword{attribute}
