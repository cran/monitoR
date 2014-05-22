\name{show-methods}
\docType{methods}
\alias{show-methods}
\alias{show,binTemplateList-method}
\alias{show,corTemplateList-method}
\alias{show,detectionList-method}
\alias{show,templateScores-method}
\alias{summary-methods}
\alias{summary,binTemplateList-method}
\alias{summary,corTemplateList-method}
\alias{summary,TemplateList-method}
\alias{summary,detectionList-method}
\alias{summary,templateScores-method}

\title{Methods for the \code{show} and \code{summary} Functions}

\description{
These methods are used for viewing template lists and other objects.
For all types of objects documented here, \code{show} and \code{summary} will produce identical results.
}

\section{Methods}{
\describe{

\item{\code{signature(object = "binTemplateList")}}{
Displays a summary of \code{\linkS4class{binTemplateList}} objects.
}

\item{\code{signature(object = "corTemplateList")}}{
Displays a summary of \code{\linkS4class{corTemplateList}} objects.
}

\item{\code{signature(object = "TemplateList")}}{
Displays a summary of \code{\linkS4class{TemplateList}} objects.
}

\item{\code{signature(object = "detectionList")}}{
Displays a summary of \code{\linkS4class{detectionList}} objects.
}

\item{\code{signature(object = "templateScores")}}{
Displays a summary of \code{\linkS4class{templateScores}} objects.
}
}}

\author{
Sasha D. Hafner
}

\seealso{
\code{\link{makeCorTemplate}},
\code{\link{makeBinTemplate}}
}

\examples{
# First create a template list
data(btnw)
writeWave(btnw,"btnw.wav")
ctemp1<-makeCorTemplate("btnw.wav",name="w1")

# Then for a quick summary:
ctemp1

# Or, using summary (although there is no reason to do so:)
summary(ctemp1)
}

\keyword{methods}
\keyword{print}
