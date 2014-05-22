\name{findPeaks}
\alias{findPeaks}

\title{
  Finding Score Peaks and Detections in a \code{templateScores} Object
  }

\description{
  This function accepts \code{templateScores} objects and returns information on all score peaks and those peaks that are considered detections.
  }

\usage{
findPeaks(score.obj, fd.rat = 1, frame, parallel = FALSE)
  }

\arguments{
  \item{score.obj}{
    A \code{templateScores} object, produced by \code{corMatch} or \code{binMatch}.
  }

  \item{fd.rat}{
    A ratio of frame width (twice minimum peak separation) to template duration. 
  }

  \item{frame}{
    If you want the same frame width for templates with varying duration, specify a value directly.
    \code{fd.rate} will be ignored if \code{frame} is specified.
  }

  \item{parallel}{
    Set to \code{TRUE} for parallel processing using \code{mclapply()} from the \code{parallel} package.
    This option is not available for Windows operating systems.
  }
}

\details{
The \code{findPeaks} function translates raw scores from template matching to detection information, by finding peaks in the score data, and determining which peaks, if any, exceed the score cutoffs specified in the templates (see the two functions for making templates, \code{\link{makeBinTemplate}} and \code{\link{makeCorTemplate}} and \code{\link{templateCutoff}} for more details on cutoffs). 
}

\value{
An S4 object of class \code{templateScores}, with the following slots:

  \item{survey.name}{The file path to the survey that the scores apply to.}
  \item{survey}{The actual survey as a \code{Wave} object.}
  \item{survey.data}{A named list with one element per template.
    Each element is a named list with time-domain results for the survey.
  }
  \item{templates}{The templates (an S4 object of class \code{corTemplateList} or \code{binTemplateList}) used to calculate the scores.}
  \item{scores}{A named list with an element for each template.
    Each element contains the scores for an individual template.
  }
  \item{peaks}{A named list with peak information (as a data frame) for each template.}
  \item{detections}{A named list with detection information (as a data frame) for each template.}
}

\author{
  Sasha D. Hafner and Jon Katz
}

\seealso{
\code{\link{makeCorTemplate}},
\code{\link{makeBinTemplate}},
\code{\link{corMatch}},
\code{\link{binMatch}},
\code{\link{getDetections}},
\code{\link{getPeaks}}
}

\examples{
# First create files to work with 
data(btnw)
data(oven)
data(survey)
writeWave(btnw,"btnw.wav")
writeWave(oven,"oven.wav")
writeWave(survey,"survey2010-12-31_120000_EST.wav")

# Correlation example
# Create two correlation templates
wct<-makeCorTemplate("btnw.wav",t.lim=c(1.5,2.1),frq.lim=c(4.2,5.6),name="w")
oct<-makeCorTemplate("oven.wav",t.lim=c(1,4),frq.lim=c(1,11),dens=0.1,name="o")

# Combine all of them
ctemps<-combineCorTemplates(wct,oct)

# Calculate scores
cscores<-corMatch("survey2010-12-31_120000_EST.wav",ctemps)

# Finally, find peaks and detections
cdetects<-findPeaks(cscores)
cdetects
plot(cdetects)

\dontrun{
# Binary example
# Create two templates
wbt<-makeBinTemplate("btnw.wav",amp.cutoff=-30,t.lim=c(1.5,2.1),frq.lim=c(4.2,5.6),
buffer=2,name="w")
obt<-makeBinTemplate("oven.wav",amp.cutoff=-20,t.lim=c(1,4),frq.lim=c(1,11),name="o")

# Combine all of them
btemps<-combineBinTemplates(wbt,obt)

# Calculate scores
bscores<-binMatch("survey2010-12-31_120000_EST.wav",btemps)

# Finally, find peaks and detections
bdetects<-findPeaks(bscores)
bdetects
plot(bdetects)
}

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

