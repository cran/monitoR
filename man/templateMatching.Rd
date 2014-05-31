\name{templateMatching}
\alias{corMatch}
\alias{binMatch}

\title{
Calculate Spectrogram Template Matching Scores
}

\description{
These functions are used to calculate spectrogram template matching scores between a set of templates and an acoustic survey using spectrogram cross correlation (\code{corMatch}) or binary point matching (\code{binMatch}).
}

\usage{
corMatch(survey,templates,parallel=FALSE,show.prog=FALSE,cor.method="pearson",
warn=TRUE,time.source="filename",...)

binMatch(survey,templates,parallel=FALSE,show.prog=FALSE,warn=TRUE,
time.source="filename",report.amp=FALSE,...)

}


\arguments{

  \item{survey}{
    A file path to a wav or mp3 recording, or a \code{\link[tuneR]{Wave}} object.
    The \code{survey} is the acoustic survey that you want to make detections within.  
  }

  \item{templates}{
    A template list--a \code{corTemplateList} object for \code{corMatch} or a \code{binTemplateList} object for \code{binMatch}.
  }

%  \item{amp.lwr}{
%    An amplitude below which bins will not be included in the correlation calculation for \code{corMatch}.
%    Length one numeric vector.
%  }

  \item{parallel}{
    If \code{TRUE}, \code{\link[parallel]{mclapply}} is used for calculation of scores across all time bins for each template.
    This option is not available for Windows operating systems.
  }

  \item{show.prog}{
    If \code{TRUE}, progress will be reported during the score calculations.
  }

  \item{cor.method}{
    For \code{corMatch}, the method used to calculate correlation coefficients (see \code{?cor}).
  }

  \item{warn}{
    Set to \code{FALSE} to suppress warnings about time bin duration and frequency bin span mismatches. 
  }

  \item{time.source}{
    The source of date and time information.
    \code{filename} will look in the name of the survey file (\code{survey} argument) for a date and time with format YYYY-MM-DD_HHMMSS_TimeZone.
    \code{"fileinfo"} will take the date and time from the file modification information.  See details.
  }

  \item{report.amp}{
    If \code{TRUE}, \code{binMatch} will return the mean ``on'' and ``off'' amplitudes as well as their difference (the score). See details.
  }

  \item{\dots}{
    Additional arguments to the \code{spectro} function.
  }
}

\details{
Scores are refereced by both the time elapsed since the beginning of the recording and the time of day on the date the recording was made.  For times derived from the date modified of the recording file (\code{time.source="fileinfo"}) to be accurate the sound file must not have been edited (no samples added or removed) since its original creation.  File copying and duplication (as from removeable media to a storage drive) should not affect the date modified, although the creation date will be reset.  Date modified values are stored in the time zone when they were recorded but will be translated to the current time zone when read, which may result in errors of as much as 1 hour when recording and analysis span the beginning or end of daylight savings periods or more if recorded surveys are shared across time zones. Times derived from a date-time value encoded in the file name (\code{time.source="filename"}) are more stable in this regard, and are automatically created with either \code{\link{fileCopyRename}} or \code{\link{mp3Subsamp}}.\cr
Binary point matching scores each time frame by computing the difference between the mean amplitude in the ``on'' cells and the mean amplitude in the ``off'' cells.  The resulting score can be a rough estimate of signal:noise.   
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
  \item{time}{A character vector containing information on the run time.}
  
}

\references{
Mellinger, D. K. and C. W. Clark. 1997. Methods for automatic detection of mysticete sounds. Marine and Freshwater Behaviour and Physiology. 29:163-181.

Towsey, M., B. Planitz, A. Nantes, J. Wimmer, and P. Roe. 2012. A toolbox for animal call recognition. Bioacoustics-the International Journal of Animal Sound and Its Recording. 21:107-125.

}

\author{
  Sasha D. Hafner and Jon Katz
}
 
\note{
Cross-correlation values are not normalized.
}

\note{
For examples, see \code{\link{findPeaks}} and \code{\link{getDetections}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{makeCorTemplate}}, 
\code{\link{makeBinTemplate}}, 
\code{\link{findPeaks}}, 
\code{\link{getDetections}}, 
\code{\link{getPeaks}}, 
\code{\link{fileCopyRename}}, 
\code{\link{mp3Subsamp}}

}

\keyword{manip}

