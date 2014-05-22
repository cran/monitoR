\name{showPeaks}
\alias{showPeaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
View or Verify Detections or Peaks
}
\description{
Use this function to view a spectrogram and score plot of detections or peaks.
In its simplest usage, \code{showPeaks} will show all detections within for the first template within the detection list object, one after the other.
With the verify option (\code{verify=TRUE}), the user can tag detections or peaks as \code{TRUE} or \code{FALSE}, and these results will be saved in an updated detection list object.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
showPeaks(detection.obj, which.one = names(detection.obj@templates)[1], fd.rat = 2, 
frame = fd.rat * detection.obj@templates[[which.one]]@duration, id = 1:nrow(pks), 
t.lim, flim = c(0, 20), point = TRUE, ask = if (verify) FALSE else TRUE, 
scorelim = NULL, verify = FALSE, what = "detections", box = TRUE, 
player = "play", spec.col = gray.3())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{detection.obj}{
    A detection list object (\code{\linkS4class{detectionList}}).
}
  \item{which.one}{
    Which template should be shown? Identify by name or position. Length-one integer or character vector.
}
  \item{fd.rat}{
    Ratio of plot frame (x limits of plots) to template duration.
}
  \item{frame}{
    Or, specify the plot frame (x limits of plots) instead of \code{fr.rate}. Length-one numeric vector.
}
  \item{id}{
    Use to specify which peaks or detections will be shown. Integer vector.
}
  \item{t.lim}{
    Or, to view only those detections or peaks within a certain time range, specify it here. Length-two numeric vector.
}
  \item{flim}{
    Frequency limits (y axis limits) for the spectrogram. Length-two numeric vector.
}
  \item{point}{
    If \code{TRUE}, plot points to show detection or peak locations.
}
  \item{ask}{
    The setting of the \code{\link{par}} setting \code{ask}. Default value is based on other arguments, and should suffice in most cases.
}
  \item{scorelim}{
    Score limits (y axis limits) for the score plot.
}
  \item{verify}{
    If \code{TRUE}, R will prompt user to identify whether detections are \code{TRUE}
}
  \item{what}{
    Should all peaks (\code{what="peaks"}) or just detections (\code{what="detections"}) be shown?
}
  \item{box}{
    If \code{TRUE} plot a box around detections in the spectrogram. Box boundaries are based on template duration and frequency limits.
}
  \item{player}{
    If \code{verify=TRUE}, the user will have the option to play the detection or peak. This argument is the command used for starting the player. See Details.
}
  \item{spec.col}{
    A vector of colors for the spectrogram.
}
}
\details{
Note that almost all of the arguments have a default value.\cr
The default audio player, "wv_player.exe", is a free third-party multi-OS media player available for download from CNET: http://download.cnet.com/WV-Player-formerly-Wav-Player/3000-2139_4-10472299.html\cr
Sox is another multi-OS media player invoked with ("play").  An option that may already be installed on Windows is Windows Media Player ("wmplayer.exe"); on Ubuntu try Rhythmbox ("rhythmbox"), and on Mac OS try afplay ("afplay").    
}
\value{
\code{NULL}, invisibly, or, if \code{verify=TRUE}, an updated detection list object (\code{\linkS4class{detectionList}}).
}
\author{
Sasha D. Hafner
}

\seealso{
  \code{\link{findPeaks}},
  \code{\link{plot-methods}}
}
\examples{
# Load example Wave object: template and survey
data(btnw)
data(survey)

# Use a Wave object directly to make a binary point template
wbt1<-makeBinTemplate("btnw.wav",frq.lim=c(3.5,6.5),name="w1",amp.cutoff=-25)

# Calculate scores
bscores<-binMatch(survey=survey,templates=wbt1,time.source='fileinfo')

# Find peaks and detections
bdetects<-findPeaks(bscores)

\dontrun{
# Interactively inspect individual detections, no return value
showPeaks(detection.obj=bdetects, which.one="w1", flim = c(2,8), point=TRUE, scorelim=c(0,40))

# Interactively verify individual detections, return adds verification field
bdetects<-showPeaks(detection.obj=bdetects, which.one="w1", flim = c(0, 20), 
point = TRUE, scorelim = c(0,40), verify = TRUE)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dynamic}
\keyword{iplot}
