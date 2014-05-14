\name{makeTemplate}
\alias{makeCorTemplate}
\alias{makeBinTemplate}

\title{
Make an Acoustic Template
}

\description{
Functions for creating a spectrogram cross-correlation template or a binary point matching template for later use in identification of acoustic signals.
A template is made by manually or automatically selecting cells within a Fourier-transformed representation (a spectrogram) of an audio recording.
}

\usage{
makeCorTemplate(clip,t.lim=NA,frq.lim=c(0,12),select="auto",dens=1,score.cutoff=0.4,
name="A",comment="",spec.col=gray.3(),sel.col=ifelse(dens==1,"#99009975","orange"),
wl=512,ovlp=0,wn="hanning",...)

makeBinTemplate(clip,t.lim=NA,frq.lim=c(0,12),select="auto",binary=TRUE,buffer=0,
dens=1,score.cutoff=12,name="A",comment="",amp.cutoff="i",shift="i",high.pass=-Inf,
spec.col=gray.3(),bin.col=c("white","black"),
quat.col=c("white","gray40","gray75","black"),sel.col=c("orange","blue"),
legend.bg.col="#2E2E2E94",legend.text.col="black",wl=512,ovlp=0,wn="hanning",...)
}

\arguments{

  \item{clip}{
    A file path to one wav or mp3 files, or a wav object.
    Or, for \code{makeBinTemplate} only, a list or vector of two such objects.
    Character vector or list. 
  }

  \item{t.lim}{
    Time limits of the spectrogram plot or template itself, or a list of exactly two such vectors.
    Length two numeric vector.
  }

  \item{frq.lim}{
    Frequency limits of spectrogram plot or template.
    Length two numeric vector.
  }

  \item{select}{
    How should points be selected? 
    Options are "cell", "rectangle", "auto".
    Length one character vector. 
  }

  \item{binary}{
    Should plot be binary?
    Length one logical vector. 
  }

  \item{buffer}{
    The size of a buffer (in number of time by frequency bins) around ``on'' points for select="rectangle" and select="auto" for \code{makeBinTemplate}.
    Bins within the buffer will not be included as ``on'' or ``off'' points.
    Length one integer vector.
  }

  \item{dens}{
    Approximate density of points included with select="rectangle" and select="auto" as a fraction of 1.0.
    Length one numeric vector.
  }

%  \item{min.reg}{
%    The minimum number of clips with high amplitude within an individual bin for that bin to plotted as high amplitude.
%  }

  \item{score.cutoff}{
    The numeric value set for the \code{score.cutoff} element of the resulting template.
    This value will determine which peaks qualify as detections when the resulting template is used in a complete detection analysis.
    Length one numeric vector.
  }

  \item{name}{
    The name of the template, which will be associated with the template.
    To change the name of an existing template, see \code{templatenames}.
    Length one character vevetor.
  }

  \item{comment}{
    Comment that will be saved with the template.
    See \code{\link{templateComment}}.
  }


  \item{amp.cutoff}{
    Amplitude cutoff for creating a binary plot.
    Length one numeric vector or else \code{"i"} for interactive selection.
  }

  \item{shift}{
    When two clips are used, the forward shift for the second clip, in time bins.
    Length one integer vector, or \code{"i"} for interactive.
  }

  \item{high.pass}{
    High-pass filter value. 
    All amplitudes below this frequency will be set to the minimum.
  }

  \item{spec.col}{
    A color palette function for the spectrogram when \code{binary=FALSE}.
  }

  \item{bin.col}{
    Colors for the spectrogram when \code{binary=TRUE}.
    Length two character vector: \code{bin.col[1]} for cells below the cutoff, \code{bin.col[2]} for cells above the cutoff.
  }

  \item{quat.col}{
    Colors for the spectrogram when using two clips.
    Length four character vector: \code{bin.col[1]} for cells below the cutoff for both clips, \code{bin.col[2]} for cells above the cutoff for clip 1 only, \code{bin.col[3]} for cells above the cutoff for clip 2 only, \code{bin.col[4]} for cells above the cutoff for both clips.
  }

  \item{sel.col}{
    The color for displaying selected cells.
  }

  \item{legend.bg.col}{
    The color of the legend background.
  }

  \item{legend.text.col}{
    Legend text color.
  }

  \item{wl}{
    The \code{wl} argument sent to the \code{spectro} function.
  }

  \item{ovlp}{
    The \code{ovlp} argument sent to the \code{spectro} function.
  }

  \item{wn}{
    The \code{wn} argument sent to the \code{spectro} function.
  }

  \item{\dots}{
    Additional arguments to \code{spectro}.
  }
}

\details{
\code{makeCorTemplate} is used for making correlation templates, while \code{makeBinTemplate} is used to make binary point matching templates.
\code{makeBinTemplate} can be used with one or two recordings (\code{clip} argument).
To use templates produced with these functions, see \code{\link{corMatch}} or \code{\link{binMatch}}.
To combine template lists, see \code{\link{combineCorTemplates}} or \code{\link{combineBinTemplates}}.
}

\value{
An S4 object of class \code{\linkS4class{corTemplateList}} (returned by \code{makeCorTemplate}) or \code{\linkS4class{binTemplateList}} (returned by \code{makeBinTemplate}).
}

\references{
Mellinger, D. K. and C. W. Clark. 1997. Methods for automatic detection of mysticete sounds. Marine and Freshwater Behaviour and Physiology. 29:163-181.

Towsey, M., B. Planitz, A. Nantes, J. Wimmer, and P. Roe. 2012. A toolbox for animal call recognition. Bioacoustics-the International Journal of Animal Sound and Its Recording. 21:107-125.
}

\author{
Sasha D. Hafner and Jon Katz
}

\seealso{
  \code{\link{corMatch}}, 
  \code{\link{binMatch}}, 
  \code{\link{templateNames}}, 
  \code{\link{templateCutoff}} 
}

\examples{
# Load example Wave objects
data(btnw)
data(oven)

# Use a Wave object directly to make a template
wct1<-makeCorTemplate(btnw,name="w1")
wct1

# For traceability, better to use acoustic files
# Here, first write Wave objects to file 
writeWave(btnw,"btnw.wav")
writeWave(oven,"oven.wav")

# Use default arguments except for name
wct1<-makeCorTemplate("btnw.wav",name="w1")

# Specify time and frequency limits to focus on a smaller area
wct2<-makeCorTemplate("btnw.wav",t.lim=c(1.5,2.1),frq.lim=c(4.2,5.6),name="w2")

# For finer control, see options for select argument, e.g.,
\dontrun{
wct3<-makeCorTemplate("btnw.wav",select="cell",name="w3")
wct4<-makeCorTemplate("btnw.wav",select="rectangle",name="w4")
}

# Use a different recording--different species here
oct1<-makeCorTemplate("oven.wav",name="o1",t.lim=c(1,4),frq.lim=c(1,11))

# Reduce cell density
oct2<-makeCorTemplate("oven.wav",name="o2",t.lim=c(1,4),frq.lim=c(1,11),dens=0.1)

# Binary templates are similar
# By default, amplitude cutoff is interactively set
\dontrun{
wbt1<-makeBinTemplate("btnw.wav",name="w1")
}

# Or specify cutoff directly
wbt1<-makeBinTemplate("btnw.wav",amp.cutoff=-40,name="w1")

# Specify time and frequency limits to focus on a smaller area in spectrogram, and add a buffer
\dontrun{
wbt2<-makeBinTemplate("btnw.wav",amp.cutoff=-30,t.lim=c(1.5,2.1),frq.lim=c(4.2,5.6),
buffer=2,name="w2")
}

# For finer control, see options for select argument, e.g.,
\dontrun{
wbt3<-makeBinTemplate("btnw.wav",amp.cutoff=-40,t.lim=c(0.5,2.5),frq.lim=c(1,11),
select="cell",name="w3")
wbt4<-makeBinTemplate("btnw.wav",amp.cutoff=-40,t.lim=c(0.5,2.5),frq.lim=c(1,11),
select="rectangle",buffer=3,name="w4")
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{hgraph}