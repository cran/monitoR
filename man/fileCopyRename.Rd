\name{fileCopyRename}
\alias{fileCopyRename}
\title{
Copy and Rename Sound Files from Portable Media
}
\description{
Collects a variety of metadata about recordings that will be acoustic surveys and encodes the date modified into the file name.  Copies files between directories to move them for an SD card to a hard disk, for example.
}
\usage{
fileCopyRename(files, from = ".", to, csv.dir = to, csv.name, loc.prefix, ext, 
hours.offset = 0, CardRecorderID = NA, kaleidoscope = TRUE, 
split.channels = FALSE, metadata.only = FALSE, full.survey.names = FALSE, 
rename = TRUE, copy = TRUE)
}

\arguments{
  \item{files}{Optional vector of mp3, WAC, or WAV files to extract surveys from.}
  \item{from}{Directory containing mp3, WAC, or WAV recordings to extract survey from; required only if \code{files} is missing.}
  \item{to}{Directory where surveys will be placed after extraction.}
  \item{csv.dir}{Directory where csv file of survey metadata will be saved; defaults to the \code{to} directory.}
  \item{csv.name}{Name to save csv file of metadata, character value ending in .csv}
  \item{loc.prefix}{Character value identifying the location at which the recording was made.  Will be used in the file name (see Details) and the csv file name.  Must be in tblLocation.fldLocationName in the acoustics database.}
  \item{ext}{three-characters.  The file extension defining the type of files to move, rename, and collect metadata on.  Typically in \code{c("wav", "wac")}}
  \item{hours.offset}{Hours to offset the modification time. Useful when transporting recordings across time zones, or if recorders are set in UTC/GMT but the computer is in local time.}
  \item{CardRecorderID}{Numeric key value from tblCardRecorder.pkCardRecorderID, which links the recorder that made the recording with the location it was recorded.}
  \item{kaleidoscope}{Logical. If \code{ext="wac"} files must be converted to .wav in Kaleidoscope.  Setting to \code{TRUE} anticipates the renaming by Kaleidoscope.}
  \item{split.channels}{Logical. If \code{ext="wac"} files must be converted to .wav in Kaleidoscope.  Setting to \code{TRUE} anticipates further renaming by Kaleidoscope.}
  \item{metadata.only}{Logical.  If \code{ext="wac"} files must be converted to .wav before metadata can be collected; this argument typically is used in the second pass to collect the metadata.}
  \item{full.survey.names}{Logical.  \code{TRUE} will use the full file path for the survey name in the resulting metadata table.  In those cases the full path name will be stored in the database as well.  Useful for coping with nested or disparate survey directories.}
  \item{rename}{Logical.  \code{FALSE} will disable renaming.}
  \item{copy}{Logical.  \code{FALSE} will disable file copying.}
}
\details{
The file name is where two important pieces of metadata are encoded: the location (as the location prefix) and the date and time of recording (as the date modified of the original file).  The detection functions \code{\link{corMatch}} \code{\link{binMatch}} are capable of using this data as a time reference.  
Metadata cannot be read for non-wave recordings, so typically a first function call is used to encode the location prefix and date modified into the file name and move it from the portable media, and a second function call with \code{metadata.ony=TRUE} is used after conversion to wave format to fill in the missing metadata.  
The \code{full.survey.names} argument is designed to permit the batch processing of sound files saved in different directories.
}
\value{
A data frame of metadata about the surveys.  Contains column names "fldOriginalDateModified", "fldOriginalRecordingName", "fldSurveyName", "fldRecordingFormat", "fkCardRecorderID", "fldSurveyLength", "fldSampleRate", "fldBitsperSample", and "fldChannels".  Column names reflect the assumption that this data will become a catalog of surveys stored in the database.
}

\author{
Jon Katz
}

\seealso{
\code{\link{mp3Subsamp}}
}
\examples{
\dontrun{
# Not run because it will create a file in user's working directory
data(survey)

writeWave(survey, "survey.wav")

meta <- fileCopyRename(
            files="survey.wav",
            to=getwd(),
            csv.name="sampleMeta.csv",
            loc.prefix="MABI06",
            ext="wav",
            CardRecorderID=1)
            
file.remove("survey.wave")}
}

\keyword{IO}
\keyword{file}
