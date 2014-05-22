\name{dbUploadResult}
\alias{dbUploadResult}
\title{Upload Detection Results to an Acoustics Database}

\description{
Upload detection results (peaks or detections) from \code{\link{findPeaks}} directly to tblResult in an acoustics database.
}

\usage{
dbUploadResult(detection.obj, which.one, what = "detections", 
    db.name = "acoustics", uid, pwd, analysis.type, analyst = "", ...)
}

\arguments{
  \item{detection.obj}{Object of class \code{\linkS4class{detectionList}} containing results from \code{\link{findPeaks}}.}
  \item{which.one}{Results from a single template can be selected for upload, or leave blank to upload results from all templates.}
  \item{what}{Character value of either "detections" (the default; peaks above the score cutoff) or "peaks" (all peaks regardless of score cutoff).}
  \item{db.name}{Name of the ODBC connector data source corresponding to the acoustics database.}
  \item{uid}{User ID to allow ODBC connector to connect to database, if not present in ODBC connector.}
  \item{pwd}{Password to allow ODBC connector to connect to database, if not present in ODBC connector.}
  \item{analysis.type}{Character value identifying analysis type, in c("BIN","COR").  Some partial matching is performed.}
  \item{analyst}{Numerical key value corresponding to the user's tblPerson.pkPersonID value in the acoustics database.}
  \item{\dots}{Additional arguments to \code{RODBC::odbcConnect}.}
}

\details{
\code{dbUploadResult} assumes a database structure identical to that provided in the acoustics schema.  If the username and password are present in the ODBC datasource they do not need to be provided.  It is possible to store only the username in the datasource and enter a password, but the reverse will not work.
  
The value for analyst must be present in tblPeople.pkPeopleID for upload to succeed.
}

\value{
Invoked for its side effect, which is to insert the detection results into tblResult in an acoustics database.  Successful upload is marked by a report of the upload time; unsuccessful upload will report any errors encountered.
}

\author{
Jon Katz
}

\seealso{
\code{\link{findPeaks}}, \code{\link{getPeaks}}, \code{\link{getDetections}}
}

\examples{
data(btnw)
writeWave(btnw,"btnw.wav")
data(survey)
writeWave(survey,"survey.wav")
# Template construction
b4<-makeBinTemplate(
    "btnw.wav",
    frq.lim=c(2,8),
    select="auto",
    name="b4",
    buffer=4,
    amp.cutoff=-31,
    binary=TRUE)
# Binary point matching
scores<-binMatch(survey="survey.wav",templates=b4,time.source='fileinfo')
# Isolate peaks
pks<-findPeaks(scores)

\dontrun{
#If using the 'acoustics' schema verbatim:
dbUploadResult(detection.obj = pks, analysis.type = "BIN", analyst = 1)

#'acoustics' schema, different database name:
dbUploadResult(
    detection.obj = pks, 
    which.one = "b4", 
    what = "peaks", 
    db.name = "LocalSQLdb", 
    uid = "EntryOnly" , 
    pwd = "07H23BBM", 
    analysis.type = "BIN", 
    analyst = 1)}
}

\keyword{database}
\keyword{utilities}
