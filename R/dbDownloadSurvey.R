# Function to download survey metadata from a database
# Created 2012 Nov 07
dbDownloadSurvey<-function(
    db.name='acoustics',	             # Connection name in ODBC _and_ on host
    uid,                                # Database User ID, if not in ODBC
    pwd,                                # Database Password, if not in ODBC
    start.date,                        # First date to download survey paths 
    end.date,                          # Last date to download survey paths
    loc.prefix,                         # Survey location(s) for which to download survey paths, missing==all
    samp.rate=c(24000,44100),          # Specify one or a vector of sampling rate(s)
    ext=c('wav','mp3'),                # Specify one or a vector of extension(s)
    ...									# Additional arguments to odbcConnect
    ){
   
    start.time<-Sys.time()
    
    require(RODBC)
   
    if(!missing(loc.prefix)){
        if (nchar(loc.prefix)!=6) {stop(paste('loc.prefix must be 6 characters, got',loc.prefix))}
    }
      
    # open the database connection
    if(missing(uid) && missing(pwd)) {dbCon<-odbcConnect(db.name,...)
    } else if(missing(uid)) {dbCon<-odbcConnect(db.name,pwd,...)
    } else dbCon<-odbcConnect(db.name,uid,pwd,...)
    
    # Establish a cleanup procedure
    on.exit(close(dbCon))
    
    # Replace samp.rate with a portion of a query for samp.rate
    samp.rate<-paste(samp.rate,collapse="' OR `fldSampleRate` = '")
    # Replace ext with a portion of a query for ext
    ext<-paste(ext,collapse="' OR `fldRecordingFormat` = '")  
    # Choose whether to download all surveys from all sites or just the specified one(s)
    if(missing(loc.prefix)) {    
         query<-paste("SELECT `fldSurveyName` FROM `tblSurvey` WHERE `fldOriginalDateModified` >= '",start.date,"' AND `fldOriginalDateModified` <= '",end.date,"' AND `fldSampleRate` = '",samp.rate,"' AND `fldRecordingFormat` = '",ext,"'",sep="")
    } else {
         query<-paste("SELECT `tblSurvey`.`fldSurveyName` FROM `tblSurvey` INNER JOIN (`tblLocation` INNER JOIN `tblCardRecorder` ON `tblLocation`.`pkLocationID` = `tblCardRecorder`.`pkCardRecorderID`) ON `tblCardRecorder`.`pkCardRecorderID` = `tblSurvey`.`fkCardRecorderID` WHERE `tblSurvey`.`fldOriginalDateModified` >= '",start.date,"' AND `tblSurvey`.`fldOriginalDateModified` <= '",end.date,"' AND `fldSampleRate` = '",samp.rate,"' AND `fldRecordingFormat` = '",ext,"' AND `tblLocation`.`fldLocationNameAbbreviation` = '",paste(loc.prefix,sep="",collapse="' OR `tblLocation`.`fldLocationNameAbbreviation` = '"),"' ",sep="")}
                
    # Download the names of the surveys 
    surveys<-sqlQuery(dbCon,query,stringsAsFactors=FALSE)
    
    message(if(class(surveys)=='data.frame') {paste('Done! Download time:',round(Sys.time()-start.time,2),'seconds')
            } else paste("Download unsuccessful; RODBC returned errors: ",paste(surveys, collapse=" ")))
    
    return(surveys)
}    



