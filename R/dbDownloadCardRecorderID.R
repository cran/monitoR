# Function to download card and recorder id by location and date from database
# Created 2012 Nov 14
dbDownloadCardRecorderID<-function(
    db.name='acoustics',	              # Connection name in ODBC _and_ on host
    uid,                                   # Database User ID, if not in ODBC
    pwd,                                 # Database Password, if not in ODBC
    date.deployed,                        # First deploy date to download 
    date.collected,                          # Last collection date to download
    loc.prefix,                        # Survey location(s) to download
    ...									# Additional arguments to odbcConnect
    ){
   
    start.time<-Sys.time()
    
    require(RODBC)
    
    if(!missing(loc.prefix)){
        if (nchar(loc.prefix[1])!=6) stop(paste('loc.prefix must be 6 characters, got',loc.prefix))
        loc.prefix=paste("'",loc.prefix,"'",sep="",collapse=(" OR `tblLocation`.`fldLocationNameAbbreviation` =  "))
    }
          
    # open the database connection
    if(missing(uid) && missing(pwd)) {dbCon<-odbcConnect(db.name,...)
    } else if(missing(uid)) {dbCon<-odbcConnect(db.name,pwd,...)
    } else dbCon<-odbcConnect(db.name,uid,pwd,...)
    
    # Establish a cleanup procedure
    on.exit(close(dbCon))

	if(!missing(loc.prefix)) query<-paste("SELECT `tblCardRecorder`.`pkCardRecorderID`, `tblLocation`.`fldLocationNameAbbreviation`, `tblRecorder`.`fldSerialNumber`, `tblCard`.`pkCardID`, `tblCardRecorder`.`fldDateDeployed`, `tblCardRecorder`.`fldDateCollected` FROM `tblCardRecorder` INNER JOIN `tblLocation` ON `tblLocation`.`pkLocationID` = `tblCardRecorder`.`fkLocationID` INNER JOIN `tblRecorder` ON `tblRecorder`.`pkRecorderID` = `tblCardRecorder`.`fkRecorderID` INNER JOIN `tblCard` ON `tblCard`.`pkCardID` = `tblCardRecorder`.`fkCardID` WHERE `tblLocation`.`fldLocationNameAbbreviation` = ",loc.prefix," AND `tblCardRecorder`.`fldDateDeployed` >= '",date.deployed,"' AND`tblCardRecorder`.`fldDateCollected` <= '",date.collected,"'",sep="")
	else query<-paste("SELECT `tblCardRecorder`.`pkCardRecorderID`, `tblLocation`.`fldLocationNameAbbreviation`, `tblRecorder`.`fldSerialNumber`, `tblCard`.`pkCardID`, `tblCardRecorder`.`fldDateDeployed`, `tblCardRecorder`.`fldDateCollected` FROM `tblCardRecorder` INNER JOIN `tblLocation` ON `tblLocation`.`pkLocationID` = `tblCardRecorder`.`fkLocationID` INNER JOIN `tblRecorder` ON `tblRecorder`.`pkRecorderID` = `tblCardRecorder`.`fkRecorderID` INNER JOIN `tblCard` ON `tblCard`.`pkCardID` = `tblCardRecorder`.`fkCardID` WHERE `tblCardRecorder`.`fldDateDeployed` >= '",date.deployed,"' AND `tblCardRecorder`.`fldDateCollected` <= '",date.collected,"'",sep="")

    # Download the query 
    cardrecorder<-sqlQuery(dbCon,query)
    
    message(if(class(cardrecorder)=='data.frame') {paste('Done! Download time:',round(Sys.time()-start.time,2),'seconds')
            } else paste("Download unsuccessful; RODBC returned errors: ",paste(cardrecorder, collapse=" ")))
    
    return(cardrecorder)
}    

