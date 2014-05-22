# A function to upload a MySQL database schema 
# JEK, 21 Nov 2013

dbSchema <- function(
    schema,                                 # File path to schema (.txt or .sql)
    name.on.host,                           # Database name on MySQL host
    tables = FALSE,                          # TRUE will return sqlTables()
    schema.name='NOH',                     # Name of schema to be replaced by name.on.host
    db.name='acoustics',	              # Connection name in ODBC data source
    uid,                                   # Database User ID, if not in ODBC
    pwd,                                 # Database Password, if not in ODBC
    ...                                     # Additional arguments to odbcConnect()
){

    require(RODBC)
    start.time<-Sys.time()
    
    # Read in the schema, break it up appropriately
    schema<-readLines(con=schema)
    schema<-paste(schema,collapse="")
    schema<-gsub(pattern=schema.name,replacement=name.on.host,x=schema)
    schema<-gsub(pattern="----DROP",replacement='----DROPDROP',x=schema)
    schema<-gsub(pattern="COMMENT",replacement=' COMMENT',x=schema)
    schema<-gsub(pattern=";",replacement='; ;;',x=schema)
    schema<-strsplit(schema,'----DROP')
    schema<-unlist(schema)     
    schema<-lapply(schema, function(x) strsplit(x,';;'))
    schema<-unlist(schema)    
    # Open the database connection
    if(missing(uid) && missing(pwd)) {dbCon<-odbcConnect(db.name,...)
    } else if(missing(uid)) {dbCon<-odbcConnect(db.name,pwd,...)
    } else dbCon<-odbcConnect(db.name,uid,pwd,...)
    
    # Establish a cleanup procedure
    on.exit(close(dbCon))
    
    # Push the intro through
    x<-sqlQuery(dbCon,"SET FOREIGN_KEY_CHECKS=0;")
#    x<-sqlQuery(dbCon,intro)
    for(i in 1:length(schema)) {
        x<-sqlQuery(dbCon,schema[i])
        if(all(length(x) != 0, x != "No Data")) warning('Error at:\n',schema[i], '\n', x, '\nVerify tables before continuing.\n')
    }
    # Push the closure through    
    x<-sqlQuery(dbCon,"SET FOREIGN_KEY_CHECKS=1;")
    # Get a list of tables
    if(tables) {
        tables<-sqlTables(dbCon)
        return(list(upload.time=paste("Upload time",round(Sys.time()-start.time,3),units(Sys.time()-start.time)), tables=tables))
    } else return(paste("Upload time",round(Sys.time()-start.time,3),units(Sys.time()-start.time)))
}






