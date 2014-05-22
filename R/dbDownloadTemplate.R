# Function to download templates from a database
# Created 2012 Nov 07
dbDownloadTemplate<-function(
    db.name='acoustics',	            # Connection name in ODBC _and_ on host
    uid,                                # Database User ID, if not in ODBC
    pwd,                                # Database Password, if not in ODBC
    by.cat,                             # Leave blank for all templates or choose 'names' or 'species'
    type,                               # 'BIN', 'binary', or 'bt', also 'COR', 'correlation' or 'ct'
    template.group,                     # Vector of template names or sp. codes to download
    FFTwl,                              # FFT window length for templates
    FFTovlp,                            # FFT overlap
    FFTwn,                              # FFt window name
    ...									# Additional arguments to odbcConnect
    ){
    
    start.time<-Sys.time()
    
    require(RODBC)
      
    # open the database connection
    if(missing(uid) && missing(pwd)) {dbCon<-odbcConnect(db.name,...)
    } else if(missing(uid)) {dbCon<-odbcConnect(db.name,pwd,...)
    } else dbCon<-odbcConnect(db.name,uid,pwd,...)
   
    # Establish a cleanup procedure
    on.exit(close(dbCon))
    # Standarize template type  
    std.type<-if(tolower(type) %in% c('bin','binary','bt','b')) {'BIN'
         } else if (tolower(type) %in% c('cor','correlation','ct','c')) {'COR'
         } else stop(paste('type expects BIN, bt, COR, or ct. Did not recognize:',type))
    # Check for template.group
    if(missing(by.cat) && !missing(template.group)) stop("Specify whether 'template.group' is 'names' or 'species'.\n")               
    # Choose whether to download all templates or just the specified templates
    if(missing(by.cat)) {
    	query<-paste("SELECT `fldTemplateName`,`fldClipPath`, `fldSampRate`, `fldPtOnT`, `fldPtOnFrq`, `fldPtOffT`, `fldPtOffFrq`, `fldPtsT`, `fldPtsFrq`, `fldPtsAmp`, `fldTStep`, `fldFrqStep`, `fldNTBins`, `fldFirstTBin`, `fldNFrqBins`, `fldDuration`, `fldFrqLim`, `fldFFTwl`, `fldFFTovlp`, `fldFFTwn`, `fldScoreCutoff`, `fldComment` FROM `tblTemplate` WHERE `fldTemplateType` = '",std.type,"' ",if(!missing(FFTwl)) paste0("AND `tblTemplate`.`fldFFTwl` = '",FFTwl,"' "),if(!missing(FFTovlp)) paste0("AND `tblTemplate`.`fldFFTovlp` = '",FFTovlp,"' "),if(!missing(FFTwn)) paste0("AND `tblTemplate`.`fldFFTwn` = '",FFTwn,"' "),"AND `tblTemplate`.`fldActive` = '1'",sep="")
    } else if (by.cat %in% c('names','name','n')) {
        query<-paste("SELECT `fldTemplateName`,`fldClipPath`, `fldSampRate`, `fldPtOnT`, `fldPtOnFrq`, `fldPtOffT`, `fldPtOffFrq`, `fldPtsT`, `fldPtsFrq`, `fldPtsAmp`, `fldTStep`, `fldFrqStep`, `fldNTBins`, `fldFirstTBin`, `fldNFrqBins`, `fldDuration`, `fldFrqLim`, `fldFFTwl`, `fldFFTovlp`, `fldFFTwn`, `fldScoreCutoff`, `fldComment` FROM `tblTemplate` WHERE `fldTemplateName` = '", paste(template.group,"'", collapse=" OR `fldTemplateName` = '",sep="")," AND `fldTemplateType` = '",std.type,"' ",if(!missing(FFTwl)) paste0("AND `tblTemplate`.`fldFFTwl` = '",FFTwl,"' "),if(!missing(FFTovlp)) paste0("AND `tblTemplate`.`fldFFTovlp` = '",FFTovlp,"' "),if(!missing(FFTwn)) paste0("AND `tblTemplate`.`fldFFTwn` = '",FFTwn,"' "),"AND `tblTemplate`.`fldActive` = '1'",sep="")
    } else if (by.cat %in% c('species','s','sp')) {
        query<-paste("SELECT `tblTemplate`.`fldTemplateName`,`tblTemplate`.`fldClipPath`, `tblTemplate`.`fldSampRate`, `tblTemplate`.`fldPtOnT`, `tblTemplate`.`fldPtOnFrq`, `fldPtOffT`, `tblTemplate`.`fldPtOffFrq`, `tblTemplate`.`fldPtsT`, `tblTemplate`.`fldPtsFrq`, `tblTemplate`.`fldPtsAmp`, `tblTemplate`.`fldTStep`, `tblTemplate`.`fldFrqStep`, `tblTemplate`.`fldNTBins`, `tblTemplate`.`fldFirstTBin`, `tblTemplate`.`fldNFrqBins`, `tblTemplate`.`fldDuration`, `tblTemplate`.`fldFrqLim`, `tblTemplate`.`fldFFTwl`, `tblTemplate`.`fldFFTovlp`, `tblTemplate`.`fldFFTwn`, `tblTemplate`.`fldScoreCutoff`, `fldComment` FROM `tblTemplate` INNER JOIN `tblSpecies` ON `tblTemplate`.`fkSpeciesID` = `tblSpecies`.`pkSpeciesID` WHERE `tblSpecies`.`fldSpeciesCode` = '", paste(template.group,"'", collapse=" OR `tblSpecies`.`fldSpeciesCode` = '",sep="")," AND `tblTemplate`.`fldTemplateType` = '",std.type,"' ",if(!missing(FFTwl)) paste0("AND `tblTemplate`.`fldFFTwl` = '",FFTwl,"' "),if(!missing(FFTovlp)) paste0("AND `tblTemplate`.`fldFFTovlp` = '",FFTovlp,"' "),if(!missing(FFTwn)) paste0("AND `tblTemplate`.`fldFFTwn` = '",FFTwn,"' "),"AND `tblTemplate`.`fldActive` = '1'",sep="")
    }
            
    # Download the name and data fields for the templates 
    tblTemplate<-sqlQuery(dbCon,query)
    # Assemble pt matrices
    if(std.type=='BIN') {
        pt.on<-lapply(1:length(tblTemplate$fldTemplateName), function(x) cbind(t=eval(parse(text=as.character(tblTemplate$fldPtOnT[x]))),frq=eval(parse(text=as.character(tblTemplate$fldPtOnFrq[x])))))
        pt.off=lapply(1:length(tblTemplate$fldTemplateName), function(x) cbind(t=eval(parse(text=as.character(tblTemplate$fldPtOffT[x]))),frq=eval(parse(text=as.character(tblTemplate$fldPtOffFrq[x])))))
    } else if(std.type=='COR') {
        pts<-lapply(1:length(tblTemplate$fldTemplateName), function(x) cbind(t=eval(parse(text=as.character(tblTemplate$fldPtsT[x]))),frq=eval(parse(text=as.character(tblTemplate$fldPtsFrq[x]))),amp=-0.01*eval(parse(text=as.character(tblTemplate$fldPtsAmp[x])))))
    }
    
    # reconstruct '__Template' objects
    if(std.type=='BIN') {
    	templates<-lapply(1:length(tblTemplate$fldTemplateName), function(x) new('binTemplate',clip.path=as.character(tblTemplate$fldClipPath[x]),samp.rate=tblTemplate$fldSampRate[x],pt.on=pt.on[[x]],pt.off=pt.off[[x]],t.step=tblTemplate$fldTStep[x],frq.step=tblTemplate$fldFrqStep[[x]],n.t.bins=tblTemplate$fldNTBins[x],first.t.bin=tblTemplate$fldFirstTBin[x],n.frq.bins=tblTemplate$fldNFrqBins[x],duration=tblTemplate$fldDuration[x],frq.lim=eval(parse(text=as.character(tblTemplate$fldFrqLim[x]))),wl=tblTemplate$fldFFTwl[x],ovlp=tblTemplate$fldFFTovlp[x],wn=as.character(tblTemplate$fldFFTwn[x]),score.cutoff=tblTemplate$fldScoreCutoff[x],comment=as.character(tblTemplate$fldComment[x])))
    } else if(std.type=='COR') {
    	templates<-lapply(1:length(tblTemplate$fldTemplateName), function(x) new('corTemplate',clip.path=as.character(tblTemplate$fldClipPath[x]),samp.rate=tblTemplate$fldSampRate[x],pts=pts[[x]],t.step=tblTemplate$fldTStep[x],frq.step=tblTemplate$fldFrqStep[x],n.t.bins=tblTemplate$fldNTBins[x],first.t.bin=tblTemplate$fldFirstTBin[x],n.frq.bins=tblTemplate$fldNFrqBins[x],duration=tblTemplate$fldDuration[x],frq.lim=eval(parse(text=as.character(tblTemplate$fldFrqLim[x]))),wl=tblTemplate$fldFFTwl[x],ovlp=tblTemplate$fldFFTovlp[x],wn=as.character(tblTemplate$fldFFTwn[x]),score.cutoff=tblTemplate$fldScoreCutoff[x],comment=as.character(tblTemplate$fldComment[x])))
    }        
    # Name each list with the template name    
    names(templates)<-as.character(tblTemplate$fldTemplateName)        
    # Join the templates as '__TemplateList'
    if(std.type=='BIN') {
    	templates<-new('binTemplateList',templates=templates)
    } else if(std.type=='COR') {
        templates<-new('corTemplateList',templates=templates)
    }
    
    message(if(class(tblTemplate)=='data.frame') {paste('Done! Download time:',round(Sys.time()-start.time,2),'seconds')
            } else paste("Download unsuccessful; RODBC returned errors: ",paste(tblTemplate, collapse=" ")))
    
    return(templates)
}        




