# For calculating scores for binary template matching 
# Modified: 2014 MAR 19

binMatch <-
function(
  survey,                 # Complete survey which is to be analyzed for calls. Wave object or vector. NOW A FILE PATH OR WAVE OBJECT
  templates,              # Template list.
  parallel=FALSE,         # If TRUE, mclapply is used for score calculations, for parallel processing (works for Linux only). If FALSE laply is used.
  show.prog=FALSE,        # If TRUE, progress is displayed during correlation calculations 
  warn=TRUE,              # If TRUE, will return a warning for time steps that don't match between the templates and the survey frequency-domain data.
  time.source='filename', # 'filename' or 'fileinfo' as the mtime source
  report.amp=FALSE,       # If TRUE, reports the on and off amplitudes (takes 3x as long) 
  ...                     # Additional arguments to the spectro function
) {

  # Check arguments
  if(missing(survey)) stop('Required argument survey is missing.')
  if(missing(templates)) stop('Required argument templates is missing.')

  # Packages
  if(parallel) {
    require(parallel) # NTS should replace with parallel::mclapply? or drop altogether?
    lapplyfun<-function(X,FUN) mclapply(X,FUN,mc.cores=detectCores()-1)
  } else lapplyfun<-lapply

  # Start tracking time (after loading packages)
  t.start<-Sys.time()

  # Work with survey outside template loop
  if(class(survey)=='character') { 
    survey.name<-survey
    file.ext<- gsub(".*\\.","",survey)
    if(time.source=='fileinfo') {
       file.time<-file.info(survey)$mtime
    } else if(time.source=='filename') {
       survey.short<-strsplit(survey,'/')[[1]][length(strsplit(survey,'/')[[1]])]
       date.time.info<-regmatches(survey.short,regexpr('[0-9]{4}-[0-9]{2}-[0-9]{2}[ _][0-9]{6}[ _][A-Z][SDM]T',survey.short))
       date.time.info<-gsub("_"," ",date.time.info)
       if(length(date.time.info)==1 && nchar(date.time.info)==21)
         file.time<-as.POSIXct(substr(date.time.info,start=1,stop=17),tz=substr(date.time.info,start=19,stop=21),format='%Y-%m-%d %H%M%S') 
       else {
         warning('time.source was set to \"filename\" but file name does not have date and time info, so using \"fileinfo\" instead')
         file.time<-file.info(survey)$mtime
       }
    }
    if(file.ext=='wav') survey<-readWave(filename=survey) else if(file.ext=='mp3') survey<-readMP3(filename=survey) else stop('File extension must be wav or mp3')
  } else if(class(survey)=='Wave') {
    file.time<-NA 
    survey.name<-'A Wave object'
  } else stop(paste('Expect file path or wave object for survey argument, got',survey))

  # score.L is a list for storing results
  score.L<-list()
  survey.data<-list()

  # Loop through templates
  for(i in names(templates@templates)) {
    cat('\nStarting ',i,'. . .\n')

    # Working with a single template
    template<-templates@templates[[i]]

    if(i==names(templates@templates)[1] || any(template@wl!=wl,template@ovlp!=ovlp,template@wn!=wn)) {
      cat('\n\tFourier transform on survey . . .')
      wl<-template@wl
      ovlp<-template@ovlp
      wn<-template@wn
      # Perform Fourier transform on survey
      survey.spec<-spectro(wave=survey,wl=wl,ovlp=ovlp,wn=wn,...)
      # NTS arbitrary adjustment to eliminate -Inf
      survey.spec$amp[is.infinite(survey.spec$amp)]<-min(survey.spec$amp[!is.infinite(survey.spec$amp)]) - 10
      frq.bins<-survey.spec$freq
      t.bins<-survey.spec$time
      t.survey<-length(survey@left)/survey@samp.rate
      t.step<-t.bins[2] - t.bins[1]
      frq.step<-frq.bins[2] - frq.bins[1]
      cat('\n\tContinuing. . .\n')
    }

    # Switch the order of columns in pt.on and pt.off to use them directly for indexing
    pt.on<-template@pt.on[,2:1]
    colnames(pt.on)<-c('frq','t')
    pt.off<-template@pt.off[,2:1]
    colnames(pt.off)<-c('frq','t')

    # Adjust pt.on and pt.off if step sizes differ
    if(!isTRUE(all.equal(template@t.step,t.step,tolerance=t.step/1E4))) {
      pt.on[,'t']<-round(pt.on[,'t']*template@t.step/t.step)
      pt.off[,'t']<-round(pt.off[,'t']*template@t.step/t.step)
      if(warn) warning('For ',i,', time step doesn\'t match survey time step: ',t.step,' != ',template@t.step)
    }
    if(!isTRUE(all.equal(template@frq.step,frq.step,tolerance=frq.step/1E4))) {
      pt.on[,'frq']<-round(pt.on[,'frq']*template@frq.step/frq.step)
      pt.off[,'frq']<-round(pt.off[,'frq']*template@frq.step/frq.step)
      if(warn) warning('For ',i,', frequency step does\'t match survey frequency step, ',frq.step,' != ',template@frq.step)
    }

    # Determine the frequency limits from the template points
    frq.lim<-frq.bins[range(pt.on[,'frq'],pt.off[,'frq'])] # HOW DOES THIS WORK????
    n.t.template<-diff(range(pt.on[,2],pt.off[,2]))

    # Get number of time windows/bins in frequency domain data
    n.t.survey<-length(survey.spec$time)
  
    # Pare down amplitude matrix based on filter frequencies 
    which.frq.bins<-which(survey.spec$freq >= frq.lim[1] & survey.spec$freq <= frq.lim[2])
    amp.survey<-survey.spec$amp[which.frq.bins,]
    # Need to shift indices in pt.on and pt.off
    pt.on[,'frq']<-pt.on[,'frq'] - min(which.frq.bins) + 1
    pt.off[,'frq']<-pt.off[,'frq'] - min(which.frq.bins) + 1

    # Create progress bar object if requested
    if(show.prog & !parallel) pb<-txtProgressBar(max=n.t.survey-n.t.template + 1,char='.',width=0,style=3)

    # Perform analysis for each time value (bin) of survey 
    # Starting time value (bin) of correlation window, set up as a list to use mclapply
    c.win.start<-as.list(1:(n.t.survey-n.t.template)) 
    if(report.amp){ # This will collect amplitude values in addition to scores
      score.survey<-unlist(
        lapplyfun(X=c.win.start,FUN=function(x) 
          {
          if(!parallel && show.prog) setTxtProgressBar(pb,x)
          pt.on.win<-cbind(pt.on[,1],pt.on[,2] + x - 1)  
          pt.off.win<-cbind(pt.off[,1],pt.off[,2] + x - 1)
          list(
            (on.ave<-sum(amp.survey[pt.on.win])/nrow(pt.on)) - (off.ave<-sum(amp.survey[pt.off.win])/nrow(pt.off)),  
            on.ave,
            off.ave
            #sum(amp.survey[pt.on.win])/nrow(pt.on),
            #sum(amp.survey[pt.off.win])/nrow(pt.off)
          )
          }
        )
      )
      } else { # This only collects scores 
        score.survey<-unlist(
          lapplyfun(X=c.win.start,FUN=function(x) 
          {
          if(!parallel && show.prog) setTxtProgressBar(pb,x)
          pt.on.win<-cbind(pt.on[,1],pt.on[,2] + x - 1)  
          pt.off.win<-cbind(pt.off[,1],pt.off[,2] + x - 1)  
          sum(amp.survey[pt.on.win])/nrow(pt.on) - sum(amp.survey[pt.off.win])/nrow(pt.off)
          }
          )
        )
      }

    # Collect correlation results and time (center of time bins) in data frame
    if(report.amp){ # This will collect amplitude values in addition to scores
      score.L[[i]]<-data.frame(
        date.time=file.time + survey.spec$time[1:(n.t.survey-n.t.template)+n.t.template/2] - t.survey,
        time=survey.spec$time[1:(n.t.survey-n.t.template)+n.t.template/2],
        score=score.survey[seq(from=1,to=length(score.survey),by=3)],
        on.amp=score.survey[seq(from=2,to=length(score.survey),by=3)],
        off.amp=score.survey[seq(from=3,to=length(score.survey),by=3)]
      )
    } else { # This only collects scores  
      score.L[[i]]<-data.frame(
        date.time=file.time + survey.spec$time[1:(n.t.survey-n.t.template)+n.t.template/2] - t.survey,
        time=survey.spec$time[1:(n.t.survey-n.t.template)+n.t.template/2],
        score=score.survey
      )
    }
    survey.data[[i]]<-list(amp=survey.spec$amp,t.bins=t.bins,frq.bins=frq.bins)
    cat('\n\tDone.\n')
  }

  # Calculate total run time
  t.run<-as.numeric(difftime(Sys.time(),t.start,units='secs'))
  time.info<-c(t.exe=signif(t.run,4),RTfactor=paste(signif(t.survey/as.numeric(t.run,units='secs'),4),'x',sep=''))

  # Return results 
  scores.obj<-new('templateScores',survey.name=survey.name,survey=survey,survey.data=survey.data,templates=templates@templates,scores=score.L,time=time.info)
  return(scores.obj)
}