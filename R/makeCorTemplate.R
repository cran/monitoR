# For making correlation templates
# Modified: 2014 MAR 29

makeCorTemplate <-
function(
   clip,                                         # File path to wav or mp3 file
   t.lim=NA,                                     # Time limits of spectrogram plot or template
   frq.lim=c(0,12),                              # Frequency limits of spectrogram plot or template 
   select='auto',                                # How should points be selected? Options are 'cell' or 'click' (the same), 'rectangle', 'line', 'auto'
   dens=1,                                       # Density of points included with 'rectangle', 'line', and 'auto' (fraction of 1)
   score.cutoff=0.4,
   name='A',                                     # Name of template
   comment="",
   spec.col=gray.3(),                            # Color palette for spectrogram
   sel.col=ifelse(dens==1,'#99009975','orange'), # Color used for plotting selected points
   wl=512,                                       # Window length for spectro
   ovlp=0,                                       # % overlap between windows for spectro
   wn='hanning',                                 # Window type for spectro
   ...
){

   # Creates a wav file for clip if it isn't already a file
   clip<-getClip(clip,name=deparse(substitute(clip)))
   clip.path<-clip
   clip<-readClip(clip)

   # Trim clip
   if(is.na(t.lim[1])) {t.lim<-c(0,Inf)} else {clip<-cutw(clip,from=t.lim[1],to=t.lim[2])}
   samp.rate<-clip@samp.rate
   
   # Fourier transform
   t.survey<-length(clip@left)/clip@samp.rate
   fspec<-spectro(wave=clip,wl=wl,ovlp=ovlp,wn=wn,...)
   
   # Filter amplitudes 
   t.bins<-fspec$time
   n.t.bins<-length(t.bins)
   which.t.bins<-1:n.t.bins
   which.frq.bins<-which(fspec$freq >= frq.lim[1] & fspec$freq <= frq.lim[2])
   frq.bins<-fspec$freq[which.frq.bins]
   n.frq.bins<-length(frq.bins)
   amp<-round(fspec$amp[which.frq.bins,],2)

   # Create empty matrix for identifying selected cells
   on.mat<-matrix(0,nrow=n.frq.bins,ncol=n.t.bins)
   
   # Bin steps
   t.step<-t.bins[2]-t.bins[1]
   frq.step<-frq.bins[2]-frq.bins[1]
   
   # Make plot
   par(mar=c(5,4,4,4))
   
   # Create plot
   image(x=which.t.bins,y=which.frq.bins,t(amp),col=spec.col,xlab='Time (s)',ylab='Frequency (kHz)',las=1,useRaster=TRUE,axes=FALSE,las=1)
   t.bin.ticks<-pretty(t.bins,n=6);axis(1,at=t.bin.ticks/t.step,labels=t.bin.ticks+t.lim[1])
   frq.bin.ticks<-pretty(frq.bins,n=6);axis(2,at=frq.bin.ticks/frq.step,labels=frq.bin.ticks,las=1)
   axis(3);axis(4,las=1);box()
   
   # Point-by-point selection
   if(select%in%c('cell','click')) {
      cat('\nSelect points with left mouse click. To finish, right click.\n')
   }
   if(select%in%c('cell','click')) {
      # Select "on" points
      i<-0
      pts<-NULL
      pos<-NULL
      while(!is.null(pos)|i==0) {
         i<-i + 1
         pos<-locator(n=1)
         if(!is.null(pos)) pos<-lapply(pos,round)
         if(!is.null(pos)) pos$y<-pos$y - min(which.frq.bins) + 1
         pts<-rbind(pts,as.numeric(pos))
         on.mat[pts[,2:1,drop=FALSE]]<-1
         image(x=which.t.bins,y=which.frq.bins,t(on.mat),col=c('transparent',sel.col),add=TRUE)
         if(!is.null(pos)) cat('\n',nrow(pts),' selected')
      } 
      pt.amp<-amp[pts[,2:1,drop=FALSE]]
      pt.on<-cbind(pts,pt.amp)
      colnames(pt.on)<-c('t','frq','amp')
      pt.on[,'frq']<-pt.on[,'frq'] + min(which.frq.bins) - 1
      pts<-pt.on

   } else if(select %in% c('rect','rectangle')) {
      # Rectangular selection
      cat('\nRectangular selection\n')
      i<-0
      pos1<-pt.on<-NULL
      bin.amp<-0*amp
      while(!is.null(pos1)|i==0) {
         # On cells first
         cat('\nSelect upper left corner of rectangle with a left click. Right click to exit.\n')
         i<-i + 1
         pos1<-locator(n=1)
         points(pos1$x,pos1$y,pch=22,cex=0.5,col='red',bg='red')
         if(!is.null(pos1)) {
            cat('\nSelect lower right corner of rectangle with a left click.\n')
            pos2<-locator(n=1)
            points(pos2$x,pos2$y,pch=22,cex=0.5,col='red',bg='red')

            # First find positions within the matrix that are within the rectangle
            frq.in.rect<-which.frq.bins<pos1$y & which.frq.bins>pos2$y
            x.in.rect<-which.t.bins>pos1$x & which.t.bins<pos2$x
            # Find positions of cells within the dens grid
            frq.in.grid<-which.frq.bins %in% seq(min(which.frq.bins),max(which.frq.bins),ceiling(1/sqrt(dens)))
            x.in.grid<-which.t.bins %in% seq(min(which.t.bins),max(which.t.bins),ceiling(1/sqrt(dens)))
            # Set cells that meet criteria to 1 in bin.amp
            on.mat[frq.in.rect & frq.in.grid,x.in.rect & x.in.grid]<-1

            # Then find locations of all selected cells within rectangle
            pts<-which(on.mat==1,arr.ind=TRUE)
            pts<-pts[,2:1]
            colnames(pts)<-c('t','frq')
            pts[,'frq']<-pts[,'frq'] + min(which.frq.bins) - 1
            pt.on<-rbind(pt.on,pts)

            # Add to plot
            # First replot amplitude data over existing plot 
            image(x=which.t.bins,y=which.frq.bins,t(amp),col=spec.col,add=TRUE)
            image(x=which.t.bins,y=which.frq.bins,t(on.mat),col=c('transparent',sel.col),add=TRUE)
         }
         pt.on<-unique(pt.on)
      } 
      pt.on.trimmed<-pt.on
      pt.on.trimmed[,'frq']<-pt.on.trimmed[,'frq'] - min(which.frq.bins) + 1
      pt.amp<-amp[pt.on.trimmed[,2:1,drop=FALSE]]
      pt.on<-cbind(pt.on,pt.amp)
      colnames(pt.on)<-c('t','frq','amp')
      pts<-pt.on
      
   } else if(select %in% c('line')) {
      # Line selection
      cat('\nLine selection\n')
      i<-0
      pos1<-pt.on<-NULL
      bin.amp<-0*amp
      while(!is.null(pos1)|i==0) {
         # On cells first
         cat('\nSelect left or top point. Right click to exit.\n')
         i<-i + 1
         pos1<-locator(n=1)
         points(pos1$x,pos1$y,pch=22,cex=0.5,col='red',bg='red')
         if(!is.null(pos1)) {
            cat('\nSelect right or bottom point.\n')
            pos2<-locator(n=1)
            points(pos2$x,pos2$y,pch=22,cex=0.5,col='red',bg='red')
            
            # Determine if this is a horizontal or vertical line
            if(abs(pos2$y - pos1$y) < abs(pos2$x - pos1$x)) {
            # horizontal
               frq.in.grid<-frq.in.line<-which.frq.bins==round(pos1$y)
               x.in.line<-which.t.bins>pos1$x & which.t.bins<pos2$x
               x.in.grid<-which.t.bins %in% seq(min(which.t.bins),max(which.t.bins),ceiling(1/dens))
            } else {
            # Vertical
               x.in.grid<-x.in.line<-which.t.bins==round(pos1$x)
               frq.in.line<-which.frq.bins>pos2$y & which.frq.bins<pos1$y
               frq.in.grid<-which.frq.bins %in% seq(min(which.frq.bins),max(which.frq.bins),ceiling(1/dens))
            }               

            # Set cells that meet criteria to 1 in bin.amp
            on.mat[frq.in.line & frq.in.grid,x.in.line & x.in.grid]<-1

            # Then find locations of all selected cells within rectangle
            pts<-which(on.mat==1,arr.ind=TRUE)
            pts<-pts[,2:1]
            colnames(pts)<-c('t','frq')
            pts[,'frq']<-pts[,'frq'] + min(which.frq.bins) - 1
            pt.on<-rbind(pt.on,pts)

            # Add to plot
            # First replot amplitude data over existing plot 
            image(x=which.t.bins,y=which.frq.bins,t(amp),col=spec.col,add=TRUE)
            image(x=which.t.bins,y=which.frq.bins,t(on.mat),col=c('transparent',sel.col),zlim=c(0,1),add=TRUE)

         }
         pt.on<-unique(pt.on)
      } 
      pt.on.trimmed<-pt.on
      pt.on.trimmed[,'frq']<-pt.on.trimmed[,'frq'] - min(which.frq.bins) + 1
      pt.amp<-amp[pt.on.trimmed[,2:1,drop=FALSE]]
      pt.on<-cbind(pt.on,pt.amp)
      colnames(pt.on)<-c('t','frq','amp')
      pts<-pt.on
      
   } else if(select %in% c('auto','automatic')) {
      # Automatic point selection
      cat('\nAutomatic point selection.\n')

      # On cells first
      # Find cells in grid
      frq.in.grid<-which.frq.bins %in% seq(min(which.frq.bins),max(which.frq.bins),ceiling(1/sqrt(dens)))
      x.in.grid<-which.t.bins %in% seq(min(which.t.bins),max(which.t.bins),ceiling(1/sqrt(dens)))
      # Set cells that meet criteria to 1 in bin.amp
      on.mat[frq.in.grid,x.in.grid]<-1
      
      # Then find locations of 
      pts<-which(on.mat==1,arr.ind=TRUE)
      pts<-pts[,2:1]
      colnames(pts)<-c('t','frq')
      pts[,'frq']<-pts[,'frq'] + min(which.frq.bins) - 1
      pt.on<-pts

      # Add to plot
      # First replot amplitude data over existing plot 
      image(x=which.t.bins,y=which.frq.bins,t(amp),col=spec.col,add=TRUE)
      image(x=which.t.bins,y=which.frq.bins,t(on.mat),zlim=c(0,1),col=c('transparent',sel.col),add=TRUE)

      # Get amplitudes
      pts<-pt.on
      pts.trimmed<-pts
      pts.trimmed[,'frq']<-pts.trimmed[,'frq'] - min(which.frq.bins) + 1
      pt.amp<-amp[pts.trimmed[,2:1,drop=FALSE]]
      pts<-cbind(pts,pt.amp)
      colnames(pts)<-c('t','frq','amp')
   }
    

   t.shift<-min(pts[,1])
   first.t.bin<-t.shift*t.step + t.lim[1] - t.step
   pts[,'t']<-pts[,'t'] - t.shift + 1

   n.t.bins<-diff(range(pts[,'t']))
   n.frq.bins<-diff(range(pts[,'frq']))
   duration<-n.t.bins*t.step
   frq.lim<-range(pts[,'frq'])*frq.step

   template<-list(new('corTemplate',clip.path=clip.path,samp.rate=as.integer(samp.rate),pts=pts,t.step=t.step,frq.step=frq.step,n.t.bins=as.integer(n.t.bins),first.t.bin=first.t.bin,n.frq.bins=as.integer(n.frq.bins),duration=duration,frq.lim=frq.lim,wl=as.integer(wl),ovlp=as.integer(ovlp),wn=wn,score.cutoff=score.cutoff,comment=comment))
   names(template)<-name
   template<-new('corTemplateList',templates=template)

   cat('\nDone.\n')
   return(template)
}
