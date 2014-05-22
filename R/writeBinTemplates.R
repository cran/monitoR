# For writing out a binary template list
# Modified: 2014 FEB 07

writeBinTemplates <-
function(
   ...,
   dir='.',
   ext='bt',
   parallel=FALSE
) {

   if(length(list(...))>1) templates<-combineBinTemplates(...) else templates<-list(...)[[1]]

   # Create dir directory if it doesn't exist
   if(!file.exists(dir)) dir.create(dir)

   if(parallel) {
      require(parallel)
      lapplyfun<-function(X,FUN) mclapply(X,FUN,mc.cores=detectCores())
   } else lapplyfun<-lapply

   names.templates<-names(templates@templates)
   y<-lapplyfun(names.templates,function(x) writeOneBinTemplate(template=templates@templates[[x]],file=paste(dir,'/',x,'.',ext,sep='')))
   invisible(NULL)
}
