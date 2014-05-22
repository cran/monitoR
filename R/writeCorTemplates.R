# For writing correlation template lists to files
# Modified: 2013 JUNE 13

writeCorTemplates <-
function(
   ...,
   dir='.',
   ext='ct',
   parallel=FALSE
) {

   if(length(list(...))>1) templates<-combineCorTemplates(...) else templates<-list(...)[[1]]

   # Create dir directory if it doesn't exist
   if(!file.exists(dir)) dir.create(dir)

   if(parallel) {
      require(parallel)
      lapplyfun<-function(X,FUN) mclapply(X,FUN,mc.cores=detectCores())
   } else lapplyfun<-lapply

   names.templates<-names(templates@templates)
   y<-lapplyfun(names.templates,function(x) writeOneCorTemplate(template=templates@templates[[x]],file=paste(dir,'/',x,'.',ext,sep='')))
   invisible()
}
