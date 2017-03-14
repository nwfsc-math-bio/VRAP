#'*****  WriteReport   *****
#'Create a pdf with basic information about the fit
#' InFile is the .rav input file
#' OutFileBase is where to save the outputfiles
WriteReport=function(InFile=NULL, OutFileBase=NULL, show.file=FALSE){
  
  #'READ INPUT DATA AND CALCULATE AEQs
  #'direct from .rav file or simple calculation from rav file inputs
  inputs=GetInput(InFile)
  if(is.null(OutFileBase)){
    tmp=strsplit(InFile,"\\\\")[[1]]
    InFileBase=tmp[length(tmp)]
    tmp=strsplit(InFileBase,"/")[[1]]
    InFileBase=tmp[length(tmp)]   
    if(str_detect(InFileBase,"[.]")){
      OutFileBase=strsplit(InFileBase,"[.]")[[1]][1]; 
    }else{
      OutFileBase=InFileBase;
    }
  }
  
  #add the output file names to the inputs
  inputs = SetOutFileNames(OutFileBase, inputs, PathName=paste(getwd(),"/",sep=""))
  
  output.file = inputs$OutFileReport

  
  if(str_sub(output.file,-4)==".pdf") output.file=str_sub(inputs$OutFileReport,1,-5)
  pkgpath <- find.package("VRAP")
  if(inputs$StepFunc=="ER") path=file.path(pkgpath, "doc", "Report-knitr-ER.xRnw")  
  knit2pdf(path, output=paste(output.file,".tex",sep=""), envir=sys.nframe(), quiet=TRUE)
  if(show.file) file.show(paste(output.file,".pdf",sep=""))
  
}