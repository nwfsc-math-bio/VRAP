# '*****  SetOutFileNames  ******
# 'Used by GetOutFiles and also by GetCommandLine
# '************************************************************************
SetOutFileNames = function(BaseName, inputs, PathName=NULL){
# NoExtensionName  'stores path and name, but not extension; in VB, not R
# Position         'position of final "\" in pathname; in VB, not R
# PathName         'string containing the path (including last "\")
# InFileNameOnly   'name (excl path) of input file; in VB, not R
    
    #'SET OUTPUT FILES
    inputs$OutFileSum = paste(PathName,BaseName,".sum",sep="")
    inputs$OutFileEsc = paste(PathName,BaseName,".esc",sep="")
    inputs$OutFileByr = paste(PathName,BaseName,".byr",sep="")
    inputs$OutFileLog = paste(PathName,BaseName,".log",sep="")
    inputs$InFileCopy = paste(PathName,BaseName,".rav",sep="")
    inputs$OutFileReport = paste(PathName,BaseName,".pdf",sep="")
    return(inputs)
}