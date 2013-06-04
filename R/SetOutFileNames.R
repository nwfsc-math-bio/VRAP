# '*****  SetOutFileNames  ******
# 'Used by GetOutFiles and also by GetCommandLine
# '************************************************************************
SetOutFileNames = function(BaseName, inputs){
# NoExtensionName  'stores path and name, but not extension
# Position         'position of final "\" in pathname
# PathName         'string containing the path (including last "\")
# InFileNameOnly   'name (excl path) of input file
    
    #'SET OUTPUT FILES
    inputs$OutFileSum = paste(BaseName,".sum",sep="")
    inputs$OutFileEsc = paste(BaseName,".esc",sep="")
    inputs$OutFileByr = paste(BaseName,".byr",sep="")
    inputs$OutFileLog = paste(BaseName,".log",sep="")
    inputs$InFileCopy = paste(BaseName,".rav",sep="")
    return(inputs)
}