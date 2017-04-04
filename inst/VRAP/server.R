.libPaths(c("/usr/lib64/R/shiny_library",.libPaths()))
require(VRAP, quietly=TRUE)
require(shiny, quietly=TRUE)

source("common.R")   ## import EXAMPLES, shared with server.R

if (file.exists("parcores.R")) {
  source("parcores.R")
} else {
  parcores <- function() { 1 }
}

VIRUSDETECTIONMSG <- paste("<b>The uploaded file has triggered",
                           "a virus detection.</br>",
                           "Please contact a",
                           "system administrator.</b>")
UPLOADERRORMSG <- paste("<b>An error occurred during the file upload.",
                        "Please try again.</br>",
                        "If the error persists,",
                        "contact a system administrator.</b>")
WAITSCANNINGMSG <- "Scanning uploaded file - please wait."
WAITESTIMATINGMSG <- "Estimating run time - please wait."

PROCESSINGMSG <- paste("Calculating - please wait.",
                       "</br>",
                       "<em>Close browser window to halt processing</em>")

RESULTSHELP <- HTML(includeHTML("html/help_results.html"))
DOWNLOADSHELP <- HTML(includeHTML("html/help_downloads.html"))

ONSERVER <- serverInfo()$shinyServer
## ONSERVER <- TRUE
## ONSERVER <- FALSE
## if(.Platform$OS.type=="windows") NOSCAN <- TRUE
SCANCMD <- "clamdscan --fdpass --remove"

DEMOFILESPATH <- "demofiles"
OUTFILEBASENAME <- "VRAPresults"

appendTimestamp <- function(filename) {
  paste0(filename,"_",strftime(Sys.time(),"%Y%m%d_%H%M%S"))
}

cleanupKnitr <- function(knitrdir=".") {
  file.remove(Sys.glob(c("*.tex","*.aux","*.log")))
  unlink("figure",recursive=TRUE)
}

shinyServer( function(input, output, session) {

  ## number of (logical) cores available
  lcores <- parcores()
  
  OUTPUTDIR <- NULL

  currentinputfile <- NULL
  
  switches <- reactiveValues(resetupload=TRUE,
                             refreshresults=FALSE,
                             downloadstatus=FALSE,
                             inputfilestatus=TRUE,
                             sumOutput=RESULTSHELP,
                             byrOutput=RESULTSHELP,
                             escOutput=RESULTSHELP,
                             downloads=DOWNLOADSHELP
                             )
  ## reset the file upload control by
  ##    switches$resetupload = !switches$resetupload
  ## trigger a results tab refresh by
  ##    switches$refreshresults = !switches$refreshresults

  if (("request" %in% names(session)) &&
        ("HTTP_USER_AGENT" %in% names(session$request))) {
    osIsWindows <- grepl("windows", session$request$HTTP_USER_AGENT,
                         ignore.case=TRUE)
  } else {
    osIsWindows <- TRUE
  }

  setCurrentInput <- function(settofile=NULL,deleteexisting=FALSE) {
    if (deleteexisting) {
      if (!is.null(currentinputfile) &&
            file.exists(currentinputfile) &&
            !grepl(DEMOFILESPATH, currentinputfile)) {
        file.remove(currentinputfile)
      }
    }
    currentinputfile <<- settofile;
  }

  getCurrentInput <- function() {
    return(currentinputfile);
  }
  
  scanclam <- function(path) {
    if (is.null(path)) { return("SCANNOFILE") }

    returncode <- "SCANVIRUS"
    
    if(ONSERVER){
      cmd <- paste(SCANCMD, path)
      ec <- system(cmd)
    }else{
      ec <- 0
    }
    if (0 == ec) { return("SCANOKAY")}
    else if (1 == ec) {
      if (file.exists(path)) {
        file.remove(path)
      }
    }
    else { returncode <- "SCANERROR" }

    ## at this point, something isn't right with the upload
    ## and the file upload control needs resetting

    switches$resetupload = !switches$resetupload
    
    return(returncode)
  }

  getDownloadTempFile <- function() {
    tempfile("dltmp")
  }

  baseName <- NULL

  getBaseName <- function(delete=FALSE) {
    if (delete) {
      baseName <<- NULL
    } else if (is.null(baseName)) {
      baseName <<- appendTimestamp(OUTFILEBASENAME);
    }
    return( baseName );
  }

  getOutputDirectory <- function() {
    if (is.null(OUTPUTDIR)) {
      OUTPUTDIR <<- tempfile("vrapout")
      dir.create(path=OUTPUTDIR)
    }
    OUTPUTDIR
  }

  outputPathBase <- function() {
    return(file.path(getOutputDirectory(), getBaseName()))
  }
  
  outputPath <- function(ext) {
    return(paste0(outputPathBase(),ext))
  }
  
  inputFileDisplayName <- function() {
    return(
      switch(input$type,
             demo=names(EXAMPLES)[EXAMPLES==input$demofile],
             upload=input$file1$name,
             NULL
             )
    )
  }

  
  ## modifies lines in summary output to replace tmp file reference
  ## with name corresponding to file the user might download
  
  vrapFilenameFilter <- function(ext='') {
    filepath <- file.path(getOutputDirectory(), paste0(getBaseName(),".sum"))
    if (file.exists(filepath)) {
      thefile <- file(filepath,"r")
      lines <- readLines(thefile)
      close(thefile)
      uploadname <- inputFileDisplayName()
      lines <- lines[!grepl("^ *Copy of Input File.*",lines,perl=TRUE)]
      lines <- sub("(\\s*Input File:\\s+)     .*$",paste0("\\1",uploadname),
                   lines,perl=TRUE)
      outfile <- file(filepath,"w")
      cat(lines, file=outfile, sep="\n")
      close(outfile)
    }
  }

  triggerShinyBusy <- reactive({
    session$sendCustomMessage('setspinner','notbusy')
    return("")
  })

  triggerShinyBusy2 <- reactive({
    input$recalcButton
    return("")
  })

  output$dummy <- renderText({
    return(triggerShinyBusy())
  })

  output$dummy2 <- renderText({
    return(triggerShinyBusy2())
  })

  inputFileExists <- reactive({
    inputfile()
    curr <- getCurrentInput()
    if (!is.null(curr) && file.exists(curr)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  outputFilesExist <- function() {
    return(3==length(Sys.glob(outputPath(".*"))))
  }
  
  output$fileuploadctl1 <- renderUI({
    switches$resetupload
    input$type
    isolate({
    })
    fileInput('file1', 'Choose .rav file from your file system',
              accept=c(".rav",".RAV"))
  })

  clearOutputDirectory <- function(clearall=TRUE) {
    if (clearall) {
      getBaseName(delete=TRUE)
      unlink(file.path(getOutputDirectory(),"*"))
    } else {
      unlink(file.path(getOutputDirectory(),
                       c("*.byr","*.esc","*.sum","*.pdf")))
    }
  }

  ## clearOutputDirectory <- function() {
  ##   getBaseName(delete=TRUE)
  ##   unlink(file.path(getOutputDirectory(),"*"))
  ## }

  session$onSessionEnded(function(){
    unlink(file.path(getOutputDirectory()), recursive=TRUE)
  })
  
  
  output$contentssum <- renderUI({
    switches$sumOutput
  })

  output$contentsbyr <- renderUI({
    switches$byrOutput
  })

  output$contentsesc <- renderUI({
    switches$escOutput
  })

  downloadControls <- function() {
    sel <- "win"
    if (!is.null(osIsWindows) && !osIsWindows) { sel="unix" }
    report <- if(file.exists(outputPath(".pdf"))) {
      downloadButton('downloadRpt', 'Report')
    } else {
      NULL
    }
    return(
      list(
        tags$hr(),
        tags$h4('Download VRAP output files'),
        selectInput("os", "Choose OS:",
                    list("Windows" = "win", 
                         "Mac/Unix" = "unix"),
                    selected=sel),
        ## downloadButton('downloadRpt', 'Report'),
        report,
        downloadButton('downloadByr', 'Byr File'),
        downloadButton('downloadEsc', 'Esc File'),
        downloadButton('downloadSum', 'Sum File')
      )
    )
  }


  output$contentsdownloads <- renderUI({
    switches$downloads
  })

  observe({
    input$type
    isolate({
      clearOutputDirectory()
      switches$downloadstatus <- !switches$downloadstatus
      switches$sumOutput <- RESULTSHELP
      switches$byrOutput <- RESULTSHELP
      switches$escOutput <- RESULTSHELP
      switches$downloads <- DOWNLOADSHELP
    })
  })
  
  observe({
    input$recalcButton

    ## Don't do anything until after the first button push.
    isolate({
      if (input$recalcButton == 0){
        HTML("")
      } else if(input$type=="upload" && is.null(input$file1)) {
        HTML("Select an input file")
      } else {
        session$sendCustomMessage('setspinner','busy')
        session$sendCustomMessage('setwaitmsg', PROCESSINGMSG)

        clearOutputDirectory(clearall=FALSE)
        
        ## Note that just by virtue of checking the value
        ## of input$recalcButton, we're now going to get
        ## called whenever it is pushed.
        ## input$file1 will be NULL initially. After the
        ## user selects and uploads a file, it will be a
        ## data frame with 'name', 'size', 'type', and
        ## 'datapath' columns. The 'datapath' column will
        ## contain the local filenames where the data can
        ## be found.

        capture.output({
          tmp <- Main(inputfile()[1], OutFileBase=outputPathBase(),
                      NRuns=inNRuns(), silent=TRUE, lcores=lcores)
        })
        
        output <- VRAP:::SaveSummary(tmp$inputs,
                                     tmp$SummaryStats,
                                     tmp$staticvars)

        ## create the report file
        owd <- setwd(getOutputDirectory())
        VRAP:::WriteReport(inputfile()[1], getBaseName())
        cleanupKnitr()
        setwd(owd)
        ## done with report file

        ## adjust file name references in summary
        vrapFilenameFilter()

        ## generate HTML versions of text outputs
        switches$sumOutput <<- HTML(
          htmlize(getoutputfile(outputPath(".sum"))))
        
        switches$byrOutput <<- HTML(
          htmlize(getoutputfile(outputPath(".byr")), 2))

        switches$escOutput <<- HTML(
          htmlize(getoutputfile(outputPath(".esc")), 2))

        ## turn off the spinner, set statuses, and update the
        ## download controls to reflect files present
        session$sendCustomMessage('setspinner','notbusy')
        switches$downloadstatus <- !switches$downloadstatus
        switches$downloads <<- downloadControls();

        ## Open the summary results tab
        updateTabsetPanel(session, "tabset", selected="resultstabsum")
      }
    })
  })

  getoutputfile <- function(filepath) {
    singlestring <- ""

    if (file.exists(filepath)) {
      thefile <- file(filepath,"r")
      lines <- readLines(thefile)
      close(thefile)
      singlestring <- paste0(lines,collapse="\n")
    }

    return(singlestring)
  }

  htmlize <- function(blob, padding=0) {
    thehtml <- paste(blob, collapse="")
    thehtml <- str_replace_all(thehtml,"\r",'')
    thehtml <- str_replace_all(thehtml,"<","&lt;")
    thehtml <- str_replace_all(thehtml,">","&gt;")
    thehtml <- str_replace_all(thehtml,"\n","<br/>")
    thehtml <- str_replace_all(thehtml,"[[:space:]]", "&nbsp;")
    for (i in 1:padding) {thehtml <- paste0(thehtml,"<br/>")}
    thehtml <- paste(c("<div class='tabtext' id='outputtext'>",
                       thehtml,"</font></div>"), collapse="")
    return(thehtml)
    
  }


  output$timest <- renderUI({
    switch(input$type,
           upload = {
             inFile <- inputfile()
             if(is.null(inFile)) {return(NULL)}
             else {inFilePath <- inFile[1]}
           },
           demo = {
             ## inFilePath <- file.path(DEMOFILESPATH,input$demofile)
             inFilePath <- inputfile()[1]
           },
           {
             ## default choice
             return(NULL)
           })

    if (is.null(inFilePath) || !file.exists(inFilePath)) return(NULL)
    
    outfile = file.path(getOutputDirectory(),"vrap_timest")
    session$sendCustomMessage('setwaitmsg', WAITESTIMATINGMSG)

    capture.output({
      mainres = Main(inFilePath, OutFileBase=outfile, NRuns=1,
                     silent=TRUE, lcores=lcores)
    })

    tefiles <- Sys.glob(paste0(outfile,"*"))
    lapply(tefiles, function(x) {file.remove(x)})
    
    isolate({
      switches$sumOutput <- RESULTSHELP
      switches$byrOutput <- RESULTSHELP
      switches$escOutput <- RESULTSHELP
      switches$downloads <- DOWNLOADSHELP
      switches$downloadstatus <- !switches$downloadstatus
    })
    
    tmpruns = as.numeric(input$NRuns)
    if(tmpruns<0) tmpruns = VRAP:::GetInput(inFilePath)$NRuns

    processing.time <- mainres$time[1]
    out.time <- mainres$output.time[1]
    timest <- round(processing.time * tmpruns + out.time)

    HTML(paste("estimated time for this file and NRuns =",
               timest,"seconds<br/><br/>"))
  })

  observe({
    if(input$msgclosebtn > 0) {
      session$sendCustomMessage('clearmsg','reset')
    }
  })
  
  inputfile <- reactive({
    inFilePath <- NULL
    setCurrentInput(inFilePath, TRUE)
    if (input$type %in% c("paste","upload")) {
      inFilePath <- NULL

      testPath <- input$file1$datapath
      if (is.null(testPath) ||
            !file.exists(testPath)) { return(NULL) }
      
      session$sendCustomMessage('setwaitmsg', WAITSCANNINGMSG)

      switch(scanclam(testPath),
             SCANNOFILE={
               ## ignore
             },
             SCANOKAY=isolate({
               inFilePath <- testPath
             }),
             SCANVIRUS=isolate({
               ## virus found
               themsg <- VIRUSDETECTIONMSG
               session$sendCustomMessage('setmsg', themsg)
             }),
             isolate({
               ## error, default
               themsg <- UPLOADERRORMSG
               session$sendCustomMessage('setmsg', themsg)
             }))
    } else if (input$type == "demo") {
      ## inFilePath = normalizePath(file.path(DEMOFILESPATH,input$demofile))

      demoFile <- input$demofile
      demoFilePath <- normalizePath(file.path(DEMOFILESPATH,demoFile))
      demoCopy <- file.path(getOutputDirectory(), demoFile)
      file.copy(demoFilePath,demoCopy)
      inFileName <- demoFile
      inFilePath <- demoCopy
    }
    
    if (is.null(inFilePath)) return(NULL)
    setCurrentInput(inFilePath)
    outfile <- outputPathBase()
    return(c(inFilePath, outfile))
  })
  
  inNRuns <- reactive({
    return(as.numeric(input$NRuns))
  })


  ## file download handlers

  downloadHelper <- function(file, ext) {
    if (input$os == "win") {
      tmp.unix <- getDownloadTempFile()
      file.copy(outputPath(ext), tmp.unix)
      system(paste0("sed -e 's/$/\r/' ", tmp.unix," > ", file))
      file.remove(tmp.unix)
    }
    else {
      file.copy(outputPath(ext), file)
    }
  }

  output$downloadByr <- downloadHandler(
    filename = function() {paste0(getBaseName(),".byr")},
    content = function(file) {downloadHelper(file, ".byr")}
  )
  
  output$downloadSum <- downloadHandler(
    filename = function() {paste0(getBaseName(), ".sum")},
    content = function(file) {downloadHelper(file, ".sum")}
  )
  
  output$downloadEsc <- downloadHandler(
    filename = function() {paste0(getBaseName(),".esc")},
    content = function(file) {downloadHelper(file, ".esc")}
  )

  output$downloadRpt <- downloadHandler(
    filename = function() {paste0(getBaseName(), ".pdf")},
    content = function(file) {file.copy(outputPath(".pdf"), file)}
  )
  
  output$downloadExample <- downloadHandler(
    filename = function() {"example.rav" },
    content = function(file) {file.copy(file.path(DEMOFILESPATH,input$file2),file)}
  )
  
  ## This bit is detected if an input file has been selected
  output$fileselected <- reactive({
    switch(input$type,
           upload = {return(inputFileExists())},
           demo = {return(TRUE)}
           )
  })
  outputOptions(output, 'fileselected', suspendWhenHidden=FALSE)

  output$downloadsready <- reactive({
    switches$downloadstatus
    outputFilesExist()
  })
  outputOptions(output, 'downloadsready', suspendWhenHidden=FALSE)
})
