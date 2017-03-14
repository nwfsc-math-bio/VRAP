shinyServer( function(input, output) {
  require(VRAP)
  require(shiny) #0.6.0
  #require(shinyIncubator) #devtools::install_github("shiny-incubator", "rstudio")
  output$timest <- renderUI({
    inFile <- input$file1
    
    if (is.null(inFile)) return(NULL)
    
    #outfile = strsplit(inFile$name,"[.]rav")[[1]][1]
    outfile = "VRAPoutfile"
    
    timest = Main(inFile$datapath, OutFileBase=outfile, NRuns=1)$time[1]
    tmpruns = as.numeric(input$NRuns)
    if(tmpruns<0) tmpruns = VRAP:::GetInput(inFile$datapath)$NRuns
    timest = round(timest * tmpruns)
    HTML(paste("estimated time for this file and NRuns =",timest,"seconds<br/><br/>"))
  })
  
  inputfile <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile)) return(NULL)
    
    #outfile = strsplit(inFile$name,"[.]rav")[[1]][1]
    outfile = "VRAPoutfile"
    return(c(inFile$datapath, outfile))
  })
  
  inNRuns <- reactive({
    return(as.numeric(input$NRuns))
  })
  
  output$contents <- renderUI({
    # Don't do anything until after the first button push.
    if (input$recalcButton == 0){
      return(HTML(""))
    }
    
    # Note that just by virtue of checking the value of input$recalcButton,
    # we're now going to get called whenever it is pushed.
    
    return(isolate({
      
      # input$file1 will be NULL initially. After the user selects and uploads a 
      # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
      # columns. The 'datapath' column will contain the local filenames where the 
      # data can be found.
      
      tmp = Main(inputfile()[1], OutFileBase=inputfile()[2], NRuns=inNRuns() )
      output = VRAP:::SaveSummary(tmp$inputs, tmp$SummaryStats, tmp$staticvars)
      output = paste(output, collapse="")
      output = str_replace_all(output,"\n","<br/>")
      output = str_replace_all(output,"[[:space:]]","&nbsp;")
      output = paste(c("<div id='outputtext'><font face='courier'>", output,"</font></div>"), collapse="")
      HTML(output)
    }))
  })
  
  
  output$downloadByr = downloadHandler(
    filename = function() {
      filetag=".byr"
      paste(inputfile()[2], filetag, sep='') },
    content = function(file) {
      filetag=".byr"
      if(input$os=="win"){ 
        file.copy(paste(inputfile()[2], filetag, sep=''), "tmp.unix")
        system(paste("sed -e 's/$/\r/' tmp.unix > ", file, sep=""))
        file.remove("tmp.unix")
      }else{
        file.copy(paste(inputfile()[2], filetag, sep=''), file)
      }
    }
  )
  
  output$downloadSum = downloadHandler(
    filename = function() {
      filetag=".sum"
      paste(inputfile()[2], filetag, sep='') },
    content = function(file) {
      filetag=".sum"
      if(input$os=="win"){ 
        file.copy(paste(inputfile()[2], filetag, sep=''), "tmp.unix")
        system(paste("sed -e 's/$/\r/' tmp.unix > ", file, sep=""))
        file.remove("tmp.unix")
      }else{
        file.copy(paste(inputfile()[2], filetag, sep=''), file)
      }
    }
  )
  
  output$downloadEsc = downloadHandler(
    filename = function() {
      filetag=".esc"
      paste(inputfile()[2], filetag, sep='') },
    content = function(file) {
      filetag=".esc"
      if(input$os=="win"){ 
        file.copy(paste(inputfile()[2], filetag, sep=''), "tmp.unix")
        system(paste("sed -e 's/$/\r/' tmp.unix > ", file, sep=""))
        file.remove("tmp.unix")
      }else{
        file.copy(paste(inputfile()[2], filetag, sep=''), file)
      }
    }
  )
  
  output$downloadExample = downloadHandler(
    filename = function() {
      "example.rav" },
    content = function(file) {
      file.copy(input$file2, file)
    }
  ) 
})