shinyUI(pageWithSidebar(
  headerPanel("VRAP R 1.3.1"),
  sidebarPanel(
    fileInput('file1', 'Step 1: Choose .rav file from your file system:',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    uiOutput('timest'),
    radioButtons('NRuns', 'Step 2: Choose (or change) number of runs (NRuns) per simulation',
                 c('1'=1, 
                   '10'=10, 
                   '100'=100,
                   '1000'=1000,
                   'Use .rav NRuns' = -1),
                 'Use .rav NRuns'),
    actionButton('recalcButton', 'Step 3: Run VRAP with selected file and NRuns'),
    conditionalPanel("input.recalcButton > 0 && !(updateBusy()) && !( $('#contents').hasClass('recalculating') )", 
                     tags$hr(),
                     tags$h3('Download VRAP output files'),
                     selectInput("os", "Choose OS:",
                                 list("Windows" = "win", 
                                      "Mac/Unix" = "unix")),
                     tags$br(),
                     downloadButton('downloadByr', 'Byr File'),
                     downloadButton('downloadEsc', 'Esc File'),
                     downloadButton('downloadSum', 'Sum File') ),
    tags$hr(),
    tags$h5('If needed, you can download an example .rav file to use:'),
    selectInput( "file2", "Choose an example .rav file",
                 c("Bev-Holt, no covariates, ER" = "exampleB2ER.rav",
                   "Bev-Holt, with covariates, Pop" = "exampleB4Pop.rav") ),
    tags$br(),
    downloadButton('downloadExample', 'Download Example File')    
  ),
  mainPanel(
    conditionalPanel("!(updateBusy()) && !( $('#contents').hasClass('recalculating') )", uiOutput('contents')),
    conditionalPanel("updateBusy() || $('#contents').hasClass('recalculating')",
                     id='progressIndicator',                     
                     "Working... (to kill, close the browser tab)",
                     div(id='progress',includeHTML("timer.js"))
    )
  )
))
