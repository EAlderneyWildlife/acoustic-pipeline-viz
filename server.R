library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(callr)
library(rmarkdown)
library(magrittr)
library(dplyr)
library(webshot)
if (!webshot::is_phantomjs_installed()) webshot::install_phantomjs()
Sys.setenv(OPENSSL_CONF="/dev/null")
options(shiny.maxRequestSize=30*1024^2) 

hideElements <- function(elementsId){
  lapply(elementsId, function(elementId){
    shinyjs::hide(elementId, time = 0)
  })
}

showElements <- function(elementsId){
  lapply(elementsId, function(elementId){
    shinyjs::show(elementId,time = 0)
  })
}

old_colnames <-
  c("RECORDING.FILE.NAME", "ORIGINAL.FILE.NAME",
    "ORIGINAL.FILE.PART", "LATITUDE",
    "LONGITUDE", "SPECIES", "SCIENTIFIC.NAME",
    "ENGLISH.NAME", "SPECIES.GROUP",
    "PROBABILITY", "WARNINGS", "ACTUAL.DATE",
    "SURVEY.DATE", "TIME", "CLASSIFIER.NAME",
    "USER.ID", "UPLOAD.KEY", "UPLOAD.NAME",
    "SURVEY.NAME"
    )

new_colnames <-
  c("RECORDING.FILE.NAME",  "ORIGINAL.FILE.NAME",  "ORIGINAL.FILE.PART",  
    "LATITUDE",  "LONGITUDE",  "SPECIES",  "SCIENTIFIC.NAME",  "ENGLISH.NAME",  
    "SPECIES.GROUP",  "PROBABILITY",  "WARNINGS",  "CALL.TYPE",  "ACTUAL.DATE",  
    "SURVEY.DATE",  "TIME",  "CLASSIFIER.NAME",  "USER.ID",  "UPLOAD.KEY",  
    "BATCH.NAME",  "PROJECT.NAME"
    )

function(input, output, session) {
  hideElements(c('selectedFiles', 'customizeBox', 'report'))
  observeEvent(
    req(input$upload), 
    {
      hideElements(c('selectedFiles', 'customizeBox', 'report'))
      ext <- tools::file_ext(input$upload$name)
      if(any(ext != 'csv')){
        showModal(modalDialog(
          title = "Warning",
          "Please only upload .csv files",
          easyClose = TRUE
        ))
      }else{
        inFile <- input$upload
        df <- inFile$datapath %>% lapply(read.csv) %>% dplyr::bind_rows() %>% dplyr::distinct()
        if(all(colnames(df) %in% old_colnames) |
           all(colnames(df) %in% new_colnames)
           ){
          showElements(c('customizeBox', 'report'))
          if(nrow(input$upload) > 1L){
            showElements(c('selectedFiles'))
            updateTabsetPanel(session, 'hiddenTabs', selected = 'selectedFilesTab')
          }else{
            updateTabsetPanel(session, 'hiddenTabs', selected = 'customizeTab')
          }
          updatePickerInput(
            session,
            inputId = "selectedFiles",
            choices = input$upload$name,
            selected = input$upload$name
          )
        }else{
          showModal(modalDialog(
            title = "Warning",
            "Please only upload .csv files output from the BTO's Acoustic Pipeline",
            easyClose = TRUE
          ))
        }
      }
    }, 
    ignoreNULL = F
  )
  
  observeEvent(
    input$nextTab1,
    {
      updateTabsetPanel(session, 'hiddenTabs', selected = 'selectedFilesTab')
    }
  )
  observeEvent(
    input$nextTab2,
    {
      updateTabsetPanel(session, 'hiddenTabs', selected = 'customizeTab')
    }
  )
  observeEvent(
    input$nextTab31,
    {
      updateTabsetPanel(session, 'hiddenTabs', selected = 'downloadTab')
    }
  )
  observeEvent(
    input$nextTab32,
    {
      updateTabsetPanel(session, 'hiddenTabs', selected = 'downloadTab')
    }
  )
  observeEvent(
    input$backTab1,
    {
      updateTabsetPanel(session, 'hiddenTabs', selected = 'uploadTab')
    }
  )
  observeEvent(
    input$backTab21,
    {
      updateTabsetPanel(session, 'hiddenTabs', selected = 'selectedFilesTab')
    }
  )
  observeEvent(
    input$backTab22,
    {
      updateTabsetPanel(session, 'hiddenTabs', selected = 'selectedFilesTab')
    }
  )
  observeEvent(
    input$backTab3,
    {
      updateTabsetPanel(session, 'hiddenTabs', selected = 'customizeTab')
    }
  )
  
  bbsData <- 
    reactive(
      {
        req(input$upload)
        ext <- tools::file_ext(input$upload$name)
        if(any(ext != 'csv')){
          showModal(modalDialog(
            title = "Warning",
            "Please only upload .csv files output from the BTO's Acoustic Pipeline",
            easyClose = TRUE
          ))
        }
        
        inFile <- 
          input$upload %>%
          .[which(.$name %in% input$selectedFiles),]
        if (any(is.null(inFile))){
          return(NULL)
        }else if(length(inFile) ==0L){
          return(NULL)
        }else{
          df <- inFile$datapath %>% lapply(read.csv) %>% dplyr::bind_rows() %>% dplyr::distinct()
          if(any(!(colnames(df) %in% old_colnames)) &
             any(!colnames(df) %in% new_colnames)){
            showModal(modalDialog(
              title = "Warning",
              "Please only upload .csv files output from the BTO's Acoustic Pipeline",
              easyClose = TRUE
            ))
            return(NULL)
          }else{
            return(df)
          }
        }
      }
    )
  
  observeEvent(input$speciesTables, {
    if(input$speciesTables %% 2 == 1){
      showElements(c("selectedColumns"))
    }else{
      hideElements(c("selectedColumns"))
    }
  })
  
  observeEvent(input$timePlots, {
    if(input$timePlots %% 2 == 1){
      showElements(c('selectedNightPhases', "emptyDays", "horizontalGridTime", "timeBreaks", 'timeFacets'))
    }else{
      hideElements(c('selectedNightPhases', "emptyDays", "horizontalGridTime", "timeBreaks", 'timeFacets'))
    }
  })
  
  observeEvent(input$detectionPlots, {
    if(input$detectionPlots %% 2 == 1){
      showElements(c("detectionBreaks", "horizontalGridDetection", "verticalGridDetection", 'detectionFacets'))
    }else{
      hideElements(c("detectionBreaks", "horizontalGridDetection", "verticalGridDetection", 'detectionFacets'))
    }
  })
  
  outputName <- reactive({
    if(input$outputName == ''){
      return(paste0('BBS_visual_summary_',as.character(Sys.Date())))
    }else{
      return(input$outputName)
    }
  })
  
  selectedColumns <- reactive({
    if(length(input$selectedColumns) == 0L){
      return('Scientific Name')
    }else{
      return(input$selectedColumns)
    }
  })
  
  selectedSpGroups <- reactive({
    if(length(input$selectedSpGroups) == 0L){
      return(sapply(c('Bats', 'Small Mammals','Bush-Crickets', 'Moths', 'Birds'), function(x){return(substr(tolower(x), 1, nchar(x)-1))}))
    }else{
      return(sapply(input$selectedSpGroups, function(x){return(substr(tolower(x), 1, nchar(x)-1))}))
    }
  })
  
  output$report <- 
    downloadHandler(
      filename = function(){paste0(outputName(),'.pdf')},
      content = function(file) {
        params <- 
          list(
            params_list = 
              list(
                n = input$n, 
                df = bbsData(), 
                name = outputName(),
                spGroups = selectedSpGroups(),
                detectionProb = input$detectionProb,
                deploymentSummary = input$deploymentSummary,
                speciesTables = input$speciesTables,
                selectedColumns = selectedColumns(),
                timePlots = input$timePlots,
                selectedNightPhases = input$selectedNightPhases,
                emptyDays = input$emptyDays,
                horizontalGridTime = input$horizontalGridTime,
                timeBreaks = input$timeBreaks,
                timeFacets = input$timeFacets, 
                detectionPlots = input$detectionPlots,
                horizontalGridDetection = input$horizontalGridDetection,
                verticalGridDetection = input$verticalGridDetection,
                detectionBreaks = input$detectionBreaks,
                detectionFacets = input$detectionFacets
              )
            )
  
        id <- showNotification(
          "Rendering report...",
          duration = NULL,
          closeButton = FALSE
        )
        on.exit(removeNotification(id), add = TRUE)
  
        report_path <- file.path(tempdir(), "report.Rmd")
        file.copy('report.Rmd', report_path, overwrite = TRUE)
        output <-
          withProgress(
            rmarkdown::render(report_path,
                            params = params,
                            output_format = 'pdf_document',
                            envir = new.env(parent = globalenv())
            ),
            message = ''
          )
        file.copy(output, file)
  
      }
    )
}