library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)

fluidPage(
  shinyjs::useShinyjs(),
  #titlePanel("Bailiwick Bat Survey - Acoustic Pipeline Data Visualisation"),
  mainPanel(
    tabsetPanel(
      id = 'hiddenTabs',
      type = 'tabs',
      tabPanel(
        'Upload Data',
        value = 'uploadTab',
        br(),
        p("Upload your output from the BTO's Acoustic Pipeline to visualise your recordings."),
        p('Multiple files can be uploaded at once.'),
        br(),
        fileInput("upload", NULL, buttonLabel = "Upload", multiple = TRUE, accept = '.csv'),
        br(),
        actionButton(
          inputId = 'nextTab1',
          label = 'Next'
        )
      ),
      tabPanel(
        'Select Files to Merge',
        value = 'selectedFilesTab',
        br(),
        p('If you uploaded multiple outputs, select which to merge (default behaviour is to merge all).'),
        br(),
        pickerInput(
          inputId = 'selectedFiles', 
          label = 'Select which files you want to merge and analyse',
          multiple = T,
          choices = NULL,
          width = "100%",
          options = list(`actions-box` = TRUE, noneSelectedText = 'Nothing uploaded')
        ),
        br(),
        actionButton(
          inputId = 'backTab1',
          label = 'Back'
        ),
        actionButton(
          inputId = 'nextTab2',
          label = 'Next'
        )
      ),
      tabPanel(
        'Customize Output',
        value = 'customizeTab',
        br(),
        p('Customize your output. There are sensible defaults in place.'),
        br(),
        actionButton(
          inputId = 'backTab21',
          label = 'Back',
          style = 'background-color: grey, font-color: white'
        ),
        actionButton(
          inputId = 'nextTab31',
          label = 'Next',
        ),
        br(),
        br(),
        box(
          id = 'customizeBox',
          textInput(
            inputId = 'outputName',
            label = 'Name your output',
            placeholder = paste0('BBS_visual_summary_',as.character(Sys.Date()))
          ),
          pickerInput(
            inputId = 'selectedSpGroups', 
            label = 'Select which species groups to include',
            multiple = T,
            choices = c('Bats', 'Small Mammals','Bush-Crickets', 'Moths', 'Birds'),
            selected = c('Bats', 'Small Mammals','Bush-Crickets', 'Moths', 'Birds'),
            width = "100%",
            options = list(`actions-box` = TRUE, noneSelectedText = 'No groups selected - all will be included')
          ),
          sliderInput(
            inputId = 'detectionProb',
            label = 'Filter to only recordings within a set range of classification accuracy',
            min = 0, max = 1,
            step = 0.01,
            value = c(0,1),
            width = '100%'
          ),
          br(),
          materialSwitch(
            inputId = 'deploymentSummary',
            label = strong('Include detector deployment summary'),
            width = '100%',
            value = T,
            right = T,
            status = 'primary'
          ),
          br(),
          br(),
          materialSwitch(
            inputId = 'speciesTables',
            label = strong('Include summary tables of species detected'),
            width = '100%',
            value = T,
            right = T,
            status = 'primary'
          ),
          pickerInput(
            inputId = 'selectedColumns', 
            label = 'Select which columns you want to include',
            multiple = T,
            choices = c('Scientific Name', 'English Name', 
                        'Maximum Classification Probability', 'Number Of Records',
                        'Warnings'),
            selected = c('Scientific Name', 'English Name', 
                         'Maximum Classification Probability', 'Number Of Records',
                         'Warnings'),
            width = "100%",
            options = list(`actions-box` = T, noneSelectedText = 'No columns selected - Scientific Name will be included')
          ),
          br(),
          br(),
          materialSwitch(
            inputId = 'timePlots',
            label = strong("Include plots of species' recordings throughout each night"),
            value = T,
            width = '100%',
            right = T,
            status = 'primary'
          ),
          pickerInput(
            inputId = 'selectedNightPhases', 
            label = 'Select which phases of night you want to show',
            multiple = T,
            choices = c('Civil Twilight', 'Nautical Twilight', 
                        'Astronomical Twilight', 'Night'),
            selected = c('Civil Twilight', 'Nautical Twilight', 
                         'Astronomical Twilight', 'Night'),
            width = "100%",
            options = list(`actions-box` = T, noneSelectedText = 'No phases selected')
          ),
          materialSwitch(
            inputId = 'emptyDays',
            label = strong('Include nights with no records for a species'),
            width = "100%",
            value = T
          ),
          materialSwitch(
            inputId = 'horizontalGridTime',
            label = strong('Include horizontal grid lines'),
            value = FALSE,
            width = "100%"
          ),
          sliderTextInput(
            inputId = 'timeBreaks',
            label = 'Set time bands to group recordings',
            choices = c(10,15,20,30,60),
            selected = 30,
            post = ' minutes',
            width = '100%'
          ),
          radioGroupButtons(
            inputId = 'timeFacets',
            label = 'Set how many nights to show in one row of the report',
            choices = c(1,2,4),
            selected = 4,
            width = '100%',
            individual = T,
            direction = 'horizontal'
          ),
          br(),
          br(),
          materialSwitch(
            inputId = 'detectionPlots',
            label = strong("Include plots of classifier accuracy for species' recordings"),
            value = TRUE, 
            right = T,
            width = '100%',
            status = 'primary'
          ),
          materialSwitch(
            inputId = 'horizontalGridDetection',
            label = strong('Include horizontal grid lines'),
            width = "100%",
            value = TRUE
          ),
          materialSwitch(
            inputId = 'verticalGridDetection',
            label = strong('Include vertical grid lines'),
            width = "100%",
            value = TRUE
          ),
          sliderTextInput(
            inputId = 'detectionBreaks',
            label = 'Set probability bands to group recordings',
            choices = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.25, 0.333, 0.5),
            selected = 0.05,
            width = '100%'
          ),
          radioGroupButtons(
            inputId = 'detectionFacets',
            label = 'Set how many species to show in one row of the report',
            choices = c(1, 2),
            selected = 2,
            width = '100%',
            direction = 'horizontal',
            individual = T
          ),
          br(),
          actionButton(
            inputId = 'backTab22',
            label = 'Back',
            style = 'background-color: grey, font-color: white'
          ),
          actionButton(
            inputId = 'nextTab32',
            label = 'Next',
          )
        )
      ),
      tabPanel(
        'Download Report',
        value = 'downloadTab',
        br(),
        downloadButton("report", "Generate report"),
        br(),
        br(),
        actionButton(
          inputId = 'backTab3',
          label = 'Back'
        ),
      )
    )
  )
)