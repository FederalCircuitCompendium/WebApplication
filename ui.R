library(shiny)
library(RSQLite)
library(shinythemes)

navbarPage(theme = shinytheme("yeti"),
           "Federal Circuit Decisions Database ",
           
           ###################################
           #Creating tab for querying the data
           tabPanel("Query Data",
                    sidebarLayout(
                      sidebarPanel(
                        tabsetPanel(
                          #Subtab for filtering the data
                          tabPanel("Filter", 
                                   br(),
                                   #Generic uiOutputs that depend on the databases current state, so they are rendered in server.R
                                   uiOutput(outputId = 'duplicateFilter'),
                                   uiOutput(outputId = 'dateRange'),
                                   uiOutput(outputId = 'originFilter'),
                                   uiOutput(outputId = 'typeFilter'),
                                   uiOutput(outputId = 'docTypeFilter'),
                                   uiOutput(outputId = 'enBancFilter'),
                                   uiOutput(outputId = 'opinion1Filter'),
                                   uiOutput(outputId = 'yearFilter'),
                                   uiOutput(outputId = 'TribOfOriginFilter'),
                                   uiOutput(outputId = 'DisputeTypeFilter'),
                                   uiOutput(outputId = 'DispGeneralFilter'),
                                   uiOutput(outputId = 'WithdrawnFilter')
                          ),
                          #Subtab for choosing columns to display
                          tabPanel("Display",
                                   br(),
                                   #Generic uiOutput that is rendered in server.R
                                   uiOutput(outputId = 'display'))
                        )
                      ),
                      #Data table which displays the data and download button
                      mainPanel(
                        DT::dataTableOutput('mytable1'), 
                        downloadButton('download', 'Download'),
                        p("*Download will only include columns chosen under 'Display Columns'")
                      )
                    )
           ),
           
          
           ##########################################
           #Creating tab to view an existing record
           tabPanel("View a Record",
                    mainPanel(
                      wellPanel(
                        fluidRow(
                          column(6,
                                 #Text box to enter desired record ID to update
                                 textInput('updateID', "Enter ID:", "")
                          ),
                          column(6,
                                 br(),
                                 #Button to retrieve the record for the supplied ID
                                 actionButton("getRecord", "Get Record"),
                                 
                                 #Text output to be displayed for invalid supplied ID's
                                 textOutput('IDNotFound')
                          )
                        )
                      ),
                      wellPanel(
                        fluidRow(
                          column(6,
                                 #Creating generic uiOutputs to hold the record after being retrieved for update
                                 #These depend on the current state of the database, so it is rendered in server.R
                                 uiOutput('caseDateUpdate'), 
                                 uiOutput('originUpdate'), 
                                 uiOutput('yearUpdate'),
                                 uiOutput('caseNameUpdate'), 
                                 uiOutput('typeUpdate'), 
                                 uiOutput('appealNumberUpdate'), 
                                 uiOutput('docTypeUpdate'), 
                                 uiOutput('enBancUpdate'), 
                                 uiOutput('judge1Update'), 
                                 uiOutput('judge2Update'),
                                 uiOutput('judge3Update'),
                                 uiOutput('opinion1Update'),
                                 uiOutput('opinion1AuthorUpdate')                                 
                          ),
                          column(6,
                                 uiOutput('opinion2Update'),
                                 uiOutput('opinion2AuthorUpdate'),
                                 uiOutput('opinion3Update'),
                                 uiOutput('opinion3AuthorUpdate'),
                                 uiOutput('notesUpdate'),
                                 uiOutput('urlUpdate'),
                                 uiOutput('TribOfOriginUpdate'),
                                 uiOutput('DisputeTypeUpdate'),
                                 uiOutput('DispGeneralUpdate'),
                                 uiOutput('FileNameUpdate'),
                                 uiOutput('duplicateUpdate'),
                                 uiOutput('WithdrawnUpdate'),
                                 uiOutput('updateButton')
                          )
                        )
                        
                      )
                    )
                    
           ), 
           
           ########################################################
           #Visualize tab to display stacked bar plots of variables
           tabPanel("Visualize Data",
                    sidebarLayout(
                      #Title over the select inputs
                      sidebarPanel(p("To make a variable available for plotting, click the variable under 'Display Columns' in the Query Data tab."),
                        br(),
                        checkboxInput(inputId = "sideBySide", label = code("Switch to grouped bar graph from stacked bar graph"), value = FALSE),
                        #Generic uiOutputs that will hold the variable names to be visualized
                        uiOutput(outputId = 'var1Filter'),
                        uiOutput(outputId = 'var2Filter')),
                      mainPanel(
                        #Plot to be displayed
                        plotOutput('plot1')
                      )
                    )
           )
)
