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
                                   uiOutput(outputId = 'DispGeneralFilter')
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
           
           ##############################
           #Tab for inserting new records
           tabPanel("Insert Data", 
                    mainPanel(wellPanel(
                      fluidRow(
                        column(6,
                               #Creating textinputs for all columns in the database
                               #Autopopulate the case date to today
                               textInput('caseDate', "Enter Case Date:", Sys.Date()), 
                               textInput('origin', "Enter Origin:",""), 
                               textInput('caseName', "Enter Case Name:",""), 
                               textInput('type', "Enter Precedential Status:",""), 
                               textInput('appealNumber', "Enter Appeal Number:",""), 
                               textInput('docType', "Enter Document Type:",""), 
                               textInput('enBanc', "Enter EnBanc:",""), 
                               textInput('judge1', "Enter Judge 1:",""), 
                               textInput('judge2', "Enter Judge 2:",""), 
                               textInput('judge3', "Enter Judge 3:",""),
                               textInput('opinion1', "Enter Opinion 1:",""),
                               textInput('opinion1Author', "Enter Opinion 1 Author:","")                               
                        ),
                        column(6,
                               #Continue placing text boxes on insert page
                               textInput('opinion2', "Enter Opinion 2:", ""),
                               textInput('opinion2Author', "Enter Opinion 2 Author:",""),
                               textInput('opinion3', "Enter Opinion 3:", ""),
                               textInput('opinion3Author', "Enter Opinion 3 Author:",""),
                               textInput('duplicate', "Duplicate?:", "No"),
                               textInput('notes', "Enter Notes:", ""),
                               textInput('url', "Enter URL:",""),
                               textInput('TribOfOrigin', "Enter Tribunal of Origin:",""),
                               textInput('DisputeType', "Enter Dispute Type:",""),
                               textInput('DispGeneral', "Enter Disposition General:",""),
                               textInput('FileName', "Enter File Name:","")
                        )
                      )
                    )
                    
                    ),
                    
                    #Text output which is used to display the uniqueID that was inserted by the application for a new record
                    textOutput('ID'),
                    br(),
                    
                    #Used to display the session insertion count
                    textOutput("text"),
                    br(),
                    
                    #Button to insert new record
                    actionButton("insert","Insert")
           ),
           
           ##########################################
           #Creating tab to update an existing record
           tabPanel("Update Record",
                    mainPanel(
                      wellPanel(
                        fluidRow(
                          column(6,
                                 #Text box to enter desired record ID to update
                                 textInput('updateID', "Enter ID:", "")
                          ),
                          column(6,
                                 br(),
                                 #Button to retreieve the record for the supplied ID
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
                                 uiOutput('SQLcheckButton'),
                                 textOutput('SQLcommandUpdate'),
                                 br(),
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
