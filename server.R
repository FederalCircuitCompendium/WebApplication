library(shiny)
library(RSQLite)  # for dbConnect, dbListFields, dbGetQuery (RSQlite imports DBI for these)
library(shinythemes)
library(ggplot2)
library(reshape2)

# custom function for SQL sanitizing 
# apostrophe in string field input:
# mimics dplyr::escape
sql_escape<-function (x) {
  first <- gsub("'","''",x)
  sql_input<- paste0("'",noquote(first),"'")
  return(noquote(sql_input))
}

#Connecting to the SQL database called
#'appearls.sqlite in shiny app folder
con <- dbConnect(SQLite(), dbname = "appeals.sqlite")

#Uses the fields stated in SQL database 
#to auto-generate a list of column names.
#The auto-names will be customized to be shown 
#in the 'Display' tab of the "Query Data"
#and for the 'appeals' table shown in the shiny-app
appealsFields <- as.list(dbListFields(con, "appeals"));

#Assigning custom names to the list for display 
#purposes, slightly easier to read with spaces, etc.
#Underlying list entries not changed, just their names
######################################################
#Custom names follows:
names(appealsFields) = c('ID', 'Case Date', 'Year', 'Origin', 'Case Name', 'Precedential Status', 'Duplicate', 'Appeal Number', 'Document Type', 'EnBanc', 'Judge 1', 'Judge 2', 'Judge 3', 'Opinion 1', 'Opinion 1 Author', 'Opinion 2', 'Opinion 2 Author', 'Opinion 3', 'Opinion 3 Author', 'Notes', 'URL', 'Tribunal of Origin', 'Dispute Type', 'Disposition General','File Name');

#Selecting the earliest case date from the database 
#for the dateRangeInput... dbGetQuery from DBI package
minCaseDate <- dbGetQuery(con, "SELECT min(caseDate) FROM appeals")[,1]



function(input, output, session) {
    
    # Each 'uniqueXXXXX' reactive value is created so anytime a new
    # record is added (via Insert Data tab), it will update the
    # available items on the drop-downlist in the filter on the
    # Query Data tab, adding new ones if necessary
    
    # ** DETERMINE DROP-DOWN CHOICES FOR FILTERING **
    # Certain variables (9 of them below) are part of dropdown list
    # for filtering and this potentially needs to be updated with
    # newly added records, this is reactive
    # NOTE: SELECT DISTINCT in SQL is similar to 'unique()' in R
    
    uniqueYear <- reactive({
        input$insert
        dbGetQuery(con, "SELECT DISTINCT(year) FROM appeals")[,1]
    })
    
    uniqueOrigin <- reactive({
        input$insert
        dbGetQuery(con, "SELECT DISTINCT(origin) FROM appeals")[,1]
    })
    
    uniquePrecStatType <- reactive({
        input$insert
        dbGetQuery(con, "SELECT DISTINCT(type) FROM appeals")[,1]
    })
    
    uniqueDocType <- reactive({
        input$insert
        dbGetQuery(con, "SELECT DISTINCT(docType) FROM appeals")[,1]
    })
    
    uniqueEnBanc <- reactive({
        input$insert
        dbGetQuery(con, "SELECT DISTINCT(enBanc) FROM appeals")[,1]
    })
    
    uniqueOpinion1 <- reactive({
        input$insert
        dbGetQuery(con, "SELECT DISTINCT(opinion1) FROM appeals")[,1]
    })
    
    uniqueTribOfOrigin <- reactive({
      input$insert
      dbGetQuery(con, "SELECT DISTINCT(TribOfOrigin) FROM appeals")[,1]
    })
    
    uniqueDisputeType <- reactive({
      input$insert
      dbGetQuery(con, "SELECT DISTINCT(DisputeType) FROM appeals")[,1]
    })
    
    uniqueDispGeneral <- reactive({
      input$insert
      dbGetQuery(con, "SELECT DISTINCT(DispGeneral) FROM appeals")[,1]
    })
    
    uniqueDuplicate <- reactive({
      input$insert
      dbGetQuery(con, "SELECT DISTINCT(duplicate) FROM appeals")[,1]
    })
    
    # The above 9 vectors now hold the updated drop-down lists
    # for the filter items on Query Data tab.
    
    # Each time a record is inserted, the next unique ID in line 
    # is ready to go.  Relevant for Insert Data tab.
    nextID <- reactive({ 
        input$insert
        dbGetQuery(con, "SELECT max(uniqueID) FROM appeals")[,1] + 1;
    })
  
    # ** USER-SELECTED CHOICES FOR FILTERING **
    # Creating filters of factors that allow multiple levels to be selected,
    # levels are determined above and are reactive.
    # The 'selected' option default is NULL, which is all levels when multiple=TRUE.
    output$duplicateFilter <- renderUI(selectInput('duplicateInput', 'Duplicate:', choices = uniqueDuplicate(), multiple = TRUE))
    
    output$originFilter <- renderUI(selectInput('originInput', 'Origin:', choices = uniqueOrigin(), multiple = TRUE))
    
    output$typeFilter <- renderUI(selectInput("typeInput", 'Precedential Status:', choices = uniquePrecStatType(), multiple = TRUE))
    
    output$docTypeFilter <- renderUI(selectInput('docTypeInput', 'Document Type:', choices = uniqueDocType(), multiple = TRUE))
    
    output$enBancFilter <- renderUI(selectInput('enBancInput', 'enBanc:', choices = uniqueEnBanc(), multiple = TRUE))
    
    output$opinion1Filter <- renderUI(selectInput('opinion1Input', 'Opinion 1:', choices = uniqueOpinion1(), multiple = TRUE))
    
    output$yearFilter <- renderUI(selectInput('yearInput', 'Year:', choices = uniqueYear(), multiple = TRUE))
    
    output$TribOfOriginFilter <- renderUI(selectInput('TribOfOriginInput', 'Tribunal of Origin:', choices = uniqueTribOfOrigin(), multiple = TRUE))
    
    output$DisputeTypeFilter <- renderUI(selectInput('DisputeTypeInput', 'Dispute Type:', choices = uniqueDisputeType(), multiple = TRUE))
    
    output$DispGeneralFilter <- renderUI(selectInput('DispGeneralInput', 'Disposition General:', choices = uniqueDispGeneral(), multiple = TRUE))
    

    
    
    # ** USER-SELECTED DISPLAY CHOICES **
    # Rendering checkboxGroupInput to allow user to display any columns.
    # Four boxes are chosen by default.
    output$display <- renderUI(checkboxGroupInput('show_vars', 'Display Columns:', choices = appealsFields,
                                                  selected = appealsFields[c(1,2,4,5)]));
    
    # ** USER-SELECTED DATE RANGE **
    # Rendering dateRangeInput, a 2-element vector, to allow the user to
    # query the database for any range.  The 'end' option default is NULL 
    # which gives today's date as end date.
    output$dateRange <- renderUI(dateRangeInput(inputId = "dateRange", label = "Select Date Range: yyyy-mm-dd", start = minCaseDate));
    
    

    
    # ** CREATE SUBSETTED DATA SET BASED ON FILTERING **
    #Reactive value holding the current date frame depending on the chosen date range, or any other filter
    current_frame <- reactive({
        # The data frame is initially selected between the chosen date range.
        # Dates are drop-down choices, so sanitizing is not needed.
        query <- paste("SELECT * FROM appeals WHERE caseDate BETWEEN '", input$dateRange[1],"' AND '",input$dateRange[2],"'", sep = "");
        tempData <- dbGetQuery(con, query);
        # 'tempData' is a data.frame structure
        
        # All other filtering is R-based, not SQLite based.
        # Subsetting by the desired orgin levels on the 'Filter' tab
        if(!is.null(input$originInput)) {
            tempData <- subset(tempData, tempData$origin %in% input$originInput);
        }
        
        #Subsetting by the desired type levels on the 'Filter' tab
        if(!is.null(input$typeInput)) {
            tempData <- subset(tempData, tempData$type %in% input$typeInput);
        }
        
        #Subsetting by the desired document type levels on the 'Filter' tab
        if(!is.null(input$docTypeInput)) {
            tempData <- subset(tempData, tempData$docType %in% input$docTypeInput);
        }
        
        ##Subsetting by the desired enBanc levels on the 'Filter' tab
        if(!is.null(input$enBancInput)) {
            tempData <- subset(tempData, tempData$enBanc %in% input$enBancInput);
        }
        
        #Subsetting by the desired opinion 1 levels on the 'Filter' tab
        if(!is.null(input$opinion1Input)) {
            tempData <- subset(tempData, tempData$opinion1 %in% input$opinion1Input);
        }
        
        #Subsetting by the desired years on the 'Filter' tab
        if(!is.null(input$yearInput)) {
            tempData <- subset(tempData, tempData$year %in% input$yearInput);
        }
        
        #Subsetting by the desired tribunal of origin on the 'Filter' tab
        if(!is.null(input$TribOfOriginInput)) {
          tempData <- subset(tempData, tempData$TribOfOrigin %in% input$TribOfOriginInput);
        }
       
        #Subsetting by the desired dispute type on the 'Filter' tab
        if(!is.null(input$DisputeTypeInput)) {
          tempData <- subset(tempData, tempData$DisputeType %in% input$DisputeTypeInput);
        } 
        
        #Subsetting by the desired disposition general on the 'Filter' tab
        if(!is.null(input$DispGeneralInput)) {
          tempData <- subset(tempData, tempData$DispGeneral %in% input$DispGeneralInput);
        }  

        #Subsetting by the desired duplicate value on the 'Filter' tab
        if(!is.null(input$duplicateInput)) {
          tempData <- subset(tempData, tempData$duplicate %in% input$duplicateInput);
        }  
        
        #The resulting data frame then contains the desired subset, containing only the columns selected in the 'Display' tab
        tempData[, input$show_vars, drop = FALSE]
    })
    
    #Reactive value holding the indices of the variables to be displayed based on the checkboxGroupInput
    current_labels <- reactive({
        which(appealsFields %in% input$show_vars); 
    })
    
    #Rendering the data table for the current data frame, and current labels
    output$mytable1 <- DT::renderDataTable({
        DT::datatable(current_frame(), colnames = names(appealsFields)[current_labels()], rownames = FALSE);
    })
    
    
    # ** INSERT A NEW RECORD **
    # The action button (input$insert) has a value which is the number of times it's clicked.
    # We count the number of inserts in a given session made by the user (Insert tab)
    output$text <- renderText({
        paste("Session Insertion Count:", input$insert);
    })
    
    # When the insert button in clicked, the SQL statement is generated and sent to the database
    # https://shiny.rstudio.com/articles/dynamic-ui.html
    # Example of SQLite command with 3 fields: ID (integer), string1, string2, ...
    #   "INSERT INTO appeals VALUES (13545,'my field entry', 'married')"
    # Special characters must be sanitized when sending the sQL command (apostrophe)
    observeEvent(input$insert, {
        #Grabbing each value supplied by the user from the text boxes in the 'Insert' tab
        query <- paste("INSERT INTO appeals VALUES (", 
                       nextID(),",'",
                       input$caseDate,"','",
                       format(as.Date(input$caseDate), "%Y"),"','",
                       input$origin,"',",
                       sql_escape(input$caseName),",'",
                       input$type,"','",
                       input$duplicate,"','",
                       input$appealNumber,"','",
                       input$docType,"','",
                       input$enBanc,"',",
                       sql_escape(input$judge1),",",
                       sql_escape(input$judge2),",",
                       sql_escape(input$judge3),",",
                       sql_escape(input$opinion1),",",
                       sql_escape(input$opinion1Author),",",
                       sql_escape(input$opinion2),",",
                       sql_escape(input$opinion2Author),",",
                       sql_escape(input$opinion3),",",
                       sql_escape(input$opinion3Author),",",
                       sql_escape(input$notes),",'",
                       input$url,"','",
                       input$TribOfOrigin,"','",
                       input$DisputeType,"','",
                       input$DispGeneral,"','",
                       input$FileName,"')",   sep="")
        
        #The line below sends insert statement to database to insert the record
        dbGetQuery(con, query)
        
        #After the insert, the text fields are cleared
        updateTextInput(session, inputId = "caseDate", value = Sys.Date())
        updateTextInput(session, inputId = "origin", value = "")
        updateTextInput(session, inputId = "caseName", value = "")
        updateTextInput(session, inputId = "type", value = "")
        updateTextInput(session, inputId = "appealNumber", value = "")
        updateTextInput(session, inputId = "docType", value = "")
        updateTextInput(session, inputId = "enBanc", value = "")
        updateTextInput(session, inputId = "judge1", value = "")
        updateTextInput(session, inputId = "judge2", value = "")
        updateTextInput(session, inputId = "judge3", value = "")
        updateTextInput(session, inputId = "opinion1", value = "")
        updateTextInput(session, inputId = "opinion1Author", value = "")
        updateTextInput(session, inputId = "opinion2", value = "")
        updateTextInput(session, inputId = "opinion2Author", value = "")
        updateTextInput(session, inputId = "opinion3", value = "")
        updateTextInput(session, inputId = "opinion3Author", value = "")
        updateTextInput(session, inputId = "duplicate", value = "No")
        updateTextInput(session, inputId = "notes", value = "")
        updateTextInput(session, inputId = "url", value = "")
        updateTextInput(session, inputId = "TribOfOrigin", value = "")
        updateTextInput(session, inputId = "DisputeType", value = "")
        updateTextInput(session, inputId = "DispGeneral", value = "")
        updateTextInput(session, inputId = "FileName", value = "") 
        output$ID <- renderText({paste("Record Inserted:", nextID())})
    })
    
    # ** UPDATE A RECORD **
    #Pulls the existing record from the database after the user supplies a unique ID and clicks 'Get Record' (Update page)
    observeEvent(input$getRecord, {
        
        #Selects the entire record from the database whose unique ID matches the one supplied
        record <- dbGetQuery(con, paste0("SELECT * FROM appeals WHERE uniqueID = ", input$updateID))
        
        #If the user supplies an invalid ID, a message is printed
        if(length(record$uniqueID) == 0){
            output$IDNotFound <- renderText({isolate(paste0("ID ",input$updateID, " not found."))})
        } else {
            
            #Gets rid of the invalid ID message
            output$IDNotFound <- renderText({""})
            
            #If a valid ID has been supplied, text boxes for each record are displayed with the current record populating the field
            output$caseDateUpdate <- renderUI(textInput('caseDateUpdate', "Case Date", record$caseDate))
            output$originUpdate <- renderUI(textInput('originUpdate', "Origin", record$origin))
            output$yearUpdate <- renderUI(textInput('yearUpdate', "Year", record$year))
            output$caseNameUpdate <- renderUI(textInput('caseNameUpdate', "Case Name", record$caseName))
            output$typeUpdate <- renderUI(textInput('typeUpdate', "Precedential Status", record$type))
            output$appealNumberUpdate <- renderUI(textInput('appealNumberUpdate', "Appeal Number", record$appealNumber))
            output$docTypeUpdate <- renderUI(textInput('docTypeUpdate', "Document Type", record$docType))
            output$enBancUpdate <- renderUI(textInput('enBancUpdate', "En Banc", record$enBanc))
            output$judge1Update <- renderUI(textInput('judge1Update', "Judge 1", record$judge1))
            output$judge2Update <- renderUI(textInput('judge2Update', "Judge 2", record$judge2))
            output$judge3Update <- renderUI(textInput('judge3Update', "Judge 3", record$judge3))
            output$opinion1Update <- renderUI(textInput('opinion1Update', "Opinion 1", record$opinion1))
            output$opinion1AuthorUpdate <- renderUI(textInput('opinion1AuthorUpdate', "Opinion 1 Author", record$opinion1Author))
            output$opinion2Update <- renderUI(textInput('opinion2Update', "Opinion 2", record$opinion2))
            output$opinion2AuthorUpdate <- renderUI(textInput('opinion2AuthorUpdate', "Opinion 2 Author", record$opinion2Author))
            output$opinion3Update <- renderUI(textInput('opinion3Update', "Opinion 3", record$opinion3))
            output$opinion3AuthorUpdate <- renderUI(textInput('opinion3AuthorUpdate', "Opinion 3 Author", record$opinion3Author))
            output$notesUpdate <- renderUI(textInput('notesUpdate', "Notes", record$notes))
            output$urlUpdate <- renderUI(textInput('urlUpdate', "URL", record$url))
            output$TribOfOriginUpdate <- renderUI(textInput('TribOfOriginUpdate', "Tribunal of Origin", record$TribOfOrigin))
            output$DisputeTypeUpdate <- renderUI(textInput('DisputeTypeUpdate', "Dispute Type", record$DisputeType))
            output$DispGeneralUpdate <- renderUI(textInput('DispGeneralUpdate', "Disposition General", record$DispGeneral))
            output$FileNameUpdate <- renderUI(textInput('FileNameUpdate', "File Name", record$FileName))
            output$duplicateUpdate <- renderUI(textInput('duplicateUpdate', "Duplicate", record$duplicate))
            output$SQLcheckButton <- renderUI(actionButton('SQLcheckButton', "Debugging - Check SQL command"))
            output$SQLcommandUpdate <- renderText({""})
            output$updateButton <- renderUI(actionButton('updateButton', "Update"))
            
        }
    }
    ) #observeEvent complete, fields populated with old information
    
    
    #After the record has been pulled from the data, the user edits any field, and then presses 'Check SQL'
    observeEvent(input$SQLcheckButton, {
      #Once the button is pushed, a SQL statement is again generated which pushes the edited fields back to the database
      #Special characters must be escaped when sending the SQL command (e.g. apostrophe)
      query <- paste0("UPDATE appeals SET caseDate = '", input$caseDateUpdate,
                      "', year = '", format(as.Date(input$caseDateUpdate), "%Y"),
                      "', origin = '", input$originUpdate,
                      "', caseName = ", sql_escape(input$caseNameUpdate),
                      ", type = '", input$typeUpdate,
                      "', duplicate = '", input$duplicateUpdate,
                      "', appealNumber = '", input$appealNumberUpdate,
                      "', docType = '", input$docTypeUpdate,
                      "', enBanc = '", input$enBancUpdate,
                      "', judge1 = ", sql_escape(input$judge1Update),
                      ", judge2 = ", sql_escape(input$judge2Update),
                      ", judge3 = ", sql_escape(input$judge3Update),
                      ", opinion1 = ", sql_escape(input$opinion1Update),
                      ", opinion1Author = ", sql_escape(input$opinion1AuthorUpdate),
                      ", opinion2 = ", sql_escape(input$opinion2Update),
                      ", opinion2Author = ", sql_escape(input$opinion2AuthorUpdate),
                      ", opinion3 = ", sql_escape(input$opinion3Update),
                      ", opinion3Author = ", sql_escape(input$opinion3AuthorUpdate),
                      ", notes = ", sql_escape(input$notesUpdate),
                      ", url = ", sql_escape(input$urlUpdate),
                      ", TribOfOrigin = ", sql_escape(input$TribOfOriginUpdate),
                      ", DisputeType = ", sql_escape(input$DisputeTypeUpdate),
                      ", DispGeneral = ", sql_escape(input$DispGeneralUpdate),
                      ", FileName = ", sql_escape(input$FileNameUpdate),
                      " WHERE uniqueID = ", input$updateID)
      
      #Sends the SQL statement to 'SQLcommandUpdate' 
      # for verification once the string has been generated
      output$SQLcommandUpdate <- renderText({query})
      
    }
    )
    
    
    
    
    #After the record has been pulled from the data, the user edits any field, and then presses 'Update Record'
    observeEvent(input$updateButton, {
        #Once the button is pushed, a SQL statement is again generated which pushes the edited fields back to the database
        #Special characters must be escaped when sending the SQL command (e.g. apostrophe)
        query <- paste0("UPDATE appeals SET caseDate = '", input$caseDateUpdate,
                        "', year = '", format(as.Date(input$caseDateUpdate), "%Y"),
                        "', origin = '", input$originUpdate,
                        "', caseName = ", sql_escape(input$caseNameUpdate),
                        ", type = '", input$typeUpdate,
                        "', duplicate = '", input$duplicateUpdate,
                        "', appealNumber = '", input$appealNumberUpdate,
                        "', docType = '", input$docTypeUpdate,
                        "', enBanc = '", input$enBancUpdate,
                        "', judge1 = ", sql_escape(input$judge1Update),
                        ", judge2 = ", sql_escape(input$judge2Update),
                        ", judge3 = ", sql_escape(input$judge3Update),
                        ", opinion1 = ", sql_escape(input$opinion1Update),
                        ", opinion1Author = ", sql_escape(input$opinion1AuthorUpdate),
                        ", opinion2 = ", sql_escape(input$opinion2Update),
                        ", opinion2Author = ", sql_escape(input$opinion2AuthorUpdate),
                        ", opinion3 = ", sql_escape(input$opinion3Update),
                        ", opinion3Author = ", sql_escape(input$opinion3AuthorUpdate),
                        ", notes = ", sql_escape(input$notesUpdate),
                        ", url = ", sql_escape(input$urlUpdate),
                        ", TribOfOrigin = ", sql_escape(input$TribOfOriginUpdate),
                        ", DisputeType = ", sql_escape(input$DisputeTypeUpdate),
                        ", DispGeneral = ", sql_escape(input$DispGeneralUpdate),
                        ", FileName = ", sql_escape(input$FileNameUpdate),
                        " WHERE uniqueID = ", input$updateID)
        
        #Sends the 'Update' statement once the string has been generated
        dbGetQuery(con, query)              
       
        #After updating a record, each field is cleared, 
        updateTextInput(session, inputId = "caseDateUpdate", value = "")
        updateTextInput(session, inputId = "originUpdate", value = "")
        updateTextInput(session, inputId = "yearUpdate", value = "")
        updateTextInput(session, inputId = "caseNameUpdate", value = "")
        updateTextInput(session, inputId = "typeUpdate", value = "")
        updateTextInput(session, inputId = "appealNumberUpdate", value = "")
        updateTextInput(session, inputId = "docTypeUpdate", value = "")
        updateTextInput(session, inputId = "enBancUpdate", value = "")
        updateTextInput(session, inputId = "judge1Update", value = "")
        updateTextInput(session, inputId = "judge2Update", value = "")
        updateTextInput(session, inputId = "judge3Update", value = "")
        updateTextInput(session, inputId = "opinion1Update", value = "")
        updateTextInput(session, inputId = "opinion1AuthorUpdate", value = "")
        updateTextInput(session, inputId = "opinion2Update", value = "")
        updateTextInput(session, inputId = "opinion2AuthorUpdate", value = "")
        updateTextInput(session, inputId = "opinion3Update", value = "")
        updateTextInput(session, inputId = "opinion3AuthorUpdate", value = "")
        updateTextInput(session, inputId = "duplicateUpdate", value = "")
        updateTextInput(session, inputId = "notesUpdate", value = "")
        updateTextInput(session, inputId = "urlUpdate", value = "")
        updateTextInput(session, inputId = "TribOfOriginUpdate", value = "")
        updateTextInput(session, inputId = "DisputeTypeUpdate", value = "")
        updateTextInput(session, inputId = "DispGeneralUpdate", value = "")
        updateTextInput(session, inputId = "FileNameUpdate", value = "")
        output$IDNotFound <- renderText({isolate(paste0("Record ", input$updateID, " updated."))})
    }
    )

    
    # ** LIST VARIABLES AVAILABLE TO USER FOR PLOTTING **
    # Excluding variables that don't make sense to be visualized,
    # i.e. should not be available for user for plotting
    uniqueVariables <- reactive({
      uniqueVariables <- names(current_frame()) # in terms of true field names
      names(uniqueVariables) <- names(appealsFields)[current_labels()]
      ignore <- c("uniqueID", "caseDate", "caseName", "appealNumber", "notes", "url","FileName")
      ignInd <- which(uniqueVariables %in% ignore)
      uniqueVariables[-ignInd]
    })

    
    #Rendering variable selection filters on the 'Visualize' tab
    output$var1Filter <- renderUI(selectInput('var1Input', 'Choose variable  for horizontal axis:',choices = uniqueVariables(), multiple = FALSE))
    output$var2Filter <- renderUI(selectInput('var2Input', 'Choose segmenting variable:',choices = uniqueVariables(), multiple = FALSE))
    
    #This reactive data frame is for plotting in the 'Visualize' tab
    selectedData <- reactive({
        #Validating that the two desired variables have been selected before trying to plot. This avoids warning messages
            #being displayed in the R console
        validate(
            need(input$var1Input, ''),
            need(input$var2Input, '')
        )
        #If none are selected, return an empty plot
        if(input$var1Input == "" && input$var2Input == "") {
            x <- NA
            y <- NA
            z <- NA
            data.frame(x, y, z)
        }
        #Otherwise the available plotting frame is returned
        else {
            current_frame()[,c(input$var1Input, input$var2Input)]
        }
    })
    
    # Plotting the available data determined as 'selectedData'
    # These are assumed to be categorical in nature
    output$plot1 <- renderPlot({
        # Rearranging the data to be suitable for ggplot2 geom_bar plot
        # Originally a nx2 object with column a,b then melted to a/b combination counts
        data <- melt(table(selectedData()))
        
        #Displays a bar plot for the selected data, where variable 1 has a bar for each level
            #and each of those are filled with the levels of variable 2.
            #Counts are then displayed
        if (input$sideBySide){
        ggplot(data, aes(x = data[,1], y = data[,3], fill = factor(data[,2]))) + 
            geom_bar(position="dodge",stat = 'identity') + xlab(names(data)[1]) + ylab("Count") + 
            scale_fill_discrete(name = names(data)[2])
        }else{
          ggplot(data, aes(x = data[,1], y = data[,3], fill = factor(data[,2]))) + 
          geom_bar(stat = 'identity') + xlab(names(data)[1]) + ylab("Count") + 
          scale_fill_discrete(name = names(data)[2])          
        }
    })
    
    #After customizing a data set in the 'Query' tab, the Download button will save the desired data frame as csv
    output$download <- downloadHandler(
        filename = "data.csv", 
        content = function(file) {
            write.csv(current_frame(), file, row.names = FALSE)
        }, 
        contentType = "text/csv"
    )
}
