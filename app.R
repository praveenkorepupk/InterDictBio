## InterDictBio R Shiny Application Development

library(shinydashboard)
library(shiny)
library(mongolite)
library(dplyr)
library(glue)
library(reactable)
library(DT)
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(tidyr)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(ddpcr)
library(shinymanager)
library(scrypt)
library(shinyBS)
library(spsComps)


inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"



# # data.frame with credentials info
credentials <- data.frame(
  user = c("praveen", "pk"), # mandatory
  password = c("pk@123", "pk@123"), # mandatory
  start = c("2015-04-15"), # optinal (all others)
  expire = c(NA, "2032-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single 'Shiny' applications.",
  stringsAsFactors = FALSE,
  moreInfo = c("someData1", "someData2"),
  level = c(2, 0)
)

css <- HTML(".btn-primary {
                  color: #ffffff;
                  background-color: #0dc5c1;
                  border-color: #0dc5c1;
              }
              .panel-primary {
                  border-color: #0dc5c1;
              }")

# In global.R for example:
set_labels(
  language = "en",
  "Please authenticate" = "Please Login",
  "Username:" = "Username:",
  "Password:" = "Password:",
  "Add a user" = "Add a user"
)

ui <- secure_app(head_auth = tags$script(inactivity),
                 theme = shinythemes::shinytheme("united"),
                 background  = "url('1_1080_2.png');",
                 
                 tags_top = tags$div(
                   tags$head(tags$style(css)),
                   tags$img(
                     src = "output-onlinepngtools.png", width = 200, height = 30, alt="Logo not found", deleteFile=FALSE
                   )),
                 dashboardPage(
                   dashboardHeader(title = tags$a(tags$img(height = "25px",src="output-onlinepngtools.png")),
                                   tags$li(class="dropdown",tags$a("Help", target="_blank")),
                                   tags$li(class="dropdown",tags$a("User", target="_blank"))),
                   
                   dashboardSidebar(
                     tags$style(HTML('
    .selectize-input {white-space: nowrap}
    #Sequence+ div>.selectize-dropdown{width: 210px !important; font-style: italic; font-weight: bold; color: green;}
    #Sequence+ div>.selectize-input{width: 210px !important; font-style: italic; font-weight: bold; color: green; margin-bottom: -10px;}
                            ')),
                     width = 250,
                     selectizeInput("Sequence", h4("Enter the Sequence:"),
                                    multiple = TRUE,
                                    choices = NULL),
                     column(12, actionButton("search", 
                                             "Filter by Sequence(s)",
                                             width='95%',
                                             class = "btn-primary",
                                             icon = icon("search", "fa-0.5x"),
                                             style='color: #fff; 
                                                            white-space:normal;
                                                            background-color: #337ab7; 
                                                            border-color: #2e6da4";
                                                            padding:4px; 
                                                            font-size:120%;
                                                            text-align:left; 
                                                            margin-left:2px;')
                            ,align = "left",
                            bsTooltip("search","Filter data by multiple Sequences", "bottom"),
                            br()
                     ),
                     fluidRow(
                       column(12,
                              br(),
                              br(),
                              # textOutput("rendertext"),
                              actionButton('addFilter', 'Find overlaps',width='85%', 
                                           class = "btn-primary",
                                           icon = icon("plus-circle"),
                                           style='color: #fff; 
                                                          background-color: #337ab7;
                                                          border-color: #2e6da4";
                                                          padding:4px; 
                                                          font-size:120%;
                                                          text-align:left;
                                                          margin-left:16px;'),
                              bsTooltip("addFilter","To Find the same Entry across multiple sequences","bottom"),
                              align = "left"),
                       
                     ),
                     # tags$hr(),
                     tags$div(id = 'placeholderAddRemFilt'),
                     tags$div(id = 'placeholderFilter'),
                     fluidRow(column(12,br(),br(),br(),br(),uiOutput("interDictUrl")))),
                   
                   dashboardBody(
                     tags$style(HTML("
    .tabbable > .nav > li > a[data-value='Results Summary'] {background-color: #847c8a;   color:white; font-size: 16px;}
    .tabbable > .nav > li > a[data-value='Expanded Sequence Data'] {background-color: #847c8a;  color:white; font-size: 16px;}
    .tabbable > .nav > li > a[data-value='Dashboard'] {background-color: #847c8a; color:white; font-size: 16px;}
    .tabbable > .nav > li > a[data-value='Admin'] {background-color: #847c8a; color:white; font-size: 16px;}
    .tabbable > .nav > li[class=active]    > a {background-color: #0dc5c1; color:white; font-size: 16px; font-style:oblique; font-weight:bold;}

  ")),
                     tabsetPanel(id = 'dataset',
                                 tabPanel("Results Summary",
                                          br(), DT::dataTableOutput("sampleData"),
                                          # shinyjs::useShinyjs(),
                                          # tags$style('removeClass("dtsb-title")'),
                                          # div(id="dtsb-title", "Search"),
                                          column(DT::dataTableOutput("tempdt"), width = 6),
                                          DT::dataTableOutput("data"),
                                          shinyjs::useShinyjs(),
                                          shinyjs::hidden(downloadButton("downloadData", "Download Selected Rows",
                                                                         icon = icon("download"),
                                                                         style="color: #333; margin-left:-700px; 
                                                                         background-color: #FFF; border-color: #333"))),
                                 tabPanel("Expanded Sequence Data", br(),DT::dataTableOutput("data2"),
                                          dashboardSidebar(width = 250,
                                                           fluidRow(style = "padding: 40px 14px 5px 14px; margin: 5px 5px 5px 5px; ",
                                                                    # custom column name
                                                                    textInput(inputId = "nameColumn", "Enter Column Name"),
                                                                    actionButton(inputId = "addColumn", "Create Column")))
                                 ),
                                 tabPanel("Dashboard", fluidRow(br(),
                                                                dashboardSidebar(width = 250),
                                                                valueBoxOutput("value1", width = 3)
                                                                ,valueBoxOutput("value2", width = 3)
                                                                ,valueBoxOutput("value3", width = 3)
                                                                ,valueBoxOutput("value4", width = 3)
                                 ),
                                 fluidRow( 
                                   box(
                                     title = "Sequence Plot"
                                     ,status = "primary"
                                     ,solidHeader = TRUE 
                                     ,collapsible = TRUE 
                                     ,plotOutput("seqencePlot", height = "300px")
                                   ),
                                   box(
                                     title = "Summary Statistics", 
                                     status = "warning", 
                                     solidHeader = TRUE,
                                     width = 6,
                                     collapsible = TRUE,
                                     height = 360,
                                     verbatimTextOutput("summaryDset")
                                   ))
                                 ),
                                 tabPanel(
                                   "Admin",
                                   shinyjs::useShinyjs(),
                                   dashboardSidebar(width = 250),
                                   # add button to add column
                                   radioButtons("select_input", "Choose Row or Column", choices = c("Row", "Column")),
                                   box(id = "rowID",width = 12,
                                       fluidRow(style = "padding: 5px 14px 5px 14px;
                                              margin: 5px 5px 5px 5px; ",                                         
                                                fileInput("file1", "Upload CSV To Add Rows",
                                                          accept = c(
                                                            "text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv")),
                                                actionButton("update", "Combine Rows"),
                                                actionButton("disp1", "Display"),
                                                tableOutput("contents"),
                                                fluidRow(style = "padding:5px 15px 5px 5px; margin: 15px 0px 0px 0px;",
                                                         # downloadButton("rowsTemplate", "Download Template"),
                                                         downloadButton('downLoadFilter',"Download Template")))),
                                   
                                   box(id = "columnID", width = 12,
                                       fluidRow(style = "padding: 5px 14px 5px 14px;
                                          margin: 5px 5px 5px 5px; ",                                         
                                                fileInput("file2", "Upload CSV To Add Columns",
                                                          accept = c(
                                                            "text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv")),
                                                useShinyalert(),
                                                actionButton("update2", "Combine Columns"),
                                                actionButton("disp2", "Display"),
                                                tableOutput("contents2"),
                                                div(style = "margin: 15px 0px 0px 0px; width:100%;"),
                                                downloadButton("columnTemplate", "Download column Template")),
                                       useShinyalert()),
                                   radioButtons("select_mathFunction", "Choose Math Function",
                                                choices = c("Arithmetic Mean", "Geometric Mean", "Addition")),
                                   selectizeInput("numeric_cols", "Select Numeric columns", multiple = TRUE, choices = NULL),
                                   textInput("newcolumnname", "new column name"),
                                   # numericInput("formula", "Multiply", min=0, max=1000, value=1),
                                   actionButton("update3", "Create Math Column"),
                                   DT::DTOutput("data_tbl")
                                 )
                                 # )
                     )
                   ),tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #0dc5c1;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #0dc5c1;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #0dc5c1;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #000000;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #ff0000;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #00ff00;
                              color: #000000;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #283747;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #283747;
                              }
                              '))
                   )
                 ))

server <- function(input, output, session) {
  
  # result_auth <- secure_server(check_credentials = check_credentials(credentials))
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  # Create reactive values including all credentials
  creds_reactive <- reactive({
    reactiveValuesToList(res_auth)
  })
  
  # Hide extraOutput only when condition is TRUE
  observe({
    if (!is.null(creds_reactive()$level) && creds_reactive()$level > 0){
      hideTab("dataset", "Admin")
      # shinyjs::hide("praveen")
    }else{
      showTab("dataset", "Admin")
      # shinyjs::show("praveen")  
    }
  })
  
  
  url <- a(imageOutput("UniProt"),href="https://www.uniprot.org/id-mapping")
  output$interDictUrl <- renderUI({
    tags$a(
      href="https://www.uniprot.org/id-mapping", target="_blank",
      tags$img(src="uniprot_bg_2.png", 
               title="UniProd ID-Mapping", 
               width="180",
               height="60")
    )
  })
  
  output$rendertext <- renderText({
    "To find unique entries for different sequences please click below button"
  })
  
  mon <- mongo(collection = "entries_seperate_rows", db = "interdictbio", url = "mongodb://192.168.204.195:27017",verbose = TRUE)
  monExpandedRows <- mongo(collection = "entries_seperate_rows", db = "interdictbio", url = "mongodb://192.168.204.195:27017",verbose = TRUE)
  # mon <- mongo(collection = "entries_unique_rows", db = "interdictbio", url = "mongodb://192.168.204.195:27017",verbose = TRUE)
  output$sampleData <- renderDataTable(server = TRUE,{
    df100 <- as.data.frame(mon$aggregate('[{"$limit": 1000}]'))
    df100 <- unique(df100[c("EntryName","Entry","ProteinName","GeneNames","Organism","Length","Sequence","Position_List","Count")])
    dTable(df100)
  })
  
  seqQry <- '[{"$group":{"_id":"$Sequence"}},{"$limit":2000}]'
  # seqQry <- '[{"$group":{"_id":"$Sequence"}}]'
  seqList <- mon$aggregate(seqQry)
  updateSelectizeInput(session, "Sequence", choices = c(seqList["_id"]), 
                       # selected = "AAAD" ,
                       server = TRUE)
  query_db <- function(input){
    doc_type <- paste0(input)
    print(doc_type)
    str_vector <- input
    str_json   <- paste0("\"",paste0(unlist(str_vector),collapse= "\",\""),"\"")
    qry <- paste0('{"Sequence":{"$in" : [',str_json,']}}')
    x <- mon$find(query = qry, fields = '{"Position" : 0}')
    # x <- x[!duplicated(x[,c("EntryName", "Position_List","Entry")]), ]
    x <- unique(x[c("EntryName","Entry","ProteinName","GeneNames","Organism","Length","Sequence","Position_List","Count")])
    return(x)
  }
  filtered_data <- eventReactive(input$search, valueExpr = query_db(input$Sequence))
  
  #Function for customize data table which contains custom search builder
  dTable <- function(df){
    datatable(df,selection = 'multiple', editable = TRUE,rownames = FALSE, class = 'cell-border stripe',
              extensions = c("SearchBuilder","Buttons",'ColReorder'), #'Select'
              options = list( initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                "}"),
                columnDefs = list(list(className = 'dt-left', targets = "_all")),
                scrollX = TRUE,
                search = list(regex = TRUE),
                searching = TRUE,
                colReorder = TRUE,
                ordering = TRUE,
                dom = "QpfrBitl",
                buttons =list(
                  I('colvis'), 'copy', 'print',
                  list(
                    extend = 'collection',
                    buttons = list(
                      list(extend = "csv", filename = "page",exportOptions = list(
                        columns = ":visible",modifier = list(page = "current"))
                      ),
                      list(extend = 'excel', filename = "page", title = NULL,
                           exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
                    text = 'Download current page'),
                  list(
                    extend = 'collection',
                    buttons = list(
                      list(extend = "csv", filename = "data",exportOptions = list(
                        columns = ":visible",modifier = list(page = "all"))
                      ),
                      list(extend = 'excel', filename = "data", title = NULL,
                           exportOptions = list(columns = ":visible",modifier = list(page = "all")))),
                    text = 'Download all data')
                  
                ),
                searchBuilder = list(
                  conditions = list(
                    string = list(
                      regex = list(
                        conditionName = "matches regex",
                        init = JS(
                          "function (that, fn, preDefined = null) {",
                          "  var el =  $('<input/>').on('input', function() { fn(that, this) });",
                          "  if (preDefined !== null) {",
                          "     $(el).val(preDefined[0]);",
                          "  }",
                          "  return el;",
                          "}"
                        ),
                        inputValue = JS(
                          "function (el) {",
                          "  return $(el[0]).val();",
                          "}"
                        ),
                        isInputValid = JS(
                          "function (el, that) {",
                          "  return $(el[0]).val().length !== 0;",
                          "}"
                        ),
                        search = JS(
                          "function (value, regex) {",
                          "  var reg = new RegExp(regex, 'g');",
                          "  return reg.test(value);",
                          "}"
                        )
                      )
                    )
                  )
                )
              )
    )
  }
  
  dTable2 <- function(df){
    datatable(df,selection = 'multiple', editable = TRUE,rownames = FALSE, class = 'cell-border stripe',
              extensions = c("SearchBuilder","Buttons",'ColReorder'), #'Select'
              options = list(  initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                "}"),
                columnDefs = list(list(className = 'dt-left', targets = "_all")),
                scrollX = TRUE,
                search = list(regex = TRUE),
                searching = TRUE,
                colReorder = TRUE,
                ordering = TRUE,
                dom = "fQlrtip",
                searchBuilder = list(
                  conditions = list(
                    string = list(
                      regex = list(
                        conditionName = "matches regex",
                        init = JS(
                          "function (that, fn, preDefined = null) {",
                          "  var el =  $('<input/>').on('input', function() { fn(that, this) });",
                          "  if (preDefined !== null) {",
                          "     $(el).val(preDefined[0]);",
                          "  }",
                          "  return el;",
                          "}"
                        ),
                        inputValue = JS(
                          "function (el) {",
                          "  return $(el[0]).val();",
                          "}"
                        ),
                        isInputValid = JS(
                          "function (el, that) {",
                          "  return $(el[0]).val().length !== 0;",
                          "}"
                        ),
                        search = JS(
                          "function (value, regex) {",
                          "  var reg = new RegExp(regex, 'g');",
                          "  return reg.test(value);",
                          "}"
                        )
                      )
                    )
                  )
                )
              )
    )
  }  
  
  cmdMongoDb<- function(df, columnName){
    str_json_2 <- paste0('"',columnName,'":{"$in" : ',jsonlite::toJSON(df[[columnName]]),'}')
    return(str_json_2)
  }
  
  cmdUpdate <- function(df, columnName){
    str_json_2 <- paste0('"',"$set",'":{', '"',columnName, '"', ':' ,jsonlite::toJSON(df[columnName][!(is.na(df[[columnName]])),]),'}')
    return(str_json_2)
  }
  
  dbColumnUpdate <- function(df){
    colsList1 <<- c("Entry", "Sequence","Position", "EntryName","ProteinName","GeneNames","Organism","Length","Count")
    colsList2 <<- names(df)
    diffColName <<- setdiff(colsList2, colsList1)
    for(j in 1:dim(df)[1]){
      # print(j)
      qry1 <- paste0('{',cmdMongoDb(df[j,], "Entry"),",",cmdMongoDb(df[j,], "Sequence"), ",", cmdMongoDb(df[j,], "Position"),'}')
      # print(qry1)
      for(i in diffColName){
        qry2 <- paste0('{',cmdUpdate(df[j,], i),'}')
        # print(qry2)
        monExpandedRows$update(query = qry1, update=qry2)
      }
    }
  }
  
  dbRowUpdate <- function(df){
    qry1 <- paste0('{',cmdMongoDb(df, "Entry"),",",cmdMongoDb(df, "Sequence"), ",", cmdMongoDb(df, "Position"),'}')
    if(dim(monExpandedRows$find(query = qry1))[1]>0){
      colsLs1 <- c("Entry", "Sequence","Position")
      colsLs2 <- names(df)
      diffColName2 <- setdiff(colsLs2, colsLs1)
      for(i in diffColName2){
        qry2 <- paste0('{',cmdUpdate(df, i),'}')
        monExpandedRows$update(query = qry1, update=qry2)
      }
    }else{
      monExpandedRows$insert(rjson::toJSON(df))
    }
  }
  
  ################################################################   
  ##################### Main TabPanel  ########################### 
  ################################################################   
  
  data = reactive({filtered_data()})
  # set up reactive value
  reactiveData = reactiveVal()
  
  # observe data
  observeEvent(data(),{
    reactiveData(data())
  })
  

  proxy <- dataTableProxy('sampleData')
  observeEvent(input$search,{
    replaceData(proxy, reactiveData(), resetPaging = TRUE)
    output$sampleData <- renderDataTable(server = FALSE,{
      dataSet <- reactiveData()
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste0(gsub(" ","_", gsub(":","\t", Sys.time())),".tsv")
        },
        content = function(file) {
          write.table(dataSet[input$sampleData_rows_selected,], file, row.names = FALSE)
        }
      )
      
      observeEvent(input$sampleData_rows_selected, {
        if (input$sampleData_rows_selected == "")
          shinyjs::hide("downloadData")
        else
          shinyjs::show("downloadData")
      })
      dTable(dataSet)
    },class = "display")
    
  }
  )
  
  dff <- eventReactive(input$addFilter,{
    datac <- filtered_data()
    datac <-  datac %>%
      group_by(EntryName) %>%
      filter(n_distinct(Sequence) > 1) %>%
      ungroup
  })
  
  filter <- character(0)
  makeReactiveBinding("aggregFilterObserver")
  aggregFilterObserver <- list()
  observeEvent(input$addFilter, {
    add <- input$addFilter
    filterId <- paste0('Filter_', add)
    colfilterId <- paste0('Col_Filter_', add)
    rowfilterId <- paste0('Row_Filter_', add)
    removeFilterId <- paste0('Remove_Filter_', add)
    insertUI(
      selector = '#placeholderFilter',where = "afterEnd",
      ui = tags$div(id = filterId,
                    fluidRow(
                      column(12, 
                             actionButton(removeFilterId, label = "Delete Filter",width='50%', icon = icon("minus-circle"),
                                          style = 'padding:4px; font-size:150%,style="text-align:left;"')),align = "center"),
                    selectInput(colfilterId, label = "Select Variable", choices = c("EntryName"), selected = NULL),
                    pickerInput(rowfilterId, label = "Select Variable Values", 
                                choices=NULL, selected = NULL,  multiple = TRUE,
                                choicesOpt = NULL,  width = 220)
      ),
      multiple = TRUE)
    
    observeEvent(input[[colfilterId]], {
      DF <<-  dff()
      col <- input[[colfilterId]]
      values <<- as.list(unique(DF[col]))[[1]]
      
      updatePickerInput(session, rowfilterId , label = "Select Variable Values", choices = values, selected = values,
                        options = list('actions-box' = TRUE))
      
      aggregFilterObserver[[filterId]]$col <<- col
      aggregFilterObserver[[filterId]]$rows <<- NULL
    })
    
    observeEvent(input[[rowfilterId]], {
      rows <- input[[rowfilterId]]
      aggregFilterObserver[[filterId]]$rows <<- rows
    })
    
    observeEvent(input[[removeFilterId]], {
      removeUI(selector = paste0('#', filterId))
      aggregFilterObserver[[filterId]] <<- NULL
      
    })
  })
  
  
  output$data <- renderDataTable(server = FALSE,{
    dataF <- dff()
    invisible(lapply(aggregFilterObserver, function(filter){
      mydata <<- dataF[which((dataF[[filter$col]] %in% filter$rows)),]
    }))
    dTable2(mydata)
  },class = "display"
  )
  
  output$tempdt <- renderDataTable(server = FALSE,{
    tempdataF <- dff()
    invisible(lapply(aggregFilterObserver, function(filter){
      tempmydata <- tempdataF[which((tempdataF[[filter$col]] %in% filter$rows)),]
      tempmydata <- transform(table(tempmydata$EntryName, tempmydata$Sequence))
      tempmydata <- data.frame(setNames(tempmydata, c('EntryName', 'Sequence', 'Count')))
      tempmydata <<- tempmydata[,c('EntryName', 'Sequence')] %>% group_by(EntryName) %>% 
        summarise_all(funs(paste(na.omit(.), collapse = ",")))
    }))
    datatable(tempmydata)
  },class = "display"
  )
  
  ################################################################   
  ##################### Admin & Selected Data TabPanel  ##########
  ################################################################   
  
  x<-reactive({
    req(input$file1)
    df_uploaded_x <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
  })
  
  y<-reactive({
    req(input$file2)
    df_uploaded_y <- read.csv(input$file2$datapath, stringsAsFactors = FALSE)
  })
  
  output$contents <- renderTable({
    if(input$disp1) {
      return(head(x()))
    }
  })
  
  output$contents2 <- renderTable({
    if(input$disp2) {
      return(head(y()))
    }
  })  
  
  
  output$data2 <- renderDataTable({
    dataSet1 <- filtered_data()
    sel <- input$sampleData_rows_selected
    if(length(dataSet1)){
      dfk <- dataSet1[sel, ]
      dfk <- dfk %>% separate_rows(Position_List, sep = ",")
      names(dfk)[names(dfk) == 'Position_List'] <- 'Position'
      dfk$Position <- as.integer(dfk$Position)
      
      dfg <- dfk
      
      qry <<- paste0('{',cmdMongoDb(dfg, "Entry"),",",cmdMongoDb(dfg, "Sequence"),",",cmdMongoDb(dfg, "Position"),'}')
      dfk <- monExpandedRows$find(query = qry, fields = '{"_id":0,"Position_List" : 0}')
      
      mytable = reactive({dfk})
      # set up reactive value
      reactiveData = reactiveVal()
      # observe data
      observeEvent(mytable(),{
        reactiveData(mytable())
      })
      
      proxy = dataTableProxy('data2')
      
      #### Append rows
      observeEvent(input$update,{
        newData <- reactiveData()
        if(is.null(input$addColumn)){
          newData <- rbind.data.frame(dfk, x())
          copyrowdf <- x()
          dbRowUpdate(copyrowdf)
        }else{
          newData <- rbind.data.frame(newData, x())
          copyrowdf <- x()
          dbRowUpdate(copyrowdf)
        }
        newData <<- newData
        reactiveData(newData)
        replaceData(proxy, reactiveData(), resetPaging = FALSE)
        output$data2 = renderDataTable(server = FALSE,{
          dTable(reactiveData())
        })
      })
      
      #### Merge new column
      observeEvent(input$update2,{
        newData <- reactiveData()
        get_common_cols <- function(df1, df2)  intersect(names(df1), names(df2))
        if(!exists("newData")){
          matchedCols <- get_common_cols(y(), dfk)
        }else{
          matchedCols <- get_common_cols(y(), newData)
        }
        print(length(matchedCols) == length(names(y())))
        mcols <- c("Sequence","Entry","Position")
        mcolchars <<- setdiff(matchedCols,mcols)
        
        if(length(matchedCols) == length(names(y()))){
          shinyalert(
            title = "Please check your data",
            callbackR = mycallback,
            text = "All the columns are already Exist",
            type = "warning",
            showCancelButton = TRUE,
            showConfirmButton = TRUE,
            confirmButtonCol = '#DD6B55',
            confirmButtonText = "Merge",
            cancelButtonText = "OverWrite",
            animation = TRUE,
          )
        }else{
          if(!exists("newData")){
            newData <- merge(dfk, y(), by = c("Sequence","Entry","Position"),all.x = TRUE)
          }else{
            newData <- merge(newData, y(), by = c("Sequence","Entry","Position"),all.x = TRUE)
          }
          newData <<- newData
          
          dbColumnUpdate(newData)

          reactiveData(newData)
          replaceData(proxy, reactiveData(), resetPaging = FALSE)
          output$data2 = renderDataTable({
            dTable(reactiveData())
          })
        }
      })
      
      mycallback <- function(value) {
        # valShinyAlert <- value
        if(value == TRUE){
          if(!exists("newData")){
            newData <- merge(dfk, y(), by = c("Sequence","Entry","Position"),all.x = TRUE)
            indx <<- grepl(mcolchars, colnames(newData))
            lss <<- names(newData[indx])
            newData[mcolchars] <- paste(newData[[lss[1]]],newData[[lss[2]]],sep=",")
            dropList <<- lss
            newData <- newData[, !colnames(newData) %in% dropList]
            dbColumnUpdate(newData)
            
          }else{
            newData <- merge(newData, y(), by = c("Sequence","Entry","Position"),all.x = TRUE)
            indx <<- grepl(mcolchars, colnames(newData))
            lss <<- names(newData[indx])
            newData[mcolchars] <- paste(newData[[lss[1]]],newData[[lss[2]]],sep=",")
            dropList <<- lss
            newData <- newData[, !colnames(newData) %in% dropList]
            
            dbColumnUpdate(newData)
            
          }
        }else if(value == FALSE){
          if(!exists("newData")){
            # newData <- anti_join(dfk, y(), by = c("Sequence","Entry","Position_List")) %>% bind_rows(y())
            # newData <- within(merge(dfk, y(), by=c("Sequence","Entry","Position_List"),all.x = TRUE), 
            #        {paste(mcolchars) <- ifelse(is.na(paste(mcolchars,".x", sep="")),paste(mcolchars,".y", sep=""),paste(mcolchars,".x", sep="")); paste(mcolchars,".x", sep="") <- NULL; paste(mcolchars,".y", sep="") <- NULL})
            newData <- merge(x = dfk, y = y(), by = c("Sequence","Entry","Position"), all = T)
            char <- paste0(mcolchars)
            char1 <- paste0(mcolchars, ".x", "")
            char2 <- paste0(mcolchars, ".y", "")
            newData[char1][!is.na(newData[char2])] <- newData[char2][!is.na(newData[char2])]
            newData[char] <- newData[char1]
            dropList2 <<- c(char1, char2)
            newData <- newData[, !colnames(newData) %in% dropList2]
            
            dbColumnUpdate(newData)
            
            
          }else{
            # newData <- anti_join(newData, y(), by = c("Sequence","Entry","Position_List")) %>% bind_rows(y())
            # newData2 <- within(merge(newData, y(), by=c("Sequence","Entry","Position_List"),all.x = TRUE), 
            #        {paste(mcolchars) <- ifelse(is.na(paste(mcolchars,".x", sep="")),paste(mcolchars,".y", sep=""),paste(mcolchars,".x", sep="")); paste(mcolchars,".x", sep="") <- NULL; paste(mcolchars,".y", sep="") <- NULL})
            newData <- merge(x = newData, y = y(), by = c("Sequence","Entry","Position"), all = T)
            char <- paste0(mcolchars)
            char1 <- paste0(mcolchars, ".x", "")
            char2 <- paste0(mcolchars, ".y", "")
            newData[char1][!is.na(newData[char2])] <- newData[char2][!is.na(newData[char2])]
            newData[char] <- newData[char1]
            dropList2 <<- c(char1, char2)
            newData <- newData[, !colnames(newData) %in% dropList2]
            dbColumnUpdate(newData)
          }
          
        }
        
        
        newData <<- newData
        
        dbColumnUpdate(newData)
        
        reactiveData(newData)
        replaceData(proxy, reactiveData(), resetPaging = FALSE)
        output$data2 = renderDataTable({
          dTable(reactiveData())
        })
        
      }
      
      
      # add a column
      observeEvent(input$addColumn,{
        newData <- reactiveData()
        
        dbColumnUpdate(newData)
        
        if(input$nameColumn %in% colnames(newData))
        {
          shinyalert("warning","The column name is already exist", type = "error")
        }else{
          newData[[input$nameColumn]] <- character(length = nrow(newData))
          newData <<- newData
          reactiveData(newData)
          replaceData(proxy, reactiveData(), resetPaging = FALSE)
          output$data2 = renderDataTable({
            dTable(reactiveData())
          })
        }
      })
      
      output$downLoadFilter <- downloadHandler(
        filename = function() {
          paste('Filtered data-', Sys.Date(), '.csv', sep = '')
        },
        content = function(file){
          if(!exists("newData")){
            write.csv(data.frame(matrix(ncol=ncol(dfk),nrow=0, dimnames=list(NULL,names(dfk)))),file, row.names = FALSE)
          }else{
            write.csv(data.frame(matrix(ncol=ncol(newData),nrow=0, dimnames=list(NULL,names(newData)))),file, row.names = FALSE)
          }
        }
      )
      
      observeEvent(input$select_mathFunction,{
        if(!exists("newData")){
          dtf <- dfk
          numcolslist <<- names(dtf)[sapply(dtf, is.numeric)]
          updateSelectizeInput(session, "numeric_cols", choices = numcolslist)
          
        }else{
          dtf <- newData
          numcolslist <<- names(dtf)[sapply(dtf, is.numeric)]
          updateSelectizeInput(session, "numeric_cols", choices = numcolslist)
        }
        
      })
      
      reactive_dt <- eventReactive(input$update3, {
        if(input$select_mathFunction == "Arithmetic Mean"){
          if(!exists("newData")){
            dtf <- dfk
          }else{
            dtf <- newData
          }
          if(input$newcolumnname!="" && !is.null(input$newcolumnname) && input$update3>0){
            newcolval <- rowMeans(dtf[,input$numeric_cols], na.rm=TRUE)
            newcol <- data.frame(newcolval)
            names(newcol) <- input$newcolumnname
            newData <<- cbind(dtf,newcol)
            
            dbColumnUpdate(newData)
          }
          newData
        }else if(input$select_mathFunction == "Geometric Mean"){
          if(!exists("newData")){
            dtf <- dfk
          }else{
            dtf <- newData
          }
          if(input$newcolumnname!="" && !is.null(input$newcolumnname) && input$update3>0){
            newcolval <- exp(rowMeans(log(dtf[,input$numeric_cols]), na.rm=TRUE))
            newcol <- data.frame(newcolval)
            names(newcol) <- input$newcolumnname
            newData <<- cbind(dtf,newcol)
            
            dbColumnUpdate(newData)
          }
          newData
        }else if(input$select_mathFunction == "Addition"){
          if(!exists("newData")){
            dtf <- dfk
          }else{
            dtf <- newData
          }
          if(input$newcolumnname!="" && !is.null(input$newcolumnname) && input$update3>0){
            newcolval <- apply(dtf[,input$numeric_cols],1,sum)
            newcol <- data.frame(newcolval)
            names(newcol) <- input$newcolumnname
            newData <<- cbind(dtf,newcol)
            
            dbColumnUpdate(newData)
          }
          newData
        }
      })      

      mytable2 = reactive({reactive_dt()})
      # set up reactive value
      reactiveData = reactiveVal()
      # observe data
      observeEvent(mytable2(),{
        reactiveData(mytable2())
      })
      proxy = dataTableProxy('data2')
      observeEvent(input$update3,{
        newData <- reactiveData()
        reactiveData(newData)
        replaceData(proxy, reactiveData(), resetPaging = FALSE)
        output$data2 = renderDataTable({
          dTable(reactiveData())
        })
      })
      
      dTable(dfk)
    }
  }, server = FALSE)
  
  observeEvent(input$select_input,{
    if(input$select_input == "Row"){
      shinyjs::hide("columnID")
      shinyjs::show("rowID")
    }else{
      shinyjs::hide("rowID")
      shinyjs::show("columnID")
    }
    
  })
  
  exists.m <- function(x) {
    all(sapply(x, exists))
  }
  
  adcolumn <- reactive({
    if(is.null(input$addColumn)){
      dfNew <- dfk
    }else if(exists("newData1")){
      dfNew <- newData
    }else{
      dfNew <- newData
    }
    return(dfNew)
  })
  
  
  output$rowsTemplate <- downloadHandler(
    filename = function() {
      paste0("rowsTemplate.csv")
    },
    content = function(file) {
      if(!exists("newData") & !exists("newData1")){
        write.csv(data.frame(matrix(ncol=ncol(filtered_data()),nrow=0, dimnames=list(NULL,names(filtered_data())))),file, row.names = FALSE)
      }else{
        write.csv(data.frame(matrix(ncol=ncol(adcolumn()),nrow=0, dimnames=list(NULL,names(adcolumn())))),file, row.names = FALSE)
      }
      
    }
  )
  
  output$columnTemplate <- downloadHandler(
    filename = function() {
      paste0("columnsTemplate.csv")
    },
    content = function(file) {
      # Create a Vector with Columns
      columns = c("Sequence","Entry","Position") 
      #Create a Empty DataFrame with 0 rows and n columns
      df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
      # Assign column names
      colnames(df) = columns
      write.csv(data.frame(df),file, row.names = FALSE)
    }
  )
  
  ################################################################   
  ##################### DashBorad TabPanel  ##################### 
  ################################################################   
  output$value1 <- renderValueBox({
    dataforCounts <- filtered_data()
    valueBox(
      formatC(length(unique(dataforCounts[["Entry"]])), format="d", big.mark=',')
      ,'Entries'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
  })
  output$value2 <- renderValueBox({ 
    dataforCounts <- filtered_data()
    valueBox(
      formatC(length(unique(dataforCounts[["GeneNames"]])), format="d", big.mark=',')
      ,'Genes'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")  
  })
  output$value3 <- renderValueBox({
    dataforCounts <- filtered_data()
    valueBox(
      formatC(length(unique(dataforCounts[["GeneNames"]])), format="d", big.mark=',')
      ,'Position'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "yellow")   
  })  
  output$value4 <- renderValueBox({
    dataforCounts <- filtered_data()
    valueBox(
      formatC(length(unique(dataforCounts[["Sequence"]])), format="d", big.mark=',')
      ,'Sequence'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "maroon")   
  })
  
  #creating the plotOutput content
  output$seqencePlot <- renderPlot({
    dataforPlots <- filtered_data()
    seqTable <- as.data.frame(table(dataforPlots$Sequence))
    colnames(seqTable)<- c("Sequence","Count")
    
    ggplot(data = seqTable, 
           aes(x=Sequence, y=Count, fill=factor(Sequence))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Sequence Count") + 
      xlab("Sequence Category") + theme(legend.position="bottom" 
                                        ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Sequence") + theme(plot.title = element_text(hjust = 0.5)) + labs(fill = "Sequence")
  })
  
  output$summaryDset <- renderPrint({
    dataforSummary <- filtered_data()
    summary(dataforSummary) 
  })  
  
}


shinyApp(ui = ui, server = server)
