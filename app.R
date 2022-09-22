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
# remotes::install_github("datastorm-open/shinymanager")
library(shinymanager)
library(scrypt)
library(shinyBS)
library(spsComps)
library(jsonlite)
library(igraph)
library(networkD3)
library(plyr)

mon <- mongo(collection = "entries_unique_rows", db = "interdictbio_v2", url = "mongodb://192.168.204.195:27018",verbose = TRUE)
monExpandedRows <- mongo(collection = "entries_seperate_rows", db = "interdictbio_v2", url = "mongodb://192.168.204.195:27018",verbose = TRUE)
seqQry <- '[{"$group":{"_id":"$Sequence"}},{"$limit":2000}]'
# seqQry <- '[{"$group":{"_id":"$Sequence"}}]'
seqList <- mon$aggregate(seqQry)

###########################################
# Custom render DataTable Function creation
###########################################
dTable <- function(df){
  datatable(df,selection = 'multiple', editable = TRUE,rownames = FALSE, class = 'cell-border stripe',
            extensions = c("SearchBuilder","Buttons",'ColReorder'), #'Select'
            options = list(pageLength = 7,
                           info = TRUE, lengthMenu = list(c(7, -1), c("7", "All")),
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                             "}"),
                           columnDefs = list(list(className = 'dt-left', targets = "_all")),
                           scrollX = TRUE,
                           search = list(regex = TRUE),
                           searching = TRUE,
                           colReorder = TRUE,
                           ordering = TRUE,
                           dom = "QfrBitlp",
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
                           language = list(searchBuilder= list(title= "Refine your search", add = 'Add Filter Condition')),
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
  qry1 <<- paste0('{',cmdMongoDb(df, "Entry"),",",cmdMongoDb(df, "Sequence"), ",", cmdMongoDb(df, "Position"),'}')
  print(dim(monExpandedRows$find(query = qry1))[1])
  if(dim(monExpandedRows$find(query = qry1))[1]>0){
    colsLs1 <<- c("Entry", "Sequence","Position")
    colsLs2 <<- names(df)
    diffColName2 <<- setdiff(colsLs2, colsLs1)
    for(i in diffColName2){
      qry2 <<- paste0('{',cmdUpdate(df, i),'}')
      monExpandedRows$update(query = qry1, update=qry2)
      print(monExpandedRows$find(query = qry1))
      # return(monExpandedRows$find(query = qry1))
    }
  }else{
    monExpandedRows$insert(rjson::toJSON(df))
    print(monExpandedRows$find(query = qry1))
    # return(monExpandedRows$find(query = qry1))
  }
  # return(monExpandedRows$find(query = qry1))
}


#####################################

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
  user = c("praveen", "puneet", "veda", "sami", "admin"), # mandatory
  password = c("pk@123", "ps@123","vt@123","sb@123", "admin"), # mandatory
  start = c("2015-04-15"), # optinal (all others)
  expire = c("2032-12-31"),
  admin = c(TRUE, TRUE, FALSE, TRUE, FALSE),
  comment = "Simple and secure authentification mechanism 
  for single 'Shiny' applications.",
  stringsAsFactors = FALSE,
  moreInfo = c("someData1", "someData2","someData3","someData4","someData5"),
  level = c(2,0,2,0,0)
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
                 fab_position = "none",
                 # fab_button = "none",
                 # fab_position = "bottom-left",
                 tags_top = tags$div(
                   tags$head(tags$style(css)),
                   tags$img(
                     src = "output-onlinepngtools.png", width = 200, height = 30, alt="Logo not found", deleteFile=FALSE
                   )),
                 dashboardPage(
                   dashboardHeader(title = tags$a(tags$img(height = "25px",src="output-onlinepngtools.png")),
                                   tags$li(class="dropdown",tags$a("Help", target="_blank")),
                                   tags$li(class="dropdown",tags$a("User", target="_blank")),
                                   tags$li(class="dropdown",actionLink("action_logout", "Logout!",
                                                                       style='font-size:120%;font-weight: bold;'))),
                   
                   dashboardSidebar(
                     tags$script("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"),
                     width = 250,
                     radioButtons("select_seq", "Choose Regex or Normal", choices = c("Regex", "Normal")),
                     textInput("Sequence", "Enter the sequences"),
                     div(style="text-align:left;padding-left: 20px; font-style: italic;",
                         "Multiple sequences should be",br(), " comma separated with no spaces.",br(), "(E.g. AAAA,AAAT)"),
                     br(),
                     # bsTooltip("Sequence","Multiple Sequences should be comma seperated","bottom"),
                     # bsTooltip("sidebar-toggle","Multiple Sequences should be comma seperated","bottom"),
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
    .tabbable > .nav > li > a[data-value='Selected Data'] {background-color: #847c8a;  color:white; font-size: 16px;}
    .tabbable > .nav > li > a[data-value='Dashboard'] {background-color: #847c8a; color:white; font-size: 16px;}
    .tabbable > .nav > li > a[data-value='Admin'] {background-color: #847c8a; color:white; font-size: 16px;}
    .tabbable > .nav > li[class=active]    > a {background-color: #0dc5c1; color:white; font-size: 16px; font-style:oblique; font-weight:bold;}

  ")),
                     tabsetPanel(id = 'dataset',
                                 tabPanel("Results Summary",
                                          # tags$head(includeCSS("searchBuilder.dataTables.min.css")),
                                          tags$head(tags$style("#dtsb-group{color: red;font-size: 20px;font-style: italic;}")),
                                          br(), DT::dataTableOutput("sampleData"),
                                          br(),
                                          br(),
                                          span(textOutput('rendertext'), style="color:black; padding-left: 20px; font-style: italic; font-size: 20px;"),
                                          br(),
                                          br(),
                                          column(DT::dataTableOutput("tempdt"), width = 6),
                                          # tags$head(tags$style(HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(10%);}"))),
                                          DT::dataTableOutput("interSectionData"),
                                          shinyjs::useShinyjs(),
                                          shinyjs::hidden(downloadButton("downloadData", "Download Selected Rows",
                                                                         icon = icon("download"),
                                                                         style="color: #333; margin-left:-700px; 
                                                                         background-color: #FFF; border-color: #333"))),
                                 tabPanel("Selected Data", br(),DT::dataTableOutput("data2"),
                                          dashboardSidebar(width = 250,
                                                           fluidRow(style = "padding: 40px 14px 5px 14px; margin: 5px 5px 5px 5px; ",
                                                                    # custom column name
                                                                    textInput(inputId = "nameColumn", "Enter Column Name"),
                                                                    actionButton(inputId = "addColumn", "Create Bins"),
                                                                    actionButton(inputId = "done", "Done")))
                                 ),
                                 tabPanel("Dashboard", fluidRow(br(),
                                                                dashboardSidebar(width = 250),
                                                                valueBoxOutput("value1", width = 4)
                                                                ,valueBoxOutput("value2", width = 4)
                                                                ,valueBoxOutput("value4", width = 4)),
                                          fluidRow(
                                            box(
                                              title = "Sequence vs Entry"
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              ,plotlyOutput("seqencePlot", height = "300px")
                                            ),
                                            box(
                                              title = "Sequence vs Count"
                                              ,status = "primary"
                                              ,solidHeader = TRUE 
                                              ,collapsible = TRUE 
                                              ,plotlyOutput("seqenceCount", height = "300px")
                                            ),
                                            box(
                                              title = "Intersection", 
                                              status = "warning", 
                                              solidHeader = TRUE,
                                              width = 12,
                                              collapsible = TRUE,
                                              simpleNetworkOutput("seqLengthnet")
                                            ),
                                          )
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
                     )
                   ),tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-image: linear-gradient(45deg, #052C49,#8C2D29);
                              background-position-x: initial;
                              background-position-y: initial;
                              background-size: initial;
                              background-repeat-x: initial;
                              background-repeat-y: initial;
                              background-attachment: initial;
                              background-origin: initial;
                              background-clip: initial;
                              background-color: initial;
                                  }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-image: linear-gradient(45deg, #052C49,#8C2D29);
                              background-position-x: initial;
                              background-position-y: initial;
                              background-size: initial;
                              background-repeat-x: initial;
                              background-repeat-y: initial;
                              background-attachment: initial;
                              background-origin: initial;
                              background-clip: initial;
                              background-color: initial;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-image: linear-gradient(45deg, #052C49,#8C2D29);
                              background-position-x: initial;
                              background-position-y: initial;
                              background-size: initial;
                              background-repeat-x: initial;
                              background-repeat-y: initial;
                              background-attachment: initial;
                              background-origin: initial;
                              background-clip: initial;
                              background-color: initial;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-image: linear-gradient(45deg, #052C49,#8C2D29);
                              background-position-x: initial;
                              background-position-y: initial;
                              background-size: initial;
                              background-repeat-x: initial;
                              background-repeat-y: initial;
                              background-attachment: initial;
                              background-origin: initial;
                              background-clip: initial;
                              background-color: initial;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-image: linear-gradient(45deg, #052C49,#8C2D29);
                              background-position-x: initial;
                              background-position-y: initial;
                              background-size: initial;
                              background-repeat-x: initial;
                              background-repeat-y: initial;
                              background-attachment: initial;
                              background-origin: initial;
                              background-clip: initial;
                              background-color: initial;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-image: linear-gradient(45deg, #052C49,#8C2D29);
                              background-position-x: initial;
                              background-position-y: initial;
                              background-size: initial;
                              background-repeat-x: initial;
                              background-repeat-y: initial;
                              background-attachment: initial;
                              background-origin: initial;
                              background-clip: initial;
                              background-color: initial;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-image: linear-gradient(45deg, #052C49,#8C2D29);
                              background-position-x: initial;
                              background-position-y: initial;
                              background-size: initial;
                              background-repeat-x: initial;
                              background-repeat-y: initial;
                              background-attachment: initial;
                              background-origin: initial;
                              background-clip: initial;
                              background-color: initial;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-image: linear-gradient(45deg, #052C49,#8C2D29);
                              background-position-x: initial;
                              background-position-y: initial;
                              background-size: initial;
                              background-repeat-x: initial;
                              background-repeat-y: initial;
                              background-attachment: initial;
                              background-origin: initial;
                              background-clip: initial;
                              background-color: initial;
                              }
                              '))
                   )
                 ))

server <- function(input, output, session) {
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  # Create reactive values including all credentials
  creds_reactive <- reactive({
    reactiveValuesToList(res_auth)
  })
  
  # Hide extraOutput only when condition is TRUE
  observe({
    if (!is.null(creds_reactive()$level) && creds_reactive()$level > 0){
      hideTab("dataset", "Admin")
    }else{
      showTab("dataset", "Admin")
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
  
  output$sampleData <- renderDataTable(server = FALSE,{
    df100 <- as.data.frame(mon$aggregate('[{"$limit": 1000}]'))
    df100 <- unique(df100[c("EntryName","Entry","ProteinName","GeneNames","Organism","Length","Sequence","Position_List","Count")])
    dTable(df100)
  })
  
  
  query_db <- function(input){
    doc_type <- paste0(input)
    print(doc_type)
    str_vector <- input
    str_json   <- paste0("\"",paste0(unlist(strsplit(str_vector, ",")),collapse= "\",\""),"\"")
    print(str_json)
    qry <- paste0('{"Sequence":{"$in" : [',str_json,']}}')
    print(qry)
    x <- mon$find(query = qry, fields = '{"Position" : 0}')
    x <- unique(x[c("EntryName","Entry","ProteinName","GeneNames","Organism","Length","Sequence","Position_List","Count")])
    return(x)
  }
  
  query_db_reg <- function(input){
    doc_type <- paste0(input)
    print(doc_type)
    str_vector <- input
    str_json   <<- paste0("\"",paste0(unlist(strsplit(str_vector, ",")),collapse= "\",\""),"\"")
    str_vector2 <<- jsonlite::toJSON(str_vector)
    str_vector3 <<- rjson::toJSON(str_vector)
    print(str_json)
    qry <- paste0('{"Sequence":{"$regex" : ',str_vector3,', "$options" : "i"}}')
    print(qry)
    x <- mon$find(query = qry, fields = '{"Position" : 0}')
    x <- unique(x[c("EntryName","Entry","ProteinName","GeneNames","Organism","Length","Sequence","Position_List","Count")])
    return(x)
  }
  
  
  observeEvent(input$select_seq,{
    
    if(input$select_seq == "Normal"){
      filtered_data <<- eventReactive(input$search, valueExpr = query_db(input$Sequence))
    }else{
      updateTextInput(session, "Sequence", value = input$Sequence)
      filtered_data <<- eventReactive(input$search, valueExpr = query_db_reg(input$Sequence))
    }
  })
  
  
  filtered_data <- eventReactive(input$search, valueExpr = query_db(input$Sequence))
  
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
      dTable(dataSet)
    },class = "display")
    
  }
  )
  
  observeEvent(input$sampleData_rows_selected, {
    if(input$sampleData_rows_selected == "")
      shinyjs::hide("downloadData")
    else
      shinyjs::show("downloadData")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(gsub(" ","_", gsub(":","\t", Sys.time())),".tsv")
    },
    content = function(file) {
      write.table(dataSet[input$sampleData_rows_selected,], file, row.names = FALSE)
    }
  )
  
  ## Finding the InterSection between the sequences based on EntryName
  dfInter <- eventReactive(input$addFilter,{
    datac <- filtered_data()
    datac <-  datac %>%
      group_by(EntryName) %>%
      filter(n_distinct(Sequence) > 1) %>%
      ungroup
  })
  
  output$interSectionData <- renderDataTable(server = FALSE,{
    dff <<- dfInter()
    datatable(dff)
  })
  
  dfj <- reactive({
    tempdataF <- dfInter()
    if(!empty(tempdataF)){
      tempmydata <- tempdataF
      tempmydata <- data.frame(lapply(tempmydata, as.character))
      tempmydata <- transform(table(tempmydata$EntryName, tempmydata$Sequence))
      tempmydata <- data.frame(setNames(tempmydata, c('EntryName', 'Sequence', 'Count')))
      tempmydata <- tempmydata[,c('EntryName', 'Sequence')] %>% group_by(EntryName) %>% 
        summarise_all(funs(paste(na.omit(.), collapse = ",")))
    }else{
      shiny::showNotification("No data", type = "error")
      tempmydata <- tempdataF
      tempmydata <- data.frame(lapply(tempmydata, as.character))
      tempmydata <- transform(table(tempmydata$EntryName, tempmydata$Sequence))
      tempmydata <- data.frame(setNames(tempmydata, c('EntryName', 'Sequence')))
      tempmydata
    }
  })
  
  output$rendertext <- renderText({
    if(nrow(dfj()) == 0)
      return("There are no overlapped entries for these sequences")
  })
  
  output$tempdt <- renderDataTable(server = FALSE,{datatable(dfj())},class = "display")
  
  
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
  
  selDF <- reactive({
    dataSet1 <- filtered_data()
    sel <- input$sampleData_rows_selected
    if(is.null(sel)){
      dfl <- dataSet1[FALSE,]
    }else{
      dfk <- dataSet1[sel, ]
      dfk <- dfk %>% separate_rows(Position_List, sep = ",")
      names(dfk)[names(dfk) == 'Position_List'] <- 'Position'
      dfk$Position <- as.integer(dfk$Position)
      dfg <- dfk
      qry <<- paste0('{',cmdMongoDb(dfg, "Entry"),",",cmdMongoDb(dfg, "Sequence"),",",cmdMongoDb(dfg, "Position"),'}')
      dfk <- monExpandedRows$find(query = qry, fields = '{"_id":0,"Position_List" : 0}')
    }
  }
  )
  
  output$data2 <- renderDataTable(server = FALSE,{
    dTable(selDF())
  })
  
  proxy = dataTableProxy('data2')
  observeEvent(input$update,{
    if(!exists("newData")){
      copyrowdf <<- x()
      dbRowUpdate(copyrowdf)
      newData <- monExpandedRows$find(query = qry, fields = '{"_id":0,"Position_List" : 0}')
      dfh <- rbind(newData,copyrowdf)
      qrys <<- paste0('{',cmdMongoDb(dfh, "Entry"),",",cmdMongoDb(dfh, "Sequence"),",",cmdMongoDb(dfh, "Position"),'}')
      newData <- monExpandedRows$find(query = qrys, fields = '{"_id":0,"Position_List" : 0}')
      
    }else{
      copyrowdf <<- x()
      dbRowUpdate(copyrowdf)
      newData <- monExpandedRows$find(query = qry, fields = '{"_id":0,"Position_List" : 0}')
      dfh <- rbind(newData,copyrowdf)
      qrys <<- paste0('{',cmdMongoDb(dfh, "Entry"),",",cmdMongoDb(dfh, "Sequence"),",",cmdMongoDb(dfh, "Position"),'}')
      newData <- monExpandedRows$find(query = qrys, fields = '{"_id":0,"Position_List" : 0}')
      
    }
    newData <<- newData
    reactiveData(newData)
    replaceData(proxy, reactiveData())
    output$data2 = renderDataTable(server = FALSE,{
      dTable(reactiveData())
    })
  })
  
  #### Merge new column
  proxy = dataTableProxy('data2')
  observeEvent(input$update2,{
    get_common_cols <- function(df1, df2)  intersect(names(df1), names(df2))
    if(!exists("newData")){
      matchedCols <- get_common_cols(y(), selDF())
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
        newData <- merge(selDF(), y(), by = c("Sequence","Entry","Position"),all.x = TRUE)
      }else{
        newData <- merge(newData, y(), by = c("Sequence","Entry","Position"),all.x = TRUE)
      }
      newData <<- newData
      dbColumnUpdate(newData)
      reactiveData(newData)
      replaceData(proxy, reactiveData(), resetPaging = FALSE)
      output$data2 = renderDataTable(server = FALSE,{
        dTable(reactiveData())
      })
    }
  })
  
  mycallback <- function(value) {
    if(value == TRUE){
      if(!exists("newData")){
        newData <- merge(selDF(), y(), by = c("Sequence","Entry","Position"),all.x = TRUE)
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
        newData <- merge(x = selDF(), y = y(), by = c("Sequence","Entry","Position"), all = T)
        char <- paste0(mcolchars)
        char1 <- paste0(mcolchars, ".x", "")
        char2 <- paste0(mcolchars, ".y", "")
        newData[char1][!is.na(newData[char2])] <- newData[char2][!is.na(newData[char2])]
        newData[char] <- newData[char1]
        dropList2 <<- c(char1, char2)
        newData <- newData[, !colnames(newData) %in% dropList2]
        dbColumnUpdate(newData)
      }else{
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
    output$data2 = renderDataTable(server = FALSE,{
      dTable(reactiveData())
    })
  }
  
  proxy = dataTableProxy('data2')
  observeEvent(input$data2_cell_edit, {
    info = input$data2_cell_edit
    print(info)
    if(!exists("newData")){
      newData <- selDF()
      newData[info$row, info$col+1] <- info$value
      reactiveData(newData)
      replaceData(proxy, reactiveData(), resetPaging = FALSE)
    }else{
      newData <- reactiveData()
      newData[info$row, info$col+1] <- info$value
      reactiveData(newData)
      replaceData(proxy, reactiveData(), resetPaging = FALSE)
    }
  })
  
  # add a column
  observeEvent(input$addColumn,{
    if(!exists("newData")){
      newData <- selDF()
      dbColumnUpdate(newData)
      if(input$nameColumn %in% colnames(newData))
      {
        shinyalert("warning","The column name is already exist", type = "error")
      }else{
        newData[[input$nameColumn]] <- character(length = nrow(newData))
        newData <<- newData
        reactiveData(newData)
        replaceData(proxy, reactiveData(), resetPaging = FALSE)
        output$data2 = renderDataTable(server = FALSE,{
          print(reactiveData())
          newData <<- reactiveData()
          dTable(newData)
          
        })
      }
    }else{
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
        output$data2 = renderDataTable(server = FALSE,{
          print(reactiveData())
          newData <<- reactiveData()
          dTable(newData)
        })
      }
    }
  })
  
  # check for 'done' button press
  observeEvent(input$done, {
    newData <<- reactiveData()
  })
  
  output$downLoadFilter <- downloadHandler(
    filename = function() {
      paste('Filtered data-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      if(!exists("newData")){
        write.csv(data.frame(matrix(ncol=ncol(selDF()),nrow=0, dimnames=list(NULL,names(selDF())))),file, row.names = FALSE)
      }else{
        write.csv(data.frame(matrix(ncol=ncol(newData),nrow=0, dimnames=list(NULL,names(newData)))),file, row.names = FALSE)
      }
    }
  )
  
  observeEvent(input$select_mathFunction,{
    if(!exists("newData")){
      dtf <- selDF()
      dtf$Count <- as.numeric(dtf$Count)
      dtf$Length <- as.numeric(dtf$Length)
      dtf$Position <- as.numeric(dtf$Position)
      numcolslist <<- names(dtf)[sapply(dtf, is.numeric)]
      updateSelectizeInput(session, "numeric_cols", choices = numcolslist)
    }else{
      dtf <- newData
      dtf$Count <- as.numeric(dtf$Count)
      dtf$Length <- as.numeric(dtf$Length)
      dtf$Position <- as.numeric(dtf$Position)
      numcolslist <<- names(dtf)[sapply(dtf, is.numeric)]
      updateSelectizeInput(session, "numeric_cols", choices = numcolslist)
    }
  })
  
  reactive_dt <- eventReactive(input$update3, {
    if(input$select_mathFunction == "Arithmetic Mean"){
      if(!exists("newData")){
        dtf <- selDF()
        dtf$Count <- as.numeric(dtf$Count)
        dtf$Length <- as.numeric(dtf$Length)
        dtf$Position <- as.numeric(dtf$Position)
      }else{
        dtf <- newData
        dtf$Count <- as.numeric(dtf$Count)
        dtf$Length <- as.numeric(dtf$Length)
        dtf$Position <- as.numeric(dtf$Position)
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
        dtf <- selDF()
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
        dtf <- selDF()
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
    output$data2 = renderDataTable(server = FALSE,{
      dTable(reactiveData())
    })
  })
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
      dfNew <- selDF()
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
  output$seqencePlot <- renderPlotly({
    dataforPlots <<- filtered_data()
    seqTable <- as.data.frame(table(dataforPlots$Sequence))
    colnames(seqTable)<- c("Sequence","Count")
    p1 <- ggplot(data = seqTable, 
                 aes(x=Sequence, y=Count, fill=factor(Sequence))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Target Count") + 
      xlab("Sequence") + theme(legend.position="bottom" 
                               ,plot.title = element_text(size=15, face="bold")) + 
      theme(plot.title = element_text(hjust = 0.5)) + labs(fill = "Sequence")
    ggplotly(p1)
  })
  
  #creating the plotOutput content
  output$seqenceCount <- renderPlotly({
    dataforPlots <<- filtered_data()
    sequenceSumCount <<- dataforPlots
    sequenceSumCount <<- aggregate(sequenceSumCount$Count, by=list(Category=sequenceSumCount$Sequence), FUN=sum, na.rm=TRUE)
    colnames(sequenceSumCount)<- c("Sequence","Count")
    p2 <- ggplot(data = sequenceSumCount, 
                 aes(x=Sequence, y=Count, fill=factor(Sequence))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Sum of Count") + 
      xlab("Sequence") + theme(legend.position="bottom" 
                               ,plot.title = element_text(size=15, face="bold")) + 
      theme(plot.title = element_text(hjust = 0.5)) + labs(fill = "Sequence")
    ggplotly(p2)
    
    # ggplotly(ggplot(sequenceSumCount, aes(x=Category, y=x)) + geom_bar(stat = "identity"))
  })
  
  #creating the plotOutput content
  output$positionVsLengthBySeq <- renderPlotly({
    dataforPlots <- filtered_data()
    dataforPlots <- dataforPlots %>% separate_rows(Position_List, sep = ",")
    dataforPlots$Position_List <- as.numeric(dataforPlots$Position_List)
    fig <- plot_ly(data = dataforPlots, x = ~Length, y = ~Position_List, color = ~Sequence, type = "scatter")
    fig
  })
  
  #creating the plotOutput content
  output$seqLengthnet <- renderSimpleNetwork({
    dataforPlots <- filtered_data()
    dataforPlots <- dataforPlots %>% separate_rows(Position_List, sep = ",")
    dataforPlots$Position_List <- as.numeric(dataforPlots$Position_List)
    simpleNetwork(dataforPlots[, c("Sequence", "Entry")], height="100px", width="100px", zoom = TRUE)
  })
  
  
  output$summaryDset <- renderPrint({
    dataforSummary <- filtered_data()
    summary(dataforSummary) 
  })
  
  observeEvent(input$action_logout, {
    session$reload()
  }) 
  observe({
    req(res_auth$user)
    shinyjs::hide("fab_btn_div")
  })
  
}

shinyApp(ui = ui, server = server)