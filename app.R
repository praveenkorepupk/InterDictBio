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
library(visNetwork)
library(shinycssloaders)
library(htmltools)

#### Version - 1
monUniqueRows <- mongo(collection = "entries_unique_rows", db = "interdictbio", url = "mongodb://192.168.204.195:27017",verbose = TRUE)
monExpandedRows <- mongo(collection = "entries_seperate_rows", db = "interdictbio", url = "mongodb://192.168.204.195:27017",verbose = TRUE)

# ### Version - 2
# monUniqueRows <- mongo(collection = "entries_unique_rows", db = "interdictbio_v2", url = "mongodb://192.168.204.195:27018",verbose = TRUE)
# monExpandedRows <- mongo(collection = "entries_seperate_rows", db = "interdictbio_v2", url = "mongodb://192.168.204.195:27018",verbose = TRUE)

###########################################
# Custom render DataTable Function creation
###########################################
path_to_searchBuilder <- # path to the folder containing the two searchBuilder files
  normalizePath("~/praveenk/RShinyApp/customSearchBuilder/")

dTable <- function(df){
  dtable <- datatable(df,selection = 'multiple', editable = FALSE,rownames = FALSE, class = 'cell-border stripe',
                      extensions = c("Buttons",'ColReorder'),
                      options = list(pageLength = 7,info = TRUE, lengthMenu = list(c(7, -1), c("7", "All")),
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
                                     searchBuilder = list()))
  
  dep <- htmlDependency(
    name = "searchBuilder",
    version = "1.0.0", 
    src = path_to_searchBuilder,
    script = "dataTables.searchBuilder.min.js",
    stylesheet = "searchBuilder.dataTables.min.css",
    all_files = FALSE
  )
  dtable$dependencies <- c(dtable$dependencies, list(dep))
  dtable
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
  colsList1 <- c("Entry", "Sequence","Position", "EntryName","ProteinName","GeneNames","Organism","Length","Count")
  colsList2 <- names(df)
  diffColName <- setdiff(colsList2, colsList1)
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
  # print(dim(monExpandedRows$find(query = qry1))[1])
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
  user = c("user1", "user2", "user3", "user4", "admin"), # mandatory
  password = c("user1", "user2","user3","user4", "admin"), # mandatory
  start = c("2015-04-15"), # optinal (all others)
  expire = c("2032-12-31"),
  admin = c(TRUE, TRUE, FALSE, TRUE, FALSE),
  comment = "Simple and secure authentification mechanism 
  for single 'Shiny' applications.",
  stringsAsFactors = FALSE,
  moreInfo = c("someData1", "someData2","someData3","someData4","someData5"),
  level = c(2,2,2,0,0)
)


css <- HTML(".btn-primary {
                  color: #ffffff;
                  background-color: #1384a7;
                  border-color: #1384a7;
              }
              .panel-primary {
                  border-color: #1384a7;
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
                                   # tags$li(class="dropdown",tags$a("Help", target="_blank", href="sample.pdf",tags$img(src='help.png'))),
                                   tags$li(class="dropdown",
                                           actionLink("Help", "Help", icon = icon("info-sign", lib = "glyphicon",
                                                                                  onclick ="window.open('sample.pdf', '_blank')"))),
                                   tags$li(class="dropdown",
                                           actionLink("User", "User", icon = icon("user", lib = "glyphicon"))),
                                   tags$li(class="dropdown",actionLink("action_logout", "Exit",icon = icon("off", lib = "glyphicon"),
                                                                       style='font-size:100%;font-weight: bold;
                                                                       margin-left:-16px;'))),
                   dashboardSidebar(
                     tags$script("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"),
                     width = 250,
                     radioButtons("select_seq", "Choose Regex or Normal", choices = c("Normal","Regex")),
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
                            bsTooltip("search","Filter data by Sequence(s)", "bottom"),
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
    .tabbable > .nav > li > a[data-value='Results Summary'] {background-color: #847c8a; color:white; font-size: 16px;}
    .tabbable > .nav > li > a[data-value='Selected Data'] {background-color: #847c8a;  color:white; font-size: 16px;}
    .tabbable > .nav > li > a[data-value='Dashboard'] {background-color: #847c8a; color:white; font-size: 16px;}
    .tabbable > .nav > li > a[data-value='Admin'] {background-color: #847c8a; color:white; font-size: 16px;}
    .tabbable > .nav > li[class=active] > a {background-image: linear-gradient(45deg, #052C49,#8C2D29); color:white; font-size: 16px; font-style:oblique; font-weight:bold;}

  ")),
                     tabsetPanel(id = 'dataset',
                                 tabPanel("Results Summary", 
                                          icon = icon("table"),
                                          tags$head(tags$style("#dtsb-group{color: red;font-size: 20px;font-style: italic;}")),
                                          br(), 
                                          shinycssloaders::withSpinner(
                                            DT::dataTableOutput("sampleData")
                                          ),
                                          
                                          shinyjs::useShinyjs(),
                                          shinyjs::hidden(downloadButton("downloadData", "Download Selected Rows",
                                                                         icon = icon("download"),
                                                                         style="color: #333; margin-left:-700px; 
                                                                         background-color: #FFF; border-color: #333")),
                                          
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          column(DT::dataTableOutput("tempdt"), width = 6),
                                          DT::dataTableOutput("interSectionData")),
                                 tabPanel("Selected Data", icon = icon("table"), br(),DT::dataTableOutput("data2"),
                                          actionButton(inputId = "refreshDb", "Extract Data"),
                                          dashboardSidebar(width = 250,
                                                           fluidRow(style = "padding: 40px 14px 5px 14px; margin: 5px 5px 5px 5px; ",
                                                                    # custom column name
                                                                    textInput(inputId = "nameColumn", "Enter Column Name"),
                                                                    actionButton(inputId = "addColumn", "Create Bins"),
                                                                    actionButton(inputId = "done", "Done")))),
                                 tabPanel("Dashboard", icon = icon("bar-chart-o"), 
                                          fluidRow(br(),
                                                   dashboardSidebar(width = 250),
                                                   valueBoxOutput("value1", width = 4)
                                                   ,valueBoxOutput("value2", width = 4)
                                                   ,valueBoxOutput("value4", width = 4)),
                                          fluidRow(br(),
                                                   style = "padding: 8px 24px 8px 0px;
                                              margin: 0px 0px 10px 8px;",
                                                   actionBttn(inputId="genPlots","Generate Plots", icon = icon("bar-chart-o"))),
                                          fluidRow(
                                            box(id = "seqplot12", width = 12,
                                                box(
                                                  title = "Sequence vs Entry"
                                                  ,status = "primary"
                                                  ,solidHeader = TRUE 
                                                  ,collapsible = TRUE
                                                  ,width = 12
                                                  ,shinycssloaders::withSpinner(plotlyOutput("seqencePlot", height = "300px"))
                                                ),
                                                box(
                                                  title = "Sequence vs Count"
                                                  ,status = "primary"
                                                  ,solidHeader = TRUE 
                                                  ,collapsible = TRUE 
                                                  ,width = 12
                                                  ,shinycssloaders::withSpinner(plotlyOutput("seqenceCount", height = "300px"))
                                                ),
                                                box(
                                                  title = "Intersection", 
                                                  status = "warning", 
                                                  solidHeader = TRUE,
                                                  width = 12,
                                                  collapsible = TRUE,
                                                  # simpleNetworkOutput("seqLengthnet")
                                                  shinycssloaders::withSpinner(visNetworkOutput("seqLengthnet"))
                                                ),
                                            ))
                                 ),
                                 tabPanel(
                                   "Admin",icon = icon("user", lib = "glyphicon"),
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
                              background-image: linear-gradient(45deg, #e6dbdb,#f4f4f4);}

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-image: linear-gradient(45deg, #e6dbdb,#f4f4f4);}

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-image: linear-gradient(45deg,#f4f4f4,#8C2D29);}        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-image: linear-gradient(45deg, #052C49,#8C2D29);}

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-image: linear-gradient(45deg, #052C49,#8C2D29);}

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-image: linear-gradient(45deg, #052C49,#8C2D29);}

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-image: linear-gradient(45deg, #052C49,#8C2D29);}
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-image: linear-gradient(45deg, #052C49,#8C2D29);}
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
    df10 <- as.data.frame(monUniqueRows$aggregate('[{"$limit": 10}]'))
    df10 <- unique(df10[c("EntryName","Entry","ProteinName","GeneNames","Organism","Length","Sequence","Position_List","Count")])
    dTable(df10)
    
  })
  
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
  
  query_db <- function(input){
    doc_type <- paste0(input)
    # print(doc_-type)
    str_vector <- input
    str_json   <- paste0("\"",paste0(unlist(strsplit(str_vector, ",")),collapse= "\",\""),"\"")
    qry <- paste0('{"Sequence":{"$in" : [',str_json,']}}')
    qry <- gsub(" ","",qry)
    x <- monUniqueRows$find(query = qry, fields = '{"Position" : 0}')
    if(empty(x) == TRUE){
      print("Sequence is not matched")
      columns = c("EntryName","Entry","ProteinName","GeneNames","Organism","Length","Sequence","Position_List","Count")
      x = data.frame(matrix(nrow = 0, ncol = length(columns)))
      # assign column names
      colnames(x) = columns
    }else{
      x <- unique(x[c("EntryName","Entry","ProteinName","GeneNames","Organism","Length","Sequence","Position_List","Count")])
    }
    return(x)
  }
  
  query_db_reg <- function(input){
    doc_type <- paste0(input)
    # print(doc_type)
    str_vector <- input
    str_json   <- paste0("\"",paste0(unlist(strsplit(str_vector, ",")),collapse= "\",\""),"\"")
    str_vector3 <- rjson::toJSON(str_vector)
    qry <- paste0('{"Sequence":{"$regex" : ',str_vector3,', "$options" : "i"}}')
    x <- monUniqueRows$find(query = qry, fields = '{"Position" : 0}')
    if(empty(x) == TRUE){
      print("Sequence is not matched")
      columns = c("EntryName","Entry","ProteinName","GeneNames","Organism","Length","Sequence","Position_List","Count")
      x = data.frame(matrix(nrow = 0, ncol = length(columns)))
      # assign column names
      colnames(x) = columns
    }else{
      x <- unique(x[c("EntryName","Entry","ProteinName","GeneNames","Organism","Length","Sequence","Position_List","Count")])
    }
    return(x)
  }
  
  
  
  customFilter <- reactive({
    if(input$select_seq == "Regex"){
      valueExpr = query_db_reg(input$Sequence)
    }else{
      valueExpr = query_db(input$Sequence)
    }
  })
  
  filtered_data <- eventReactive(input$search, customFilter())
  
  observeEvent(input$search,{
    output$sampleData <- renderDataTable(server = FALSE,{
      if(dim(filtered_data())[1] == 0){
        shinyalert("Oops!", "You have entered wrong sequence", type = "error")
      }
      input$search
      dTable(filtered_data())
    })
  })
  
  expandedDf<- function(df){
    dataSet1 <- df
    sel <- input$sampleData_rows_selected
    if(is.null(sel)){
      dfk <- dataSet1[FALSE,]
    }else{
      dfk <- dataSet1[sel, ]
      dfk <- dfk %>% separate_rows(Position_List, sep = ",")
      names(dfk)[names(dfk) == 'Position_List'] <- 'Position'
      dfk$Position <- as.integer(dfk$Position)
    }
    return(dfk)
  }
  
  rbindcounter <- reactiveVal(0)
  observeEvent(input$update, rbindcounter(rbindcounter() + 1))
  selectedDf <- eventReactive(input$refreshDb,{
    exdf <- expandedDf(filtered_data())
    #increment per click
    if(rbindcounter()>0){
      print("rbind > 0")
      rdf <- x()
      dfh <- rbind(exdf[,c('Entry','Sequence','Position')],rdf[,c('Entry','Sequence','Position')])
      dfh <- dfh[!duplicated(dfh), ]
      qry <- paste0('{',cmdMongoDb(dfh, "Entry"),",",cmdMongoDb(dfh, "Sequence"),",",cmdMongoDb(dfh, "Position"),'}')
      dfsel <- monExpandedRows$find(query = qry, fields = '{"_id":0,"Position_List" : 0}')
      isolate(dfsel)
    }else{
      print("no rbind")
      qry <- paste0('{',cmdMongoDb(exdf, "Entry"),",",cmdMongoDb(exdf, "Sequence"),",",cmdMongoDb(exdf, "Position"),'}')
      dfsel <- monExpandedRows$find(query = qry, fields = '{"_id":0,"Position_List" : 0}')
      isolate(dfsel)
    }
    
  })
  
  dfInter <- reactive({
    datac <- filtered_data()
    datac <-  datac %>%
      group_by(EntryName) %>%
      filter(n_distinct(Sequence) > 1) %>%
      ungroup
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
  
  
  observeEvent(input$addFilter,{
    if(dim(dfInter())[1] == 0){
      shinyalert("Oops!", "There are no overlapped entries for these sequences", type = "info")
    }else{
      output$interSectionData <- renderDataTable(server = FALSE,{
        datatable(dfInter(), extensions = 'Buttons', 
                  options = list(dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
      })
      output$tempdt <- renderDataTable(server = FALSE,{
        datatable(dfj(), extensions = 'Buttons', 
                  options = list(dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
      })
    }
  })
  
  output$data2 <- renderDataTable(server = FALSE,{
    df_selected <- selectedDf()
    mytable = reactive({selectedDf()})
    # set up reactive value
    reactiveData = reactiveVal()
    # observe data
    observeEvent(mytable(),{
      reactiveData(mytable())
    })
    
    proxy = dataTableProxy('data2')
    rowBind <- eventReactive(input$update,{
      copyrowdf <- x()
      dbRowUpdate(copyrowdf)
      qry <- paste0('{',cmdMongoDb(selectedDf(), "Entry"),",",cmdMongoDb(selectedDf(), "Sequence"),",",cmdMongoDb(selectedDf(), "Position"),'}')
      newData <- monExpandedRows$find(query = qry, fields = '{"_id":0,"Position_List" : 0}')
      dfh <- rbind(newData,copyrowdf)
      qrys <- paste0('{',cmdMongoDb(dfh, "Entry"),",",cmdMongoDb(dfh, "Sequence"),",",cmdMongoDb(dfh, "Position"),'}')
      newData <- monExpandedRows$find(query = qrys, fields = '{"_id":0,"Position_List" : 0}')
    })
    
    observeEvent(input$update,{
      newData <- rowBind()
      reactiveData(newData)
      replaceData(proxy, reactiveData())
      output$data2 <- renderDataTable(server = FALSE,{
        dTable(reactiveData())
      })
    })
    
    #### Merge new column
    observeEvent(input$update2,{
      newData <- reactiveData()
      get_common_cols <- function(df1, df2)  intersect(names(df1), names(df2))
      matchedCols <- get_common_cols(y(), selectedDf())
      print(length(matchedCols) == length(names(y())))
      mcols <- c("Sequence","Entry","Position")
      mcolchars <- setdiff(matchedCols,mcols)
      
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
        newData <- merge(selectedDf(), y(), by = c("Sequence","Entry","Position"),all.x = TRUE)
        dbColumnUpdate(newData)
        reactiveData(newData)
        replaceData(proxy, reactiveData(), resetPaging = FALSE)
        output$data2 = renderDataTable({
          dTable(reactiveData())
        })
      }
    })
    
    mycallback <- function(value) {
      get_common_cols <- function(df1, df2)  intersect(names(df1), names(df2))
      matchedCols <- get_common_cols(y(), selectedDf())
      print(length(matchedCols) == length(names(y())))
      mcols <- c("Sequence","Entry","Position")
      mcolchars <- setdiff(matchedCols,mcols)
      
      if(value == TRUE){
        newData <- merge(selectedDf(), y(), by = c("Sequence","Entry","Position"),all.x = TRUE)
        indx <- grepl(mcolchars, colnames(newData))
        lss <- names(newData[indx])
        newData[mcolchars] <- paste(newData[[lss[1]]],newData[[lss[2]]],sep=",")
        dropList <- lss
        newData <- newData[, !colnames(newData) %in% dropList]
        dbColumnUpdate(newData)
      }else if(value == FALSE){
        newData <- merge(x = selectedDf(), y = y(), by = c("Sequence","Entry","Position"), all = T)
        char <- paste0(mcolchars)
        char1 <- paste0(mcolchars, ".x", "")
        char2 <- paste0(mcolchars, ".y", "")
        newData[char1][!is.na(newData[char2])] <- newData[char2][!is.na(newData[char2])]
        newData[char] <- newData[char1]
        dropList2 <- c(char1, char2)
        newData <- newData[, !colnames(newData) %in% dropList2]
        dbColumnUpdate(newData)
      }
      dbColumnUpdate(newData)
      reactiveData(newData)
      replaceData(proxy, reactiveData(), resetPaging = FALSE)
      output$data2 = renderDataTable({
        dTable(reactiveData())
      })
    }
    
    output$downLoadFilter <- downloadHandler(
      filename = function() {
        paste('Filtered data-', Sys.Date(), '.csv', sep = '')
      },
      content = function(file){
        write.csv(data.frame(matrix(ncol=ncol(reactiveData()),nrow=0, dimnames=list(NULL,names(reactiveData())))),file, row.names = FALSE)
      }
    )
    
    observeEvent(input$data2_cell_edit, {
      info = input$data2_cell_edit
      newData <- selectedDf()
      newData[info$row, info$col+1] <- info$value
      reactiveData(newData)
      replaceData(proxy, reactiveData(), resetPaging = FALSE)
    })
    
    # add a column
    observeEvent(input$addColumn,{
      newData <- selectedDf()
      if(input$nameColumn %in% colnames(newData))
      {
        shinyalert("warning","The column name already exists", type = "error")
      }else{
        newData[[input$nameColumn]] <- character(length = nrow(newData))
        newData <- newData
        reactiveData(newData)
        replaceData(proxy, reactiveData(), resetPaging = FALSE)
        output$data2 = renderDataTable(server = FALSE,{
          newData <- reactiveData()
          dTable(newData)
        })
      }
    })
    # check for 'done' button press
    eventReactive(input$done, {
      newData <- reactiveData()
    })
    
    observeEvent(input$select_mathFunction,{
      dtf <- selectedDf()
      dtf$Count <- as.numeric(dtf$Count)
      dtf$Length <- as.numeric(dtf$Length)
      dtf$Position <- as.numeric(dtf$Position)
      numcolslist <- names(dtf)[sapply(dtf, is.numeric)]
      updateSelectizeInput(session, "numeric_cols", choices = numcolslist)
    })
    
    observeEvent(input$update3, {
      if(input$select_mathFunction == "Arithmetic Mean"){
        dtf <- selectedDf()
        dtf$Count <- as.numeric(dtf$Count)
        dtf$Length <- as.numeric(dtf$Length)
        dtf$Position <- as.numeric(dtf$Position)
        if(input$newcolumnname!="" && !is.null(input$newcolumnname) && input$update3>0){
          newcolval <- rowMeans(dtf[,input$numeric_cols], na.rm=TRUE)
          newcol <- data.frame(newcolval)
          names(newcol) <- input$newcolumnname
          newData <- cbind(dtf,newcol)
          dbColumnUpdate(newData)
        }
      }else if(input$select_mathFunction == "Geometric Mean"){
        dtf <- selectedDf()
        dtf$Count <- as.numeric(dtf$Count)
        dtf$Length <- as.numeric(dtf$Length)
        dtf$Position <- as.numeric(dtf$Position)
        if(input$newcolumnname!="" && !is.null(input$newcolumnname) && input$update3>0){
          newcolval <- exp(rowMeans(log(dtf[,input$numeric_cols]), na.rm=TRUE))
          newcol <- data.frame(newcolval)
          names(newcol) <- input$newcolumnname
          newData <- cbind(dtf,newcol)
          dbColumnUpdate(newData)
        }
      }else if(input$select_mathFunction == "Addition"){
        dtf <- selectedDf()
        dtf$Count <- as.numeric(dtf$Count)
        dtf$Length <- as.numeric(dtf$Length)
        dtf$Position <- as.numeric(dtf$Position)
        if(input$newcolumnname!="" && !is.null(input$newcolumnname) && input$update3>0){
          newcolval <- apply(dtf[,input$numeric_cols],1,sum)
          newcol <- data.frame(newcolval)
          names(newcol) <- input$newcolumnname
          newData <- cbind(dtf,newcol)
          dbColumnUpdate(newData)
        }
      }
      reactiveData(newData)
      replaceData(proxy, reactiveData(), resetPaging = FALSE)
      output$data2 = renderDataTable({
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
      dfNew <- reactiveData()
    })
    
    output$rowsTemplate <- downloadHandler(
      filename = function() {
        paste0("rowsTemplate.csv")
      },
      content = function(file) {
        write.csv(data.frame(matrix(ncol=ncol(reactiveData()),nrow=0, dimnames=list(NULL,names(reactiveData())))),file, row.names = FALSE)
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
    dTable(df_selected)  
  })
  
  ################################################################   
  ##################### DashBorad TabPanel  ##################### 
  ################################################################
  
  
  output$value1 <- renderValueBox({
    hide("seqplot12")
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
  observeEvent(input$genPlots,{
    show("seqplot12")
    output$seqencePlot <- renderPlotly({
      dataforPlots <- filtered_data()
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
      dataforPlots <- filtered_data()
      sequenceSumCount <- dataforPlots
      sequenceSumCount <- aggregate(sequenceSumCount$Count, by=list(Category=sequenceSumCount$Sequence), FUN=sum, na.rm=TRUE)
      colnames(sequenceSumCount)<- c("Sequence","Count")
      p2 <- ggplot(data = sequenceSumCount, 
                   aes(x=Sequence, y=Count, fill=factor(Sequence))) + 
        geom_bar(position = "dodge", stat = "identity") + ylab("Sum of Count") + 
        xlab("Sequence") + theme(legend.position="bottom" 
                                 ,plot.title = element_text(size=15, face="bold")) + 
        theme(plot.title = element_text(hjust = 0.5)) + labs(fill = "Sequence")
      ggplotly(p2)
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
    output$seqLengthnet <- renderVisNetwork({
      dataforPlots <- filtered_data()
      v1 <- dataforPlots$Sequence
      v2 <- dataforPlots$Entry
      v3 <- append(v1,v2)
      nodes <- data.frame(unique(v3),unique(v3))
      colnames(nodes) <- c("id","title")
      edges <- dataforPlots[,c(2,7)]
      colnames(edges) <- c("from", "to")
      visNetwork(nodes, edges, height = "600px", width = "100%") %>% 
        visIgraphLayout(physics = F) %>%
        visInteraction(hover = TRUE) %>%
        visEvents(hoverNode = "function(nodes) {
                Shiny.onInputChange('unique_id', nodes);
                ;}", hoverEdge = "function(edges) {
    Shiny.onInputChange('unique_id', edges);
    ;}")
    })
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
  
  observeEvent(input$Help, {
    # Absolute path to a pdf
    file.show(file.path("www/sample.pdf"))
  })
  
}

shinyApp(ui = ui, server = server)