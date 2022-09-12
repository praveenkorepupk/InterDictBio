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

ui <- dashboardPage(
  dashboardHeader(title = tags$a(tags$img(height = "25px",src="TitleAndTradeMark.png")),
                  tags$li(class="dropdown",tags$a("Help", target="_blank")),
                  tags$li(class="dropdown",tags$a("User", target="_blank")),
                  tags$li(class="dropdown",tags$a("Exit", target="_blank"))),
  
  dashboardSidebar(width = 250,
                   # textInput("Sequence", "Enter the Sequence:", ""),
                   selectizeInput("Sequence", "Enter the Sequence:",
                                  multiple = TRUE,
                                  choices = NULL),
                   column(12, actionButton("search", "Search"),align = "center")),
  
  dashboardBody(tabsetPanel(id = 'dataset',
                            tabPanel("Table", DT::dataTableOutput("sampleData"),
                                     shinyjs::useShinyjs(),
                                     shinyjs::hidden(downloadButton("downloadData", "Download Selected Rows",
                                                                    icon = icon("download"),
                                                                    style="color: #333; background-color: #FFF; border-color: #333"))),
                            tabPanel("Selected Data", DT::dataTableOutput("data2")),
                            tabPanel("Dashboard", fluidRow(br(),
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
                            tabPanel("Admin", 
                                     # DT::dataTableOutput("mytable3"),
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
                                     downloadButton("rowsTemplate", "Download Template")))),
                                     
                                     box(id = "columnID", width = 12,
                                     fluidRow(style = "padding: 5px 14px 5px 14px;
                                          margin: 5px 5px 5px 5px; ",                                         
                                     fileInput("file2", "Upload CSV To Add Columns",
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                                     actionButton("update2", "Combine Columns"),
                                     actionButton("disp2", "Display"),
                                     tableOutput("contents2"),
                                     div(style = "margin: 15px 0px 0px 0px; width:100%;"),
                                     downloadButton("columnTemplate", "Download column Template")),
                                     
                                     useShinyalert(),
                                     fluidRow(style = "padding: 40px 14px 5px 14px;
                                              margin: 5px 5px 5px 5px; ",
                                       # custom column name
                                       textInput(inputId = "nameColumn", "Enter Column Name"),
                                       actionButton(inputId = "addColumn", "Create Column"))),
                            )
  ),tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #D9D9D9;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #D9D9D9;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #D9D9D9;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #D9D9D9;
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
                              ')))
  )
)

server <- function(input, output, session) {
  mon <- mongo(collection = "entries_seperate_rows", db = "interdictbio", url = "mongodb://192.168.204.195:27017",verbose = TRUE)
  
  output$sampleData <- renderDataTable(server = TRUE,{
    df100 <- as.data.frame(mon$aggregate('[{"$limit": 1000}]'))
    df100 <- unique(df100[c("EntryName","Entry","ProteinName","GeneNames","Organism","Length","Sequence","Position_List","Count")])
    datatable(df100)
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
    datatable(df,selection = 'multiple', editable = TRUE,
              extensions = c("SearchBuilder","Buttons",'ColReorder'), #'Select'
              options = list(scrollX = TRUE,
                             search = list(regex = TRUE),
                             searching = TRUE,
                             # select = TRUE,
                             colReorder = TRUE,
                             ordering = TRUE,
                             dom = "BfQlrtip",
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
  
  proxy = dataTableProxy('sampleData')
  
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
        }else{
          newData <- rbind.data.frame(newData, x())
        }
        newData <<- newData
        reactiveData(newData)
        replaceData(proxy, reactiveData(), resetPaging = FALSE)
        output$data2 = renderDataTable(server = FALSE,{
          # datatable(reactiveData())
          dTable(reactiveData())
        })
      })
      
      #### Merge new column
      observeEvent(input$update2,{
        newData1 <- reactiveData()
        if(!exists("newData")){
          newData1 <- merge(dfk, y(), by = c("Sequence","Entry","Position_List"),all.x = TRUE)
        }else{
          newData1 <- merge(newData, y(), by = c("Sequence","Entry","Position_List"),all.x = TRUE)
          }
        newData1 <<- newData1
        reactiveData(newData1)
        replaceData(proxy, reactiveData(), resetPaging = FALSE)
        output$data2 = renderDataTable({
          # datatable(reactiveData())
          dTable(reactiveData())
        })
      })
      
      # add a column
      observeEvent(input$addColumn,{
        newData <- reactiveData()
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
      dfNew <- newData1
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
      columns = c("Sequence","Entry","Position_List") 
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
