pacman::p_load(
  shiny,        # dashboard  
  shinydashboard,   # dashboard  
  janitor,      # data cleaning and tables
  matchmaker,   # dictionary-based cleaning
  epikit,       # age_categories() function
  tidyverse,    # data management and visualization
  shinythemes,  # shiny themes
  plotly,       # plotting
  pastecs,      # descriptive statistics
  shinyWidgets,
  readxl,
  skimr,
  shinyjs,
  bs4Dash,
  waiter,
  rsconnect, 
  DT
)

options(shiny.maxRequestSize = 10 * 1024^2)

#build a sidebar menu
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Input File",tabName = "input_file", icon = icon('database')),
    menuItem("Visualize",tabName = "plotting", icon = icon("chart-bar",lib = "font-awesome")),
    menuItem("About Me", icon = icon('linkedin'), href = "https://www.linkedin.com/in/rafael-simson-riston-332260192/"),
    menuItem("Source Code", icon= icon("github"))
  )
)



body <- dashboardBody( #use_theme(dark_theme),
  #collection of content in every sidebar menu
  
  tabItems(
    
    # input file menu
    tabItem(tabName = "input_file",
            fluidPage(
              fluidRow(
                box(status = "primary", solidHeader = TRUE, title = "Input Table", width = 12,
                    column(6,
                           radioButtons("file_type", "Choose File Type",
                                        choices = c("Excel" = "xlsx", "CSV" = "csv"), 
                                        selected = "csv")),
                    
                    
                    column(6,fileInput("file", "Upload a File", 
                                       multiple = F,
                                       accept = c(".csv", ".xlsx")))
                    
                ) # fluid row
              ),
              fluidRow( 
                box(status = "primary", solidHeader = TRUE, title = "Table", width = 12, icon = icon('table-list', lib = "font-awesome"),
                    column(12, 
                           DT::dataTableOutput("data")))
              ),
              
              fluidRow(
                tabBox( width= 12,
                        tabPanel(
                          title = "Basic Statistics", icon = icon('calculator',lib = "font-awesome"),
                          verbatimTextOutput("summary") 
                        ),
                        tabPanel(
                          title = "info data", icon = icon('info'),
                          DT::dataTableOutput("info")
                        )
                )
              ) # fluid row
            )# fluid Page
            
    ),#tab item
    
    # plotting menu
    tabItem(tabName = "plotting",
            fluidPage(
              fluidRow(
                box(title="Silahkan Pilih Diagram", status="primary", solidHeader = TRUE, width = 12,
                    column(4,selectInput("select_plot", label = "Jenis Diagram",
                                         choices = c("Bar"="bar" ,"Line"="line",
                                                     "Pie"="pie",
                                                     "Scatter Plot"="scatter"))),
                    column(3,uiOutput("xcol_all")),
                    column(3,uiOutput("ycol_all")),
                    column(2, "Cleaning Data",
                           prettyCheckbox(
                             inputId ="snake" , label = "Rename columns to snake case?", icon = icon("thumbs-up"), 
                             status = "default", shape = "curve", animation = "pulse"
                           ),
                           prettyCheckbox(
                             inputId ="empty" , label = "Do you wanna keep empty data ?", icon = icon("thumbs-up"), 
                             status = "default", shape = "curve", animation = "pulse"
                           )
                           
                    )
                )
              ),
              
              fluidRow(
                tabBox( width = 12, height = 750,
                        tabPanel( 
                          title="Diagram", icon = icon('chart-line', lib = "font-awesome"),
                          uiOutput("all_plot"), width = 12,
                          dropdownButton(
                            tags$h4("Input Variables"),
                            uiOutput("hue_all"),
                            status = "danger",
                            icon = icon("gear", width="300px"),
                            tooltip = tooltipOptions(title = "Click to see inputs !"),
                            circle = TRUE,
                            margin = 50
                            
                            
                          )
                        ),
                        
                        tabPanel(
                          title = "Table Update", icon=icon('database'),
                          DT::dataTableOutput("update_table")
                        ),
                        tabPanel(
                          title = "Histogram", icon=icon("chart-simple",lib = "font-awesome"),
                          uiOutput("hist_all")
                        )
                )
              )
              
            )
    )
    
  ) # tab items
)#body





#ui
ui <- dashboardPage(title = 'Rafael Simson Riston', 
                    preloader = list(html = tagList(spin_6(), "Loading ..."), color = "#3c8dbc"),
                    dashboardHeader(title = 'Dashboard'),
                    sidebar,
                    body
)
#server
server <- function(input, output, session) {
  # hide pre-loader
  shinyjs::delay(2000, {waiter_hide()})
  showNotification("Created by Rafael Simson Riston", duration = NULL, type = 'error')
  
  # Tab Menu Input File
  # data ................................................................................
  data <- reactive({
    req(input$file)
    
    ext <- input$file_type
    
    switch(ext,
           "xlsx" = as.data.frame(read_excel(input$file$datapath)),
           "csv" = as.data.frame(read.csv(input$file$datapath)),
           validate("Invalid file; Please upload a .csv or .xlsx file"))
  })
  
  # data frame output ......................................................................
  output$data <- DT::renderDataTable({
    data()
  },options = list(lengthMenu=c(10,30,50,100), pageLength=10, scrollX=T))
  
  # data info
  output$info <- DT::renderDataTable({input$file},
                                     options = list(lengthMenu=c(10,30,50,100), 
                                                    pageLength=10, scrollX=T))
  
  
  # summary output .......................................................................
  output$summary <- renderPrint({
    skim(data())
  })
  
  
  # Tab Menu Ploting
  
  # na remove
  
  clear.data <- reactive({
    new <- data()
    if (input$snake){
      names(new) <- make_clean_names(names(new))
    }
    if (input$empty){
      new <- new %>% na.omit()
    }
    
    
    new
  })
  
  
  #update data
  output$update_table <- DT::renderDataTable({
    clear.data()
  },options = list(lengthMenu=c(10,30,50,100), pageLength=10, scrollX=T))
  
  
  #Input Variables
  output$xcol_all <- renderUI({
    selectInput("selected_x", "X - Axis", names(clear.data()))
  })
  
  output$ycol_all <- renderUI({
    selectInput("selected_y", "Y - Axis", names(clear.data()), selected = names(clear.data())[2])
  })
  
  
  #Input Hue
  output$hue_all <- renderUI({
    selectInput("selected_hue", "hue", names(clear.data()))
  })
  
  #plotting
  show.plot <- reactive({
    
    databaru <- clear.data()
    plot.Select <- input$select_plot
    
    x_data <- databaru[[input$selected_x]]  
    y_data <- databaru[[input$selected_y]]
    
    if (plot.Select=="bar") {
      
      outplot <- plot_ly(x=x_data, y=y_data, type='bar', height = 700)
      
    }
    if (plot.Select=="line") {
      
      outplot <- plot_ly(x=x_data, y=y_data, type = 'scatter', mode = 'lines+markers', height = 700)
      
    }
    if (plot.Select=="pie") {
      
      outplot <- plot_ly(labels=x_data, values=y_data, type='pie', height = 700)
      
    }
    
    if (plot.Select=="scatter") {
      
      outplot <- plot_ly(x=x_data, y=y_data,  type = 'scatter', height = 700)
      
    }
    
    
    outplot
    
  })
  
  output$all_plot <- renderUI({
    show.plot()
  })
  
  
  
  #create subplot histogram
  
  #select numeric col_data
  
  hist.plot <- reactive({
    data.num <- select_if(clear.data(), is.numeric)
    
    #looping
    
    n.row <- length(data.num)/2
    single.plot <- c()
    
    for (name in names(data.num)){
      single.plot[name] <- plot_ly(x= data.num[[name]], type='histogram', height = 1500, name=name)
    }
    fig <- subplot(single.plot, nrows = round(n.row)) %>%
      layout(title = 'Histogram Numeric Variable')
    
    fig
  })
  
  
  output$hist_all <- renderUI({
    hist.plot()
  })
  
}

shinyApp(ui, server)