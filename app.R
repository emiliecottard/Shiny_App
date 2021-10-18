# Packages
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(DT)


# Import data
rawdata <- read.table("C:/Users/emili/Desktop/Imperial/shiny app/data/All_Data3.txt",
                      header = TRUE,sep = ",")

# Import glossary
glossary <- read.csv("C:/Users/emili/Desktop/Imperial/shiny app/data/glossary.txt",
                      header = TRUE,sep = "|")

# Re-arrange glossary
glossary <- glossary[-1,-c(1,5)]
glossary$Term <- gsub("`", "", glossary$Term)


# Remove empty columns 
dataset <- Filter(function(x)!all(is.na(x) | x == ""), rawdata)
dataset <- dataset[,-1]


# Merging groups and sub groups 
dataset$region <- c(paste(dataset$region, dataset$sub.region, sep = " "))  ; head(dataset)
dataset$group <- c(paste(dataset$group, dataset$sub.group, sep = " ")) ; head(dataset)
dataset <- subset(dataset, select = -c(sub.region,sub.group)) ; colnames(dataset)


# Removing unnecessary NAs in  merged columns
dataset$group <- gsub("NA", "", dataset$group) ; head(dataset$group)
dataset$region <- gsub("NA", "", dataset$region) ; head(dataset$region)


### Define UI
  
ui <- navbarPage("Selective Vulnerability Meta Analysis", id = "main_navbar",
                 tabPanel("Plot",
  sidebarPanel(h3("Select filters :", style ="color: steelblue"),
               hr(),
    selectizeGroupUI(
      id = "my_filters",
      inline = FALSE,
      params = list(
        year_published = list( inputId = "year_published",
                               placeholder = 'Select a year of publication'),
        rob_score = list( inputId = "rob_score",
                               placeholder = 'Select a rob score'),
        data_type = list( inputId = "data_type",
                          placeholder = 'Select a type of data'),
        group = list( inputId = "group", 
                      placeholder = 'Select a group'),
        region = list( inputId = "region", 
                      placeholder = 'Select a region'),
        stain_marker = list( inputId = "stain_marker", 
                              placeholder = 'Select a stain marker'),
        cell_type = list( inputId = "cell_type", 
                          placeholder = 'Select a cell type'),
        quant_meth = list( inputId = "quantification_method", 
                           placeholder = 'Select a quantification method')
      )
    )
  ),
  mainPanel(
    fluidRow(
      column(4,
              pickerInput('xvar', 
                          tags$span(style = "color: steelblue","Select X variable"),
                          choices = colnames(dataset[,c(1:26)]), selected = "PMID")
      ),
      column(4,
             pickerInput('yvar',
                          tags$span(style = "color: steelblue","Select Y variable"),
                          choices = colnames(dataset[,-c(1:26)]))
      ),
    ),
    fluidRow(plotOutput("plot1", click = "plot_click"),
             verbatimTextOutput("info")
))),
      tabPanel("Glossary",
               mainPanel(DTOutput("glossary")
               )
)
)


### Define server

server <- function(input, output, session){
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my_filters",
    data = dataset,
    vars = c("year_published", "rob_score", "data_type", "group", "region",
             "stain_marker", "cell_type", "quantification_method")
  )
  
  # Update the X and Y variable choices depending on the input
  observeEvent({
    input[["my_filters-year_published"]]
    input[["my_filters-rob_score"]]
    input[["my_filters-data_type"]]
    input[["my_filters-group"]]
    input[["my_filters-region"]]
    input[["my_filters-stain_marker"]]
    input[["my_filters-cell_type"]]
    input[["my_filters-quantification_method"]]
    },{
    res_mod2 <- Filter(function(x)!all(is.na(x) | x == ""), res_mod())
    a <- which(colnames(res_mod2) == "quantification_method")                 #Get the column number between the quantitative variables and the qualitative variables
    updatePickerInput(session, 'yvar', choices = colnames(res_mod2[,-c(1:a)]))
    updatePickerInput(session, 'xvar', choices = colnames(res_mod2[,c(1:a)]))
  })
  
  # Reactive value for the X axis 
  x <- reactive({res_mod()[,input$xvar]})
  
  # Reactive value for the Y axis 
  y <- reactive({res_mod()[,input$yvar]})
  
  # Create a boxplot
  output$plot1 <- renderPlot({
    boxplot(y() ~ x(), data = res_mod(),
            main = paste ( input$yvar, " by ",input$xvar),
            xlab = input$xvar, 
            ylab = input$yvar)
  })
  
  # Coordinates of selected points 
  output$info <- renderPrint({
    req(input$plot_click)
    xvalue <- round(input$plot_click$x, 2)
    yvalue <- round(input$plot_click$y, 2)
    cat(input$xvar," is :", xvalue, "\t",input$yvar, " is : ",yvalue)
  })
  
  # Glossary
  output$glossary <- renderDT(glossary)
}


###Run the app
shinyApp(ui,server)

