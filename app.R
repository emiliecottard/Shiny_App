# Packages
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(DT)


# Import data
rawdata <- read.table("C:/Users/emili/Desktop/Imperial/shiny app/data/All_Data2.txt",
                      header = TRUE,sep = ",")

# Import glossary
glossary <- read.table("C:/Users/emili/Desktop/Imperial/shiny app/data/glossary.txt",
                      header = TRUE,sep = ",")

# Remove empty columns 
dataset <- Filter(function(x)!all(is.na(x) | x == ""), rawdata)


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
  #pageWithSidebar(
  # headerPanel(h1("Selective Vulnerability Meta Analysis \n",
  #                align = "center", style ="color: steelblue")),
  # hr(),
  sidebarPanel(
    selectizeGroupUI(
      id = "my_filters",
      inline = FALSE,
      params = list(
        year_published = list( inputId = "year_published",
                               title = tags$span(style = "color: steelblue",
                                                 "Select a year of publication"),
                               placeholder = 'Select'),
        rob_score = list( inputId = "rob_score",
                               title = tags$span(style = "color: steelblue",
                                                 "Select a rob score"),
                               placeholder = 'Select'),
        data_type = list( inputId = "data_type", 
                          title = tags$span(style = "color: steelblue",
                                            "Select a type of data"),
                          placeholder = 'Select'),
        group = list( inputId = "group", 
                      title = tags$span(style = "color: steelblue",
                                        "Select a group"),
                      placeholder = 'Select'),
        region = list( inputId = "region", 
                       title = tags$span(style = "color: steelblue",
                                         "Select a region"),
                      placeholder = 'Select'),
        stain_marker = list( inputId = "stain_marker", 
                             title = tags$span(style = "color: steelblue",
                                               "Select a stain marker"),
                              placeholder = 'Select'),
        cell_type = list( inputId = "cell_type", 
                          title = tags$span(style = "color: steelblue",
                                            "Select a type of cell"),
                          placeholder = 'Select'),
        quant_meth = list( inputId = "quantification_method", 
                           title = tags$span(style = "color: steelblue",
                                             "Select a quantification method"),
                           placeholder = 'Select')
      )
    ),
    hr(),
    pickerInput('xvar', 
                tags$span(style = "color: steelblue","Select X variable"),
                choices = colnames(dataset[,c(1:26)]), selected = "PMID"),
    pickerInput('yvar',
                tags$span(style = "color: steelblue","Select Y variable"),
                choices = colnames(dataset[,-c(1:26)]))
  ),
  mainPanel(plotOutput("plot1", click = "plot_click")
)
),
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
  
  # Glossary
  output$glossary <- renderDT(glossary)
}


###Run the app
shinyApp(ui,server)
