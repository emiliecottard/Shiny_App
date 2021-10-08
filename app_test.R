###Library 
library(shiny)
library(dplyr)
library(ggvis)
library(ggplot2)
library(shinythemes)

###Import data
dataset <- read.table("C:/Users/emili/Desktop/Imperial/shiny app/data/dummy_data_tidy.txt",
                      header = TRUE,sep = ",")
head(dataset)
dataset[1,5] <- "Control" ; dataset[1,5]
addNA(dataset)   #setting NAs as factors 
summary(dataset)


### Define UI

ui <- fluidPage(theme = shinytheme(theme = "sandstone"),
                titlePanel(h1("Selective Vulnerability Meta Analysis", align = "center")),
                sidebarLayout(
                  
                  # Create a spot for input 
                  sidebarPanel(width = 4,
                               selectInput("xvar","Select a qalitative variable :", 
                                           choices= c(colnames(dataset[,c(2,5,7,9,10,11)])),
                                           selected = colnames(dataset[,2])),
                               selectInput("yvar","Select a quantitative variable :",
                                           choices = c("Mean" = "value_measurement_variable_1",
                                                       "SD" = "value_measurement_variable_2",
                                                       "age"="value_measurement_variable_3", 
                                                       "age range"="value_measurement_variable_4",
                                                       "disease duration"="value_measurement_variable_5")),
                               submitButton("Update filters")),
                  
                  #Create a spot for the plot
                  mainPanel(
                    plotOutput("plot1", click = "plot_click", width = "100%")
                  )
                )
)


### Define server

server <- function(input, output,session) {
  dataInput<- reactive({
    Cropdata <- dataset[,c(input$xvar,input$yvar)]
    return(Cropdata)
  })
  output$plot1 <- renderPlot({
    ggplot(dataInput(), aes(x=input$xvar, y = input$yvar))+
      geom_point(aes(colour = input$xvar), size = 1.7)+
      labs(title = "Title of the graph",x= input$xvar, y= input$yvar)
  })
}


### Run the app

shinyApp(ui,server)