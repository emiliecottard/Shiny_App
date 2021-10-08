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
                br(),
                titlePanel(h1("Selective Vulnerability Meta Analysis", align = "center")),
                br(),
                br(),
                sidebarLayout(
                  
                  # Create a spot for input 
                  sidebarPanel(width = 4,
                               varSelectInput("xvar","Select X variable :", dataset[,c(2,5,7,9,10,11)]),
                               varSelectInput("yvar", "Select Y variable :", dataset[,c(13,15,17,19,21)]),
                               hr(),
                               submitButton("Update filters")),
                  
                  #Create a spot for the plot
                  mainPanel(
                    plotOutput("plot1", click = "plot_click", width = "100%")
                  )
                )
)


### Define server

server <- function(input, output,session) {
  output$plot1 <- renderPlot({
    ggplot(dataset, aes(x=!!input$xvar, y = !!input$yvar))+
      geom_point(aes(color = !!input$xvar),size = 1.7)+
      labs(title = paste(input$xvar, " by ", input$yvar, "\n"))+
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            axis.line = element_line(size = 0.5, colour = "darkgrey"),
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.title = element_text(size = 14),
            axis.text = element_text( colour = 'black',size = 14),
            axis.text.x = element_text( angle = 45, hjust = 1),
            legend.text = element_text(size = 14),
            legend.key = element_blank(),
            legend.title = element_text(size = 16))
  })
}


### Run the app

shinyApp(ui,server)
