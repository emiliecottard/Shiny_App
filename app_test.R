### Library 
library(shiny)
library(shinythemes)
library(ggplot2)


### Import data
dataset <- read.table("C:/Users/emili/Desktop/Imperial/shiny app/data/dummy_data_tidy.txt",
                      header = TRUE,sep = ",")
head(dataset)

# Fixing some values 
dataset[1,5] <- "Control" ; dataset[1,5]
addNA(dataset)   #setting NAs as factors 
summary(dataset)


# Merging groups and sub groups 
dataset$region <- c(paste(dataset$region, dataset$sub.region, sep = " "))  ; head(dataset)
dataset$group <- c(paste(dataset$group, dataset$sub.group, sep = " ")) ; head(dataset)
dataset <- subset(dataset, select = -c(sub.region,sub.group)) ; colnames(dataset)

# Re-arranging columns Measurement and Value of measurement
dataset <- subset(dataset, select = -c(measurement_variable_1, measurement_variable_2,
                                         measurement_variable_3, measurement_variable_4,
                                         measurement_variable_5)) ; colnames(dataset)

# Changing columns names
colnames(dataset) <- c("Id", "Type of data", "Number", "Case", "Group", "Region", 
                        "Stain marker", "Cell type", "Quantification method",
                        "Number for individuals or mean for groups", "SD for groups", 
                        "Age", "Age range for groups","Disease duration for individuals") ; colnames(dataset)



### Define UI

ui <- fluidPage(theme = shinytheme(theme = "sandstone"),
                br(),
                titlePanel(h1("Selective Vulnerability Meta Analysis", align = "center")),
                br(),
                br(),
                sidebarLayout(
                  
                  # Create a spot for input 
                  sidebarPanel(width = 4,
                               varSelectInput("xvar","Select X variable :",
                                              dataset[,c(2,5,6,7,8,9)]),
                               varSelectInput("yvar", "Select Y variable :", 
                                              dataset[,c(10:14)])),
                  
                  #Create a spot for the plot
                  mainPanel(
                    plotOutput("plot1", click = "plot_click", width = "100%"),
                    verbatimTextOutput("info"),
                  )
                )
)


### Define server

server <- function(input, output,session) {
  output$plot1 <- renderPlot({
    ggplot(dataset, aes(x=!!input$xvar, y = !!input$yvar))+
      geom_point(aes(color = !!input$xvar),size = 1.7)+
      labs(title = paste(input$yvar, " by ", input$xvar, "\n"))+
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            axis.line = element_line(size = 0.5, colour = "darkgrey"),
            plot.title = element_text(hjust = 0.5, size = 18),
            axis.title = element_text(size = 15, face = "bold"),
            axis.text = element_text( colour = 'black',size = 14),
            axis.text.x = element_text( angle = 45, hjust = 1),
            legend.text = element_text(size = 14),
            legend.key = element_blank(),
            legend.title = element_text(size = 15, face = "bold"))
  })
  
  output$info <- renderPrint({
    req(input$plot_click)
    y <- round(input$plot_click$y, 2)
    cat(input$yvar," is : ", y, sep = "")
  })
}


### Run the app

shinyApp(ui,server)
