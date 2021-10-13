### Library 
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(DT)


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


# Removing the NA in new "Group" column
dataset$group <- gsub("NA", "", dataset$group) ; dataset$group

# Re-arranging columns Measurement and Value of measurement
dataset <- subset(dataset, select = -c(measurement_variable_1, measurement_variable_2,
                                         measurement_variable_3, measurement_variable_4,
                                         measurement_variable_5)) ; colnames(dataset)

# Changing columns names
colnames(dataset) <- c("Id", "Type of data", "Number", "Case", "Group", "Region", 
                       "Stain marker", "Cell type", "Quantification method",
                       "Number or mean", "SD", 
                       "Age", "Age range","Disease duration") ; colnames(dataset)



### Define UI

ui <- fluidPage(theme = shinytheme(theme = "cerulean"),
                br(),
                titlePanel(h1("Selective Vulnerability Meta Analysis \n", align = "center",
                           h3("Information on how the dataset was created ..."))),
                hr(),
                sidebarLayout(
                  
                  # Create a spot for input 
                  sidebarPanel(width = 4,
                               selectInput("data_type", 
                                            label = tags$span(style = "color: steelblue",
                                                              "Select a type of data"),
                                           choices = levels(as.factor(dataset$`Type of data`)), 
                                           selected = 'group'),
                               varSelectInput("xvar",
                                              label = tags$span(style = "color: steelblue",
                                                                "Select X variable :"),
                                             dataset[,c(5,6,7,8,9)]),
                               varSelectInput("yvar", 
                                              label = tags$span(style = "color: steelblue",
                                                                "Select Y variable :"), 
                                              dataset[,c(10:14)]),
                               hr(),
                               htmlOutput("var_info")
                               ),
                               
                  #Create a spot for the plot
                  mainPanel(
                    plotOutput("plot1", click = "plot_click"),
                    textOutput("text"),
                    verbatimTextOutput("info"),
                    #DTOutput("table")
                  )
                )
)



### Define server

server <- function(input, output,session) {
  
  # Create a reactive table with only the rows selected with the input
  newdataset <- reactive({
      dta <- dataset %>% filter(`Type of data` == input$data_type)
      dta <- dta[,colSums(is.na(dta))<nrow(dta)]                  # delete columns with only Nas
  })
  
  # Update the choices of the Y variable depending on the type of data chosen
  observeEvent(input$data_type,updateVarSelectInput(session, 'yvar', data = newdataset()[,-c(1:8)]))
  
  
  #Creating a plot 
  output$plot1 <- renderPlot({
   ggplot(newdataset(), aes(x=!!input$xvar, y = !!input$yvar)) + 
      geom_point(aes(color = !!input$xvar),size = 1.7) +    
      labs(title = paste(input$yvar, " by ", input$xvar, "\n for ", input$data_type)) +
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
  
  # Instructions for the graph
  output$text <- renderText(print("Click on the points on the graph to get the exact value"))
  
  # Instructions on the variables
  output$var_info <- renderUI({
    str1 <- h5(strong("X variables : "))
    str2 <- h6("Type of data : ...")
    str3 <- h6("Group : ...")
    str4 <- h6("Region : ...")
    str5 <- h6("Stain marker : ...")
    str6 <- h6("Cell type : ...")
    str7 <- h6("Quantification method : ...")
    str8 <- h5(strong("Y variables :"))
    str9 <- h6("Number or mean : ...")
    str10 <- h6("SD : ...")
    str11 <- h6("Age : ...")
    str12 <- h6("Age range : ...")
    str13 <- h6("Disease duration : ...")
    HTML(paste(str1,str2,str3,str4,str5,str6,str7,
               str8,str9,str10,str11,str12,str13,sep = '\n'))
  })
  
  # Shows the value of the selected point on the graph
  output$info <- renderPrint({
    req(input$plot_click)
    y <- round(input$plot_click$y, 2)
    cat(input$yvar," is : ", y, sep = "")
  })
  
  # Data table with the selected rows and columns
  # output$table <- renderDT({
  #   newdataset()                                        
  #   })
}


### Run the app

shinyApp(ui,server)

