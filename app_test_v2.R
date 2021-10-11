### Library 
library(shiny)
library(shinythemes)
library(ggplot2)
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
dataset$Group <- gsub("NA", "", dataset$Group) ; dataset$Group

# Re-arranging columns Measurement and Value of measurement
dataset <- subset(dataset, select = -c(measurement_variable_1, measurement_variable_2,
                                         measurement_variable_3, measurement_variable_4,
                                         measurement_variable_5)) ; colnames(dataset1)

# Changing columns names
colnames(dataset) <- c("Id", "Type of data", "Number", "Case", "Group", "Region", 
                        "Stain marker", "Cell type", "Quantification method",
                        "Number or mean", "SD for groups", "Age", "Age range for groups",
                        "Disease duration") ; colnames(dataset)



### Define UI

ui <- fluidPage(theme = shinytheme(theme = "sandstone"),
                br(),
                titlePanel(h1("Selective Vulnerability Meta Analysis", align = "center")),
                br(),
                br(),
                sidebarLayout(
                  
                  # Create a spot for input 
                  sidebarPanel(width = 4,
                               selectInput("xvar","Select X variable :",
                                             choices = colnames(dataset[,c(2,5,6,7,8,9)])),
                               uiOutput("select_var1"),
                               uiOutput("select_yvar")
                               ),
                               
                  #Create a spot for the plot
                  mainPanel(
                    DTOutput("table")
                  )
                )
)



### Define server

server <- function(input, output,session) {
   
  # Must be optimized
  choice_var1 <- reactive({
        if (input$xvar == 'Type of data'){
          dataset %>% filter(`Type of data` == input$var1)
        }
        else if (input$xvar == 'Group'){
          dataset %>% filter(`Group` == input$var1)
        }
        else if (input$xvar == 'Region'){
          dataset %>% filter(`Region` == input$var1)
        }
        else if (input$xvar == 'Stain marker'){
          dataset %>% filter(`Stain marker` == input$var1)
        }
        else if (input$xvar == 'Cell type'){
          dataset %>% filter(`Cell type` == input$var1)
        }
        else if (input$xvar == 'Quantification method'){
          dataset %>% filter(`Quantification method` == input$var1)
        }
    #dataset <- dataset[,colSums(is.na(dataset))<nrow(dataset)]  #remove columns with only NAs
      })
  
  output$select_var1 <- renderUI({
        selectizeInput('var1', 'Select sub variable ', 
                  choices = c(levels(as.factor(dataset[,input$xvar]))))   
  })

  output$select_yvar <- renderUI({
    selectizeInput("yvar", "Select Y variable ",
                   choices = colnames(dataset))   # put the reactive element in the choice spot
  })

  output$table <- renderDT({
    cropdata <- choice_var1()[,-c(1:9)]   # need to remove empty columns 
    })
}


### Run the app

shinyApp(ui,server)

