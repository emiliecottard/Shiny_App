# Packages
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(DT)


### Import data
dataset <- read.table("C:/Users/emili/Desktop/Imperial/shiny app/data/All_Data.txt",
                      header = TRUE,sep = ",")
dataset <- dataset[which(dataset$year_published %in% 2021),]                   #only working on 2021 data
dataset <- dataset[,colSums(is.na(dataset))<nrow(dataset)] ; head(dataset)     #deleting columns with only NAs
dataset <- dataset[,-c(28:30)]; head(dataset)

# Merging groups and sub groups 
dataset$region <- c(paste(dataset$region, dataset$sub.region, sep = " "))  ; head(dataset)
dataset$group <- c(paste(dataset$group, dataset$sub.group, sep = " ")) ; head(dataset)
dataset <- subset(dataset, select = -c(sub.region,sub.group)) ; colnames(dataset)


# Removing the NA in new "Group" column
dataset$group <- gsub("NA", "", dataset$group) ; head(dataset$group)
dataset$region <- gsub("NA", "", dataset$region) ; head(dataset$region)

# Re-arranging columns Measurement and Value of measurement
dataset <- dataset[,-c(18,20,22,24)] ; colnames(dataset)

# Changing columns names
# colnames(dataset) <- c("Id", "Type of data", "Number", "Case", "Group", "Region", 
#                        "Stain marker", "Cell type", "Quantification method",
#                        "Number or mean", "SD", 
#                        "Age", "Age range","Disease duration") ; colnames(dataset)


### Define UI
  
ui <- pageWithSidebar(
  headerPanel(h1("Selective Vulnerability Meta Analysis \n", align = "center", style ="color: steelblue",
                 h3("Information on how the dataset was created ...", style ="color: steelblue"))),
  sidebarPanel(
    selectizeGroupUI(
      id = "my_filters",
      inline = FALSE,
      params = list(
        Type_of_data = list( inputId = "data_type", 
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
    pickerInput('xvar', 
                tags$span(style = "color: steelblue","Select X var"),
                choices = colnames(dataset[,c(1:17)]), selected = "PMID"),
    pickerInput('yvar',
                tags$span(style = "color: steelblue","Select Y var"),
                choices = colnames(dataset[,-c(1:17)]), selected = "value_measurement_1")
  ),
  mainPanel(#DTOutput("table"),
            plotOutput("plot1", click = "plot_click"),
            hr(),
            fluidRow(
              column(4,
                     htmlOutput("quanti_var_info")),
              column(4, 
                     htmlOutput("quali_var_info")))
)
) 


### Define server

server <- function(input, output, session){
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my_filters",
    data = dataset,
    vars = c("data_type", "group", "region", "stain_marker", "cell_type", "quantification_method")
  )
  
  # # Filtered table
  # output$table <- renderDT({
  #   res_mod()
  # })

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
  
  # Instructions on the variables
  output$quanti_var_info <- renderUI({
    str1 <- h5(strong("Quantitative variables : "))
    str2 <- h6("Type of data : ...")
    str3 <- h6("Group : ...")
    str4 <- h6("Region : ...")
    str5 <- h6("Stain marker : ...")
    str6 <- h6("Cell type : ...")
    str7 <- h6("Quantification method : ...")
    HTML(paste(str1,str2,str3,str4,str5,str6,str7,sep = '\n'))
  })
  
  output$quali_var_info <- renderUI({
    str1 <- h5(strong("Qualitative variables :"))
    str2 <- h6("Number or mean : ...")
    str3 <- h6("SD : ...")
    str4 <- h6("Age : ...")
    str5 <- h6("Age range : ...")
    str6 <- h6("Disease duration : ...")
    HTML(paste(str1,str2,str3,str4,str5,str6,sep = '\n'))
  })
}


###Run the app
shinyApp(ui,server)
