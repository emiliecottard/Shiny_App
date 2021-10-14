library(shinyWidgets)

### Import data
dataset <- read.table("C:/Users/emili/Desktop/Imperial/shiny app/data/dummy_data_tidy.txt",
                      header = TRUE,sep = ",")
head(dataset)

# Fixing some values 
dataset[1,5] <- "Control" ; dataset[1,5] 
summary(dataset)


# Merging groups and sub groups 
dataset$region <- c(paste(dataset$region, dataset$sub.region, sep = " "))  ; head(dataset)
dataset$group <- c(paste(dataset$group, dataset$sub.group, sep = " ")) ; head(dataset)
dataset <- subset(dataset, select = -c(sub.region,sub.group)) ; colnames(dataset)


# Removing the NA in new "Group" column
dataset$region <- gsub("NA", "", dataset$region) ; dataset$region
dataset$group <- gsub("NA", "", dataset$group) ; dataset$group

# Re-arranging columns Measurement and Value of measurement
dataset <- subset(dataset, select = -c(measurement_variable_1, measurement_variable_2,
                                       measurement_variable_3, measurement_variable_4,
                                       measurement_variable_5)) ; colnames(dataset)

# Changing columns names
colnames(dataset) <- c("Id", "Type_of_data", "Number", "Case", "Group", "Region", 
                       "Stain_marker", "Cell_type", "Quantification_method",
                       "Number or mean", "SD", 
                       "Age", "Age range","Disease duration") ; colnames(dataset)



### Define UI
  
ui <- pageWithSidebar(
  headerPanel(h1("Selective Vulnerability Meta Analysis \n", align = "center", style ="color: steelblue",
                 h3("Information on how the dataset was created ...", style ="color: steelblue"))),
  sidebarPanel(
    selectizeGroupUI(
      id = "my_filters",
      inline = FALSE,
      params = list(
        Type_of_data = list( inputId = "Type_of_data", 
                          title = tags$span(style = "color: steelblue",
                                            "Select a type of data"),
                          placeholder = 'Select'),
        group = list( inputId = "Group", 
                      title = tags$span(style = "color: steelblue",
                                        "Select a group"),
                      placeholder = 'Select'),
        region = list( inputId = "Region", 
                       title = tags$span(style = "color: steelblue",
                                         "Select a region"),
                      placeholder = 'Select'),
        stain_marker = list( inputId = "Stain_marker", 
                             title = tags$span(style = "color: steelblue",
                                               "Select a stain marker"),
                              placeholder = 'Select'),
        cell_type = list( inputId = "Cell_type", 
                          title = tags$span(style = "color: steelblue",
                                            "Select a type of cell"),
                          placeholder = 'Select'),
        quant_meth = list( inputId = "Quantification_method", 
                           title = tags$span(style = "color: steelblue",
                                             "Select a quantification method"),
                           placeholder = 'Select')
      )
    )
  ),
  mainPanel(tableOutput("table"),
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
    vars = c("Type_of_data", "Group", "Region", "Stain_marker", "Cell_type", "Quantification_method")
  )
  
  # Filtered table
  output$table <- renderTable({
    res_mod()
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
