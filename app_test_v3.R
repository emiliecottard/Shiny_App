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


shinyApp(
  ui <- pageWithSidebar(
    headerPanel(h1("Selective Vulnerability Meta Analysis \n", align = "center",
                   h3("Information on how the dataset was created ..."))),
    sidebarPanel(
      selectizeGroupUI(
        id = "my_filters",
        inline = FALSE,
        params = list(
          type_of_data = list( inputId = "Type_of_data", 
                            title = "Select Type of data", placeholder = 'Select'),
          group = list( inputId = "Group", 
                        title = "Select group", placeholder = 'Select'),
          region = list( inputId = "Region", 
                        title = "Select region", placeholder = 'Select'),
          stain_marker = list( inputId = "Stain_marker", 
                         title = "Select stain marker", placeholder = 'Select'),
          cell_type = list( inputId = "Cell_type", 
                         title = "Select type of cell", placeholder = 'Select'),
          quant_meth = list( inputId = "Quantification_method", 
                         title = "Select method of quantification", placeholder = 'Select')
        )
      )
      
    ),
    mainPanel(tableOutput("table")
  )
  ), 

server = function(input, output, session){
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my_filters",
    data = dataset,
    vars = c("Type_of_data", "Group", "Region", "Stain_marker", "Cell_type", "Quantification_method")
  )
  output$table <- renderTable({
    res_mod()
  })
}, option = list(height = 500)
)
