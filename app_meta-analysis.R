# Packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)


# Get global environment 
source("C:/Users/emili/Desktop/ukdri/shiny app/global.R")

# functions of the shinyWidget package 
devtools::source_url("https://raw.githubusercontent.com/ismirsehregal/shinyWidgets/master/R/module-selectizeGroup.R")
devtools::source_url("https://raw.githubusercontent.com/ismirsehregal/shinyWidgets/master/R/module-utils.R")


##### Define UI #####

header<- dashboardHeader(title = HTML("Selective Vulnerability Meta Analysis"))

sidebar <- dashboardSidebar(
  sidebarMenu( id = "sidebar",
              menuItem("Select filters", tabName = "filters"),
               div(id = "sidebar_filters",
                    selectizeGroupUI(
                      id = "my_filters",
                      inline = FALSE,
                      params = list(
                        year_published = list( inputId = "year_published",
                                               placeholder = 'Select a year of publication'),
                        group_high_level = list( inputId = "group_high_level", 
                                                 placeholder = 'Select a group'),
                        region = list( inputId = "region", 
                                       placeholder = 'Select a region'),
                        sub.region = list( inputId = "sub.region", 
                                           placeholder = 'Select a sub region'),
                        stain_marker = list( inputId = "stain_marker", 
                                             placeholder = 'Select a stain marker'),
                        cell_type = list( inputId = "cell_type", 
                                          placeholder = 'Select a cell type'),
                        quant_meth = list( inputId = "quantification_method", 
                                           placeholder = 'Select a quantification method')
                      )
                    )),
              menuItem("Ratio", tabName = "ratio"),
              div(id = "sidebar_ratio",
                    fluidRow(
                      column(6,
                             numericInput("min_ratio",h5("Min"), 
                                          value = 0, 
                                          min = 0, 
                                          max = round(max(final_data$vect_ratio), digits = 0))),
                      column(6,
                             numericInput("max_ratio",h5("Max"), 
                                          value = round(max(final_data$vect_ratio), digits = 0), 
                                          min = 0, 
                                          max = round(max(final_data$vect_ratio)+5, digits = 0))))
                  ),
              menuItem("Standard deviation", tabName = "sd"),
              div(id = "sidebar_sd",
                    fluidRow(
                      column(6,
                             numericInput("min_sd_ratio",h5("Min"), 
                                          value = 0, 
                                          min = 0, 
                                          max = round(max(final_data$vect_sd_ratio), digits = 0))),
                      column(6,
                             numericInput("max_sd_ratio",h5("Max"), 
                                          value = round(max(final_data$vect_sd_ratio), digits = 0), 
                                          min = 0, 
                                          max = round(max(final_data$vect_sd_ratio)+5, digits = 0))))
                    )
              ))


body <- dashboardBody(tabItems(
  tabItem(tabName = "filters",
          fluidRow(plotOutput("plot1", click = "plot_click"),
                   verbatimTextOutput("info")),
  )
))

ui <- dashboardPage(header, sidebar, body)


##### Define server #####

server <- function(input, output, session){
  plot_table <- callModule(
    module = selectizeGroupServer,
    id = "my_filters",
    data = final_data,
    vars = c("year_published", "group_high_level", "region", "sub.region",
             "stain_marker", "cell_type", "quantification_method"),
    inline = FALSE
  )
  
  plot_table2 <- reactive({
    plot_table2 <- plot_table()[plot_table()$vect_ratio >= input$min_ratio &
                            plot_table()$vect_ratio <= input$max_ratio &
                            plot_table()$vect_sd_ratio >= input$min_sd_ratio &
                            plot_table()$vect_sd_ratio <= input$max_sd_ratio,]
    
  })
  
  # Create a boxplot
  output$plot1 <- renderPlot({
    ggplot2::ggplot(plot_table2(), aes(x = PMID, y = vect_ratio, color = group_high_level2)) + geom_point(size = 1) +
      geom_errorbar(aes(ymin=plot_table2()$vect_ratio-plot_table2()$vect_sd_ratio,
                        ymax=plot_table2()$vect_ratio+plot_table2()$vect_sd_ratio), width = .2) + 
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            axis.line = element_line(size = 0.5, colour = "darkgrey"),
            axis.text.x = element_text(size = 7, angle = 90, hjust = 1))
  })
  
  # Coordinates of selected points
  output$info <- renderPrint({
    req(input$plot_click)
    xvalue <- round(input$plot_click$x, 2)
    yvalue <- round(input$plot_click$y, 2)
    cat("PMID :", xvalue, "\t", "Ratio : ",yvalue)
  })
  
}


##### Run the app #####
shinyApp(ui,server)

