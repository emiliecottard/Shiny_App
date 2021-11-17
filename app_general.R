# Packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)

source("C:/Users/emili/Desktop/ukdri/shiny app/global.R")

# functions of the shinyWidget package 
devtools::source_url("https://raw.githubusercontent.com/ismirsehregal/shinyWidgets/master/R/module-selectizeGroup.R")
devtools::source_url("https://raw.githubusercontent.com/ismirsehregal/shinyWidgets/master/R/module-utils.R")


##### Define UI #####

header<- dashboardHeader(title = HTML("Selective Vulnerability Meta Analysis"))

sidebar <- dashboardSidebar(
  sidebarMenu( id = "sidebar",
               menuItem('Publications by brain region', tabName = 'pub_by_region'),
               menuItem('Publications by type of cell', tabName = 'pub_by_cell'),
               menuItem('Publications by brain region and type of cell', tabName = 'pub_by_region_cell')
               ))


body <- dashboardBody(tabItems(
  tabItem( tabName = "pub_by_region",
           h3(paste0("Publications by region")),
           fluidRow(
             plotOutput("plot_region", click = "plot_region_click"),
             verbatimTextOutput("info_region"))),
  tabItem( tabName = "pub_by_cell",
           h3(paste0("Publications by type of cell")),
           fluidRow(
             plotOutput("plot_cell", click = "plot_cell_click"),
             verbatimTextOutput("info_cell"))),
  tabItem( tabName = "pub_by_region_cell",
           h3(paste0("Publications by region and type of cell")),
           fluidRow(
             plotOutput("plot_region-cell", click = "plot_region-cell_click"),
             verbatimTextOutput("info_region-cell"))
)))

ui <- dashboardPage(header, sidebar, body)


##### Define server #####

server <- function(input, output, session){
  
  # Plot Number of publication by region 
  output$plot_region <- renderPlot({
    ggplot2::ggplot(nb_by_region, mapping=aes(x=region, y=nrow, fill= quantification_method))+
      geom_bar(stat = "identity") +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            axis.text.x = element_text(size = 7, angle = 90, hjust = 1))
  })
  output$info_region <- renderPrint({
    req(input$plot_region_click)
    xvalue <- levels(as.factor(nb_by_region$region))[round(input$plot_region_click$x,0)]
    return(nb_by_region[which(nb_by_region$region == as.factor(xvalue)),])
  })
  
  
  # Plot Number of publication by cell type 
  output$plot_cell <- renderPlot({
    ggplot2::ggplot(nb_by_cell, mapping=aes(x=cell_type, y=nrow, fill= quantification_method))+
      geom_bar(stat = "identity") +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            axis.text.x = element_text(size = 7, angle = 90, hjust = 1))
  })
  output$info_cell <- renderPrint({
    req(input$plot_cell_click)
    xvalue <- levels(as.factor(nb_by_cell$cell_type))[round(input$plot_cell_click$x,0)]
    return(nb_by_cell[which(nb_by_cell$cell_type == as.factor(xvalue)),])
  })

}


##### Run the app #####
shinyApp(ui,server)

