# Packages
library(shiny)
library(shinydashboard)
library(DT)


# Get global environment 
source("C:/Users/emili/Desktop/ukdri/shiny app/global.R")


##### Define UI #####

header<- dashboardHeader(title = HTML("Selective Vulnerability Meta Analysis"))

sidebar <- dashboardSidebar(
  sidebarMenu( id = "sidebar",
               menuItem("Glossary", tabName = "glossary")
               ))

body <- dashboardBody(tabItems(
  tabItem(tabName = "glossary",
          DTOutput("glossary"))
  
))

ui <- dashboardPage(header, sidebar, body)


##### Define server #####

server <- function(input, output, session){
  
  # Glossary
  output$glossary <- renderDT(glossary)
  
}


##### Run the app #####
shinyApp(ui,server)

