





# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(DT)
require(rCharts)
#options(RCHART_LIB = 'polycharts')


sidebar <-         dashboardSidebar(
        headerPanel("Breach Dashboard ")  ,
        sliderInput(
                inputId = "years",
                label = "Years:",
                min = minYear,
                max = maxYear,
                value = c(minYear, maxYear),
                step = 1,
                sep = ""
        ),
        sidebarMenu(
               
                menuItem("Breaches by Year", tabName = "breachByYear",icon = icon("calendar")),
                menuItem("Breach by Devices", tabName = "breachByDevice", icon = icon("map")),
                menuItem("Breach Geography", tabName = "breachByGeo", icon = icon("map")),
                menuItem("Breach Data", tabName = "breachData", icon = icon("table")),
             menuItem("Help", tabName = "help", icon = icon("question-circle"))
                ),
       
        
       
        checkboxGroupInput(
                inputId = "coveredEntityType", label = h5("Covered Entity Type"),
                choices = list(
                        "Business Associate", "Health Plan", "Healthcare Clearing House","Healthcare Provider"
                ) ,
              selected = list(
                        "Business Associate", "Health Plan", "Healthcare Clearing House","Healthcare Provider"
                )
        

)
)
body <-  dashboardBody(tabItems(
        tabItem(tabName = "breachByYear",
                fluidRow(
                        box(title = "Breach Count",plotOutput("breachPlotByYear")),
                        box(title = "Breach Impact",plotOutput("breachImpactPlotByYear"))
                ),
                fluidRow(
                        box(title = "Breach Types - Count",plotOutput("breachTypePlotByYear")),
                        box(title = "Breach Types - Impact",plotOutput("breachTypeImpactPlotByYear"))
                        
                )),
        tabItem(tabName = "breachByDevice",
                fluidRow(
#                         box(title = "Breach Count",plotOutput("breachPlotByYear")),
#                         box(title = "Breach Impact",plotOutput("breachImpactPlotByYear"))
                ),
                fluidRow(
#                         box(title = "Breach Types - Count",plotOutput("breachTypePlotByYear")),
#                         box(title = "Breach Types - Impact",plotOutput("breachTypeImpactPlotByYear"))
                        
                )),
        tabItem(tabName = "breachByGeo",
                fluidRow(box(title="Breach By Geography", rCharts::chartOutput('breachesByGeo', 'datamaps'),width = 12)),
                fluidRow(box(title = "Corelation between Population and Breach Count", plotOutput("breachesPopCoorelationPlot")),
                         box(title = "Corelation between Population and Individuals Impacted", plotOutput("breachesPopCoorelation2Plot"))),
                fluidRow(box( verbatimTextOutput("breachesPopCoorelation")),
                 box(verbatimTextOutput("breachesPopCoorelation2")))
                ),
        tabItem(tabName = "breachData",
                h1("Breach Data"),
                        
                                title = "Breach Data",        
                                DT::dataTableOutput("breachData",width="100%",height="100%")
                       ),
        tabItem(tabName = "help",
                h3("Breach Dashboard Help"),
                fluidRow(box(width = 12,uiOutput("helpOverview"))),
                fluidRow(title = "Help",
                         box( title="Time Range",uiOutput("helpTimeRange")),
                             box( title="Covered Entity Type",uiOutput("helpCoveredEntity"))),
                fluidRow(box(width = 12,title = "Breach Dashboards",uiOutput("helpBreachDashboards"))),
                fluidRow(box(width = 12,title = "Breach Data",uiOutput("helpBreachData"))))
))

dashboardPage(dashboardHeader(title = "U.S. PHI Breaches"),
              
              sidebar,
              body)
 