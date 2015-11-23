

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(scales)
library(ggplot2)
library(rMaps)
library(rCharts)
#library(leaflet)
library(RColorBrewer)
library(maps)
library(mapproj)
library(DT)
source("plotStates.R")

formatNumbers <- function(y){
       formatNumber <-  function(x) {
                if(x< 1e+03){return(comma(x))}
                if(x < 1e+06) {return(paste(x/1e03,"T"))}
                if(x < 1e+09) {return(paste(x/1e06,"M"))}
                if(x >= 1e+09) {return(paste(x/1e09,"B"))}
        }
        sapply(y,formatNumber )}

# if (packageVersion('shiny') > '0.7') {
#         library(shiny)
#         runGitHub("shiny-examples", "rstudio", subdir = "012-datatables")
# }

#require(devtools)
#install_github('ramnathv/rCharts@dev')
#install_github('ramnathv/rMaps')
#devtools::install_github('rstudio/shinyapps')
#devtools::install_github('rstudio/DT')
#install.packages("leaflet")

function(input, output) {
        breach.filtered <- reactive({
                 selectedEntityTypes <- input$coveredEntityType
                range <- input$years
                firstYear <- range[1]
                #   message(firstYear)
                lastYear <- range[2]
                #   message(lastYear)
                breachesRange <-
                        breaches %>% filter((Breach.Year >= firstYear) &
                                                    (Breach.Year <= lastYear))
         
                breachesRange <-
                        breachesRange %>% filter(Covered.Entity.Type %in% selectedEntityTypes)
                 
                breachesRange
        })
        
        
        breachTypesReactive <- reactive ({
                subsetOfBreaches <- breach.filtered()
                breachTypesByYear <-
                        subsetOfBreaches[,c(
                                "Breach.Year","Covered.Entity.Type","Individuals.Affected",names(df)
                        )]
                
                breach.types <-
                        breachTypesByYear %>% gather(
                                Breach.X,Breach.Type,-Breach.Year,-Covered.Entity.Type,-Individuals.Affected,na.rm =
                                        TRUE
                        )
                breach.types <- breach.types %>% select(-Breach.X)
                breach.types
        })
        populationBreachCountFit <- reactive( {
                stateBreaches <- stateBreachReactive() 
                fit <- lm(count ~ Population, data=stateBreaches)
                fit
        })
        
       populationBreachImpactFit <- reactive( {stateBreaches <- stateBreachReactive() 
        fit <- lm(Individuals.Affected ~ Population , data=stateBreaches)})
        
        stateBreachReactive <- reactive ( {
                breach.filtered() %>%filter(!is.na(Individuals.Affected)&!is.na(State))%>%
                        group_by(State )%>%
                        #                  group_by(State,Breach.Year )%>%
                        summarize(Individuals.Affected=sum(Individuals.Affected),count=n())%>%
                        mutate( State.Name =state.name[match(State,state.abb)] ,
                                Population = (statePop$POPESTIMATE2014)[match(State,statePop$STUSAB)] ,
                                popup = sprintf("<strong>State:</strong> %s <br/><strong>Impacted:</strong> %s <br/><strong>Count:</strong> %s<br/><strong>Population: </strong>%s", 
                                                State.Name, prettyNum(Individuals.Affected,big.mark=",") ,prettyNum(count,big.mark=","),prettyNum(Population,big.mark=",")) 
                        )%>%ungroup()%>%
                        arrange( State)%>%
                        filter(State != "DC")})
        
        output$breachPlotByYear <- renderPlot({
                breachesInRange <- breach.filtered()
                if (nrow(breachesInRange) > 0) {
                        ggplot(
                                breachesInRange, aes(
                                        x = as.factor(Breach.Year),fill = Covered.Entity.Type
                                )
                        ) +
                                geom_histogram(color = "black") +
                                xlab("Year - Breach Submitted") +
                                ylab("Number of Breaches") +
                                
                                theme_bw() +
                                theme(axis.text.x = element_text(angle = 45,vjust = .5))
                } else {
                        "Failed"
                }
                
        })
        output$breachImpactPlotByYear <- renderPlot({
                 breachesInRange <- breach.filtered()
                if (nrow(breachesInRange) == 0) return()
                
                breachesRange <-
                        breachesInRange %>% filter(!is.na(Covered.Entity.Type) &
                                                           !is.na(Individuals.Affected))  %>%
                        group_by(Breach.Year,Covered.Entity.Type) %>% summarise(impacted =
                                                                                        sum(Individuals.Affected)) %>% ungroup()
                if (nrow(breachesRange) > 0) {
                        ggplot(
                                breachesRange, aes(
                                        x = as.factor(Breach.Year),y = impacted ,fill = Covered.Entity.Type
                                )
                        ) +
                                geom_bar(stat = "identity" ,color = "black") +
                                scale_y_continuous(labels=comma)+
                                xlab("Year - Breach Submitted") +
                                ylab("Number of Individuals Impacted") +
                                theme_bw() +
                                theme(axis.text.x = element_text(angle = 45,vjust = .5))
                } else {
                        "Failed"
                }
                
        })
        
        output$breachTypePlotByYear <- renderPlot({
                breachesRange <- breachTypesReactive()
                if (nrow(breachesRange) == 0) return()
                
                breachesRange <-
                        breachesRange %>% filter(!is.na(Covered.Entity.Type) &
                                                         !is.na(Individuals.Affected))
                if (nrow(breachesRange) > 0) {
                        ggplot(breachesRange, aes(
                                x = as.factor(Breach.Year),fill = Breach.Type
                        )) +
                                geom_histogram(color = "black") +
                                scale_y_continuous(labels=comma)+
                                xlab("Year - Breach Submitted") +
                                ylab("Breach Type - Count") +
                                theme_bw() +
                                theme(axis.text.x = element_text(angle = 45,vjust = .5))
                } else {
                        "Failed"
                }
                
        })
        output$breachTypeImpactPlotByYear <- renderPlot({
                breachesRange <- breachTypesReactive()
                if (nrow(breachesRange) == 0) return()
                
                breachesRange <-
                        breachesRange %>% filter(!is.na(Covered.Entity.Type) &
                                                         !is.na(Individuals.Affected)) %>% group_by(Breach.Year,Breach.Type) %>% summarize(Individuals.Affected =
                                                                                                                                                   sum(Individuals.Affected))
                
                breachesRange[which(is.na(breachesRange$Individuals.Affected)),"Individuals.Affected"] <-0
                
                 
               
                        ggplot(
                                breachesRange, aes(
                                        x = as.factor(Breach.Year),y = Individuals.Affected ,fill = Breach.Type
                                )
                        ) +
                                geom_bar(stat = "identity" ,color = "black") +
                                scale_y_continuous(labels=comma)+
                                xlab("Year - Breach Submitted") +
                                ylab("Number of Individuals Impacted") +
                                theme_bw() +
                                theme(axis.text.x = element_text(angle = 45,vjust = .5))
         
                
        })
        output$breachesByGeo <- renderChart2({
                stateBreaches <- stateBreachReactive() 
                ichoropleth(Individuals.Affected ~ State,
                            data = stateBreaches,
                            pal = 'PuRd',
                            ncuts = 5,
                         geographyConfig = list(
                                popupTemplate = "#! function(geography, data){
        Shiny.onInputChange('State', geography.properties.name)
        return '<div class=hoverinfo><strong>' + data.popup + '</strong></div>';
      } !#"))

                        
                })
        output$breachesPopCoorelation <- renderPrint({
                fit <- populationBreachCountFit() #Reactive
                (summary(fit)) # show results
        })
        output$breachesPopCoorelationPlot <- renderPlot({
                fit <- populationBreachCountFit()
                plot(fit)
        })
        output$breachesPopCoorelation2 <- renderPrint({
               fit <- populationBreachImpactFit()
                (summary(fit)) # show results
        })
        output$breachesPopCoorelation2Plot <- renderPlot({
                fit <- populationBreachImpactFit()
                plot(fit)
        })
        
        output$breachBySourceOfDataImpact <- renderPlot({
                breachesInRange <- breach.filtered()
                if (nrow(breachesInRange) == 0) return()
                
                breachesRange <-
                        breachesInRange %>% filter(!is.na(Covered.Entity.Type) &
                                                           !is.na(Individuals.Affected))  %>%
                        group_by(Breach.Year,location.of.breached.info.1) %>% summarise(impacted =
                                                                                        sum(Individuals.Affected)) %>% ungroup()
                if (nrow(breachesRange) > 0) {
                        ggplot(
                                breachesRange, aes(
                                        x = as.factor(Breach.Year),y = impacted ,fill = location.of.breached.info.1
                                )
                        ) +
                                geom_bar(stat = "identity" ,color = "black") +
                                scale_y_continuous(labels=comma)+
                                xlab("Year - Breach Submitted") +
                                ylab("Number of Individuals Impacted") +
                                theme_bw() +
                                theme(axis.text.x = element_text(angle = 45,vjust = .5))
                } else {
                        "Failed"
                }
                
        })
        output$breachBySourceOfDataCount <- renderPlot({
                breachesInRange <- breach.filtered()
                if (nrow(breachesInRange) == 0) return()
                
                breachesRange <-
                        breachesInRange %>% filter(!is.na(Covered.Entity.Type) &
                                                           !is.na(Individuals.Affected))  %>%
                        count(Breach.Year,location.of.breached.info.1) 
                if (nrow(breachesRange) > 0) {
                        ggplot(
                                breachesRange, aes(
                                        x = as.factor(Breach.Year),y = n ,fill = location.of.breached.info.1
                                )
                        ) +
                                geom_bar(stat = "identity" ,color = "black") +
                                scale_y_continuous(labels=comma)+
                                xlab("Year - Breach Submitted") +
                                ylab("Number of Breaches") +
                                theme_bw() +
                                theme(axis.text.x = element_text(angle = 45,vjust = .5))
                } else {
                        "Failed"
                }
                
        })
        output$breachData <- DT::renderDataTable({
                bdata <- breach.filtered()[,1:10]
                if(nrow(bdata) == 0)return(datatable(bdata))
               
                datatable(bdata , extensions = c("ColReorder",'ColVis','Responsive' ), options = list(dom = 'RC<"clear">lfrtip',pageLength=20, autoWidth = TRUE,
                                                                                         colVis = list(exclude = c(0, 1), activate = 'mouseover')
                ) )

                })
        output$helpOverview <- renderUI({div(tags$link(rel="stylesheet",type="text/css",href="help.css"),
                                             p("The Healthcare industry has been at risk for some time related to security breaches.  This dashboard shows the breaches that have occurred between 2009 and 2015.  The data sources is from the federal government.  Healthcare Covered entities should use this dashboard to interpret where the majority of vulnerabilities occur, and how this is changing over time."),
p("As required by section 13402(e)(4) of the HITECH Act, the Secretary must post a list of breaches of unsecured protected health information affecting 500 or more individuals. These breaches are now posted in a new, more accessible format that allows users to search and sort the posted breaches. Additionally, this new format includes brief summaries of the breach cases that OCR has investigated and closed, as well as the names of private practice providers who have reported breaches of unsecured protected health information to the Secretary. The breaches seen in this dashboard have been reported to the Secretary:
"))})
        output$helpTimeRange <- renderUI({
                (div(
                        p("Personal Health Information Breaches have been tracked by the Office of the National Coordinator for Health Information Technology (ONC) of the United States government since the year 2009. The time range is controlled by the Years slider in the left side panel. "),
                        tags$table(tags$tr(tags$td(img(src="yearRange.png", height=200,width=100)),
                                           tags$td(""),
                                           tags$td(class="howTo", " To select the range of time that you would like to investigate, use the  'Years:` slider to select the start and finish times.  This allows you to control all tabs on the Breach Dashboard.")))
                        
                )
                )
                
        })
        output$helpCoveredEntity <- renderUI({
                (div(
                        p("Covered entities are defined in the HIPAA rules as (1) health plans, (2) health care clearinghouses, and (3) health care providers who electronically transmit any health information in connection with transactions for which HHS has adopted standards. "),
                        tags$table(tags$tr(tags$td(img(src="coveredEntityTypes.png", height=200,width=100)),
                                           tags$td(""),
                                           tags$td(class="howTo", " To select the covered entity type that you would like to investigate, use the  'Covered Entity Type:` checkbox to select the startentity types.  This allows you to control all tabs on the Breach Dashboard.")))
                        
                )
                )
                
        })
        output$helpBreachDashboards <- renderUI({
                (div(tags$table(tags$thead(tags$th("Dashboard"),tags$th("      "),tags$th("Description")),
                                tags$tr(tags$td(class="helpName","Breaches by Year"),
                                        tags$td(""),
                                        tags$td(class="howTo", "This dashboard displays the volumes of breaches, and the number of individuals impacted by year. 
                                                The top row segments by Covered Entity Type, and the bottom row segments by Breach Type.  
                                                The observations are filtered b the Years and Covered Entity Type checkboxes in the left hand side panel.")),
                     tags$tr(tags$td(class="helpName","Breaches by Device"),
                             tags$td(""),
                             tags$td(class="howTo", "This shows the source where the data was lost or stolen from.   It includes devices such as servers, laptops, and desktops.  It also includes other types of sources like loosing data via email, or a paper record.  The observations are filtered by the Years and Covered Entity Type checkboxes in the left hand side panel.")),
                     tags$tr(tags$td(class="helpName","Breaches Geography"),
                             tags$td(""),
                             tags$td(class="howTo", " This dashboard shows the U.S. states with breaches.  The bottom of the dashboard looks at the correlation between Breach Count and Breach Impact vs. the population of the state.  There looks to be a significant coorelation between population and count.  The observations are filtered by the Years and Covered Entity Type checkboxes in the left hand side panel."))
                )
                )
                )
                
        })
        output$helpBreachData <- renderUI({
                (div(
                        p("The Breach Data is made available by the Office of the National Coordinator on a frequent basis through the " ,
                          tags$a(href="https://ocrportal.hhs.gov/ocr/breach/breach_report.jsf", target="_blank", "website."),
                          tags$div("You can select the columns that you want to display in the data table by clicking on the column when you place your cursor over the 'Show/hide columns' button in the upper right corner.  Columns that are not displayed that have a check mark can be displayed by selecting the '+' circle icon.")
                          ,margin=10)
                            
                )
                )
                
        })
}
       


