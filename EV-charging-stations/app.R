library(shiny)
library(tidyverse)
library(ggplot2)
library(readr)
library(sf)
library(tidycensus)
library(datasets)
library(readr)
library(broom)
library(gt)
library(shinythemes)
library(plotly)
library(leaflet)
options(scipen=999)

ui <- navbarPage(theme = shinytheme("flatly"), "EV Charging Stations",
                 tabPanel("Home",
                          h2("Welcome!", align = "center"),
                          p("Electric vehicles are the future — but for whom? This project examines the locations 
                          of more than 28,000 EV charging stations in the U.S., comparing their frequency to the Census 
                          demographics, such as median household income, of the county they are located in. On this 
                          website, I present state-by-state maps of this relationship, as well as a statistical 
                            analysis of the nationwide data.", 
                            align = "center"),
                          p("Though not reaching the level of statistical significance, I found a slight
                            positive correlation between median income and charging stations in a county. 
                            This means that, generally, counties with a higher median income receive a higher share
                            of EV infrastructure investment.",
                            align = "center"),
                          p("This has important policy implications. Investment in EV infrastructure is investment
                            in the future of transportation, and it should be going towards reducing inequality, not
                            exacerbating it. Passing over less wealthy areas for stations will leave residents there
                            without the flexibility to buy an electric vehicle.",
                            align = "center"),
                          br(),
                          plotOutput("all_stations_graphic", width = "100%", height = "100%"),
                          br(),
                          p("The locations of all 28,000+ charging stations nearly draw a map of the U.S. by themselves,
                            with a few notable gaps.",
                            align= "left")
                        
                 ),
                 
                 tabPanel("Graphs by State",
                          sidebarLayout(
                              sidebarPanel(
                                  h4("Interactive Map"),
                                  p("Toggle your desired state and demographic attribute."),
                                  selectInput(inputId = "state",
                                              label = "State",
                                              choices = state.name, selected = "Massachusetts"),
                                  
                                  radioButtons(inputId = "attribute", 
                                               label = "Demographic Attribute",
                                               choices = list("Median Income" = 1, 
                                                              "Percentage of Rural Residents" = 2, 
                                                              "Vehicles Per Capita" = 3, 
                                                              "Total Population" = 4, 
                                                              "Percentage of Adults with Bachelor's Degree or Higher" = 5), 
                                               selected = 1)
                              ),
                              
                              mainPanel(
                                  h4(textOutput("map_title")),
                                  leafletOutput("state_maps"),
                                  h6("Source: U.S. Census 2010, American Communities Survey 2018, and 
                                     National Renewable Energy Laboratory database")
                                  
                              )
                          )),
                          
                          tabPanel("Regression Model",
                                       mainPanel(
                                                h2("Graph of Relationship"),
                                             p("I ran a linear regression to explore the relationship
                                                        between median household income in a county and the number
                                                        of EV stations built there. The resulting best-fit line is
                                                        plotted."),
                                                        plotlyOutput("stats"),
                                              h2("Statistical Relationship"),
                                              h4("Linear Regression and Interpretation"),
                                                        tableOutput("stats_table"),
                                             p("This table shows my average coefficient, or the slope of the regression
                                                  line, along with a 95% confidence interval. Because this was not a randomized
                                                  experiment, we are not able to use this model to determine a causal relationship
                                                  or predict the number of stations in any given county. There are so many possible confounding
                                                  variables that we cannot say for certain that a higher median household income is causing a county to have
                                                  more charging stations; in fact, it could even be the opposite, that a higher number
                                                  of stations is attracting wealthier people to the area and increasing the median household income."),
                                             p("We can, however, use it to compare the number of stations in counties of differing income 
                                                 levels. For example, using the coefficient from the table, we would expect a U.S. county with
                                                 a median household income of $50,000 to have about __ charging stations, while a county with a median
                                                 household income of $100,000 to have about __ charging stations. Even for counties with a less
                                                 stark difference in median income, such as $30,000 and $40,000, the county with a median income of 
                                                 $40,000 would have approximately __ more charging stations."),
                                             p("This does demonstrate that counties with a higher median income seem to be receiving more investment
                                               in electric vehicle infrastructure. While we cannot say for certain that the increase in EV investment
                                               is caused by the wealth of the county, it is an important consideration for public officials to be
                                               aware of when determining how best to distribute public dollars for EV projects.")
                                             # h4("Scaled Linear Regression"),
                                             #    p("This table shows the same figures, but with both axes scaled, so it demonstrates
                                             #      percent change. Here, the coefficient demonstrates that for each one percent increase
                                             #      in median household income, there are 0.24 percent more charging stations in that
                                             #      county. While this is strong('not statistically significant'**'), it does show a slight
                                             #      positive correlation."),
                                             #            tableOutput("stats_table_scaled"))
                                       )),
                          tabPanel("About",
                                   h2("Background"),
                                   p("As an avid electric vehicle driver, I'm always happy to find a charging station
                                     to make sure I have enough charge to make it to my destination. But looking for a station
                                     tends to conjure up the image of a glitzy mall, not an highly impoverished neighborhood. Yet these
                                     are where the stations need to be. Not only will this improve living conditions, help the 
                                     environment, and drive investment in these areas, the people living there could benefit most
                                     from these stations. They often have long commutes or cannot afford gas."), 
                                   p("As investment in EV infrastructure continues to climb, policymakers need to remain congnizant of
                                     who is at the receiving end of these benefits, some of which come out of all of our taxdollars.
                                     My hope is that my project can shed light onto the current situation."),
                                   br(),
                                   h2("About the Data"),
                                   p("The data on EV charging station locations comes from the National Renewable Energy
                                     Laboratory's Developer Network. Though the database has locations of many different types of
                                     fuel stations, including Biodiesel (B20 and above), Compressed Natural Gas (CNG), Electric, 
                                     Ethanol (E85), Hydrogen, Liquefied Natural Gas (LNG), and Propane (LPG), I focused on electric, 
                                     as that has been seeing significant investment in recent years. The data, which is updated 
                                     regularly, comes from ", 
                                     a(href = 'https://developer.nrel.gov/docs/transportation/alt-fuel-stations-v1/', 'here', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end")),
                                    p("In addition, I drew demographic data from the U.S. Census Bureau's 2018 American Communities
                                      Survey, which is a limited sample version of the Census conducted each year. More information
                                      on the ACS survey can be found ",
                                    a(href = 'https://www.census.gov/programs-surveys/acs', 'here', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end")),
                                   br(),
                                   h2("Contact"),
                                   p("My name is James Bikales, I am a sophomore at Harvard College studying Government 
                                     and East Asian Studies. I'm learning R for its applications in data journalism and 
                                     political science. Contact me with questions or comments at ",
                                     a("jbikales@college.harvard.edu", href = "mailto: jbikales@college.harvard.edu", .noWS = "outside"),".", .noWS = c("after-begin", "before-end")),
                                   p("This project was created for my", a(href = 'https://www.davidkane.info/files/gov_1005_spring_2020.html', 'Gov 1005'),
                                     "final project. The code can be found ",
                                     a(href = 'https://github.com/jbikales/EV-Charging-Stations', 'here', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end"))
                          ))

server <- function(input, output) {
    
    output$state_maps <- renderLeaflet({
        
        # load data in from rds in shiny folder
        
        ev_base <- read_rds("clean-data/ev_data_sf.rds") %>% 
            filter(state.name == as.character(input$state))
        
        incomes_base <- read_rds("clean-data/census_incomes_data.rds") %>% 
            filter(State == as.character(input$state))
        
        rural_base <- read_rds("clean-data/census_rural_data.rds") %>% 
            filter(State == as.character(input$state))
        
        pop_base <- read_rds("clean-data/census_pop_data.rds") %>% 
            filter(State == as.character(input$state))
        
        cars_base <- read_rds("clean-data/census_cars_data.rds") %>% 
            filter(State == as.character(input$state))
        
        education_base <- read_rds("clean-data/census_education_data.rds") %>% 
            filter(State == as.character(input$state))
        
        state_data <- switch(input$attribute,
                        '1' = incomes_base,
                        '2' = rural_base,
                        '3' = cars_base,
                        '4' = pop_base,
                        '5' = education_base)      
        
        census_data <- switch(input$attribute, 
                       '1' = incomes_base$median_household_income,
                       '2' = rural_base$prop_rural,
                       '3' = cars_base$cars_per_capita,
                       '4' = pop_base$total_pop,
                       '5' = education_base$percent_bachelors)
        
        pal <- colorNumeric("YlOrRd", 
                            domain = census_data)
        
        leaflet(options = leafletOptions(dragging = TRUE,
                                                     minZoom = 4,
                                                     maxZoom = 20)) %>%
            addProviderTiles("CartoDB") %>%
            addCircles(data = ev_base,
                      radius = 5,
                      fillColor = "black",
                      stroke = FALSE, 
                      fillOpacity = 1) %>% 
            addPolygons(data = state_data, 
                        weight = 0.5, 
                        color = "#000000",
                        fillColor = ~pal(census_data),
                        fillOpacity = 0.7,
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE),
                        label = state_data$county) %>% 
            addLegend("bottomleft",
                      pal = pal,
                      values = census_data,
                      title = case_when(input$attribute == '1' ~ "Median Household Income",
                                        input$attribute == '2' ~ "Percentage of \n Rural Residents",
                                        input$attribute == '3' ~ "Cars Per Capita",
                                        input$attribute == '4' ~ "Total Population",
                                        input$attribute == '5' ~ "Percentage of Adults \n with Bachelor's Degree \n or Higher"),
                      labFormat = labelFormat(prefix = case_when(input$attribute == '1' ~ "$",
                                                                 input$attribute == '2' ~ "",
                                                                 input$attribute == '3' ~ "",
                                                                 input$attribute == '4' ~ "",
                                                                 input$attribute == '5' ~ ""),
                                              suffix = case_when(input$attribute == '1' ~ "",
                                                                 input$attribute == '2' ~ "%",
                                                                 input$attribute == '3' ~ "",
                                                                 input$attribute == '4' ~ "",
                                                                 input$attribute == '5' ~ "%")))
       
        
    })
    
    output$map_title <- renderText ({
        paste("Map of EV Chargers in", 
              input$state, 
              "With", 
              case_when(input$attribute == '1' ~ "Median Household Income",
                        input$attribute == '2' ~ "Percentage of Rural Residents",
                        input$attribute == '3' ~ "Cars Per Capita",
                        input$attribute == '4' ~ "Total Population",
                        input$attribute == '5' ~ "Percentage of Adults with Bachelor's Degree or Higher"),
              "In Each County")
    })
    
    output$stats <- renderPlotly({
 
        reg_base <- read_rds("clean-data/regression.rds")  

            plot <- ggplot(data = reg_base, 
                           aes(x = median_household_income, y = number_stations,
                               text = paste("County:", county, "<br>",
                                                      "Median Income:", median_household_income, "<br>",
                                                      "Stations:", number_stations))) + 
            geom_point(size = 1) +
            geom_smooth(mapping = aes(x = median_household_income, y = number_stations), method = 'lm', inherit.aes = FALSE) +
            labs(title = "Relationship Between Median Household Income of U.S. Counties \n and Number of EV Charging Stations",
                 subtitle = "Line of Best Fit Shown",
                 caption = "American Communities Survey 2018 and \n National Renewable Energy Laboratory database"
                 ) +
            xlab("Median Household Income") +
            ylab("Number of EV Charging Stations")
            
            ggplotly(plot, tooltip = c("text"))
    })
    
    output$stats_table <- renderTable({
        
        reg_table <- read_rds("clean-data/regression.rds") 
        
        reg_lm <- lm(formula = number_stations ~ median_household_income,
                     data = reg_table)
        
        reg_tidy <- reg_lm %>% 
            tidy(conf.int = TRUE) %>% 
            select(estimate, conf.low, conf.high) %>%
            rename(
                "Coefficient" = estimate,
                "Lower Bound" = conf.low,
                "Upper Bound" = conf.high) %>% 
            tail(1)
        
        gt(reg_tidy)
        
    })
    
    
    # output$stats_table_scaled <- renderTable({
        
        # reg_table_scaled <- read_rds("clean-data/regression.rds") 
        
       #  reg_lm_scaled <- lm(formula = scale(number_stations) ~ scale(median_household_income),
       #              data = reg_table_scaled)
        
       # reg_tidy_scaled <- reg_lm_scaled %>% 
       #     tidy(conf.int = TRUE) %>% 
       #     select(estimate, conf.low, conf.high) %>%
       #     rename(
       #         "Coefficient" = estimate,
       #         "Lower Bound" = conf.low,
       #         "Upper Bound" = conf.high) %>% 
        #    tail(1)
        
       # gt(reg_tidy_scaled)
        
   # })
    
    output$all_stations_graphic <- renderImage({
        list(src = "all_stations.png",
             width="500", 
             height="500"
             )
    }, deleteFile = FALSE)
    
}


shinyApp(ui = ui, server = server)
