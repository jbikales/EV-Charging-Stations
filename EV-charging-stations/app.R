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

ui <- navbarPage(theme = shinytheme("yeti"), "EV Charging Stations",
                 tabPanel("Home",
                          h2("Welcome!", align = "center"),
                          p("Electric vehicles are the future — but for whom? This project examines more than 28,000 EV 
                          charging stations in the U.S., comparing their locations to Census demographics, such as median 
                          household income, of the county in which they are located. On this website, I present state-by-state 
                          maps of this relationship, as well as a statistical analysis of the nationwide data.", 
                            align = "center"),
                          p("I found that, in general, counties with a higher median income have a greater number of EV
                            charging stations. This has important policy implications. Investment in EV infrastructure is investment
                            in the future of transportation, and it should go towards reducing inequality, not
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
                                  h6("Sources: U.S. Census 2010, American Communities Survey 2018, and 
                                     National Renewable Energy Laboratory database")
                                  
                              )
                          )),
                          
                          tabPanel("Regression Model",
                                                h2("Graph of Relationship"),
                                             p("I ran a linear regression to explore the relationship
                                                        between the median household income of a county and the number
                                                        of EV stations built there. The resulting best-fit line is
                                                        plotted. Hover over a dot to see which county it represents."),
                                                        plotlyOutput("stats"),
                                   h6("Sources: American Communities Survey 2018 and National Renewable Energy Laboratory database."),
                                              h2("Statistical Relationship"),
                                              h4("Linear Regression and Interpretation"),
                                                        tableOutput("stats_table"),
                                             p("This table shows my average coefficient, along with a 95% confidence interval. Because this was not a randomized
                                                  experiment, we are not able to use this model to determine a causal relationship. There are so many possible confounding
                                                  variables that we cannot say for certain that a higher median household income is causing a county to have
                                                  more charging stations; in fact, it could even be the opposite, that a higher number
                                                  of stations is attracting wealthier people to the area and increasing the median household income."),
                                             p("We can, however, use the model to compare the number of stations in counties of differing income 
                                                 levels. For example, using the coefficient from the table, we would expect a U.S. county with
                                                 a median household income of $50,000 to have about 42 charging stations (with 95% confidence that the true
                                                 number is between 36 and 48), while we would expect a county with a median household income $100,000 to have about
                                                 84 charging stations (with 95% confidence that the true number is between 72 and 96). Even for counties with a less
                                                 stark difference in median income, such as $30,000 and $40,000, the county with a median income of 
                                                 $40,000 would be expected to have approximately nine more charging stations."),
                                             p("This does demonstrate that counties with a higher median income seem to be receiving more investment
                                               in electric vehicle infrastructure. While we cannot say for certain that the increase in EV investment
                                               is caused by the wealth of the county, it is an important consideration for public officials to be
                                               aware of when determining how to distribute public dollars for EV projects.")
                                             # h4("Scaled Linear Regression"),
                                             #    p("This table shows the same figures, but with both axes scaled, so it demonstrates
                                             #      percent change. Here, the coefficient demonstrates that for each one percent increase
                                             #      in median household income, there are 0.24 percent more charging stations in that
                                             #      county. While this is strong('not statistically significant'**'), it does show a slight
                                             #      positive correlation."),
                                             #            tableOutput("stats_table_scaled"))
                                       ),
                          tabPanel("About",
                                   h2("Background"),
                                   p("As an avid electric vehicle driver, I'm always happy to find a charging station
                                     to make sure I have enough charge to make it to my destination. But looking for a station
                                     tends to mean finding the nearest glitzy mall, not a highly impoverished neighborhood. Yet these
                                     are where the stations need to be. This will improve living conditions, help the 
                                     environment, and drive investment in these areas."), 
                                   p("As investment in EV infrastructure continues to climb, policymakers need to remain cognizant of
                                     who is at the receiving end of these benefits, some of which come out of all of our taxdollars.
                                     My hope is that my project can shed light onto the current situation."),
                                   br(),
                                   h2("About the Data"),
                                   p("The data on EV charging station locations comes from the National Renewable Energy
                                     Laboratory's Developer Network. Though the database has locations of many different types of
                                     fuel stations, including Biodiesel (B20 and above), Compressed Natural Gas (CNG), Electric, 
                                     Ethanol (E85), Hydrogen, Liquefied Natural Gas (LNG), and Propane (LPG), I focused on electric, 
                                     as that has seen significant investment in recent years. The data, which is updated 
                                     regularly, comes from ", 
                                     a(href = 'https://developer.nrel.gov/docs/transportation/alt-fuel-stations-v1/', 'here', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end")),
                                    p("I drew demographic data from the U.S. Census Bureau's 2010 Census and 2018 American Communities
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
    
    # rendering the state by state maps with leaflet. Note that some stations show
    # up in the incorrect location due to errors in the NREL dataset. Also, I am not sure
    # why when Alaska is selected, the map automatically zooms to Iceland. 
    
    output$state_maps <- renderLeaflet({
        
        # load data in from each of the rds files in shiny folder. First one is the 
        # ev stations and the rest are census data. It selects only the desired state
        # based on user input.
        
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
        
        # based on user selection of demographic attribute, chooses which dataset to feed to the map
        
        state_data <- switch(input$attribute,
                        '1' = incomes_base,
                        '2' = rural_base,
                        '3' = cars_base,
                        '4' = pop_base,
                        '5' = education_base)  
        
        # based on user selection of demographic attribute, chooses the appropriate variable to display
        
        census_data <- switch(input$attribute, 
                       '1' = incomes_base$median_household_income,
                       '2' = rural_base$prop_rural,
                       '3' = cars_base$cars_per_capita,
                       '4' = pop_base$total_pop,
                       '5' = education_base$percent_bachelors)
        
        # setting color palette for the map polygons
        
        pal <- colorNumeric("YlOrRd", 
                            domain = census_data)
        
        leaflet(options = leafletOptions(dragging = TRUE,
                                                     minZoom = 4,
                                                     maxZoom = 20)) %>%
            addProviderTiles("CartoDB") %>%
            
            # adds circles to denote placement of EV stations
            
            addCircles(data = ev_base,
                      radius = 5,
                      fillColor = "black",
                      stroke = FALSE, 
                      fillOpacity = 1) %>% 
            
            # adds overlayed polygons with the census data based on user selection
            
            addPolygons(data = state_data, 
                        weight = 0.5, 
                        color = "#000000",
                        fillColor = ~pal(census_data),
                        fillOpacity = 0.7,
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE),
                        label = state_data$county) %>% 
            
            # adds the legend with varied text based on user selection
            
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
    
    # this is the title for the state maps. It has to vary based on user selection of the
    # state and the demographic attribute.
    
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
    
    # rendering the regression plotly
    
    output$stats <- renderPlotly({
 
        reg_base <- read_rds("clean-data/regression.rds")  

            plot <- ggplot(data = reg_base, 
                           aes(x = median_household_income, y = number_stations,
                               text = paste("County:", county, "<br>",
                                                      "Median Income:", median_household_income, "<br>",
                                                      "Stations:", number_stations))) + 
            geom_point(size = 1) +
            
            # the best fit line (geom_smooth) was having trouble due to the text added to the aes
            # above to create labels. So I had it override the above data and put the aes in again
            # here without the text.
                
            geom_smooth(mapping = aes(x = median_household_income, y = number_stations), method = 'lm', inherit.aes = FALSE) +
            labs(title = "Relationship Between Median Household Income of U.S. Counties and Number of EV Charging Stations",
                 subtitle = "Line of Best Fit Shown",
                 caption = "Sources: American Communities Survey 2018 and \n National Renewable Energy Laboratory database"
                 ) +
            xlab("Median Household Income") +
            ylab("Number of EV Charging Stations")
            
            ggplotly(plot, tooltip = c("text"))
    })
    
    # rendering the linear regression table
    
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
            
            # because I only wanted to show the coefficient not the intercepts, 
            # I only have it display the bottom line. 
            
            tail(1)
        
        gt(reg_tidy)
        
    })
    
    # this is code to render a scaled regression. Decided not to display this.
    
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
    
    # renders the static graph on the home page. 
    
    output$all_stations_graphic <- renderImage({
        list(src = "all_stations.png",
             width="500", 
             height="500"
             )
    }, deleteFile = FALSE)
    
}


shinyApp(ui = ui, server = server)
