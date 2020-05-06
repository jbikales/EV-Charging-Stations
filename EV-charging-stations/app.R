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
                                  h4("About"),
                                  p("decription"),
                                  p("more description"),
                                  selectInput(inputId = "state",
                                              label = "State",
                                              choices = state.name, selected = "Massachusetts"),
                                  
                                  radioButtons(inputId = "attribute", 
                                               label = "Demographic Attribute",
                                               choices = list("Median Income" = 1, "Percent Rural - TBD" = 2, "Vehicles Per Capita - TBD" = 3, "Total Population - TBD" = 4, "Educational Attainment - TBD" = 5), selected = 1)
                              ),
                              
                              mainPanel(
                                  plotOutput("graph1"),
                                  p("Source: American Communities Survey 2018 and National Renewable Energy Laboratory database")
                                  
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
                                              h4("Standard Linear Regression"),
                                                p("This table shows my average coefficient, or the slope of the regression
                                                  line, along with a 95% confidence interval. The estimate value indicates
                                                  that there is a slight positive correlation between median household income
                                                  of a county and the number of charging stations there. For every $1 increase 
                                                  in median income, there are 0.00083 more stations in that county. While that
                                                  might seem small, if you hypothetically consider a $10,000 increase in median 
                                                  household income, that means the county gains around 8 new charging stations."),
                                                        tableOutput("stats_table"),
                                             h4("Scaled Linear Regression"),
                                                p("This table shows the same figures, but with both axes scaled, so it demonstrates
                                                  percent change. Here, the coefficient demonstrates that for each one percent increase
                                                  in median household income, there are 0.24 percent more charging stations in that
                                                  county. While this is strong('not statistically significant'**'), it does show a slight
                                                  positive correlation."),
                                                        tableOutput("stats_table_scaled"))
                                       )
                                   ,
                          tabPanel("About",
                                   h2("Background"),
                                   p("As an avid electric vehicle driver, I'm always happy to find a charging station
                                     to make sure I have enough charge to make it to my destination. But looking for a station
                                     tends to conjure up the image of a glitzy mall, not an area with high poverty. Yet these
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
    
    output$graph1 <- renderPlot({
        
        # load data in from rds in shiny folder
        
        combined_data <- read_rds("clean-data/combined.rds")
        
        combined_data %>% 
            
            filter(State == input$state) %>% 
            
            ggplot(aes(fill = median_household_income)) +
            geom_sf() +
            geom_point(aes(Longitude, Latitude), size = 1, color = "green") +
            labs(title = paste("EV charging stations in", input$state, sep = " "), 
                 subtitle = "Counties by Median Household Income", 
                 fill = "Median Household Income" 
            ) +
            theme_void()
        
    })
    
    output$stats <- renderPlotly({
 
        reg_data <- read_rds("clean-data/regression.rds") 
            
            plot <- ggplot(data = reg_data, mapping = aes(median_household_income, number_stations)) + 
            geom_point() +
            geom_smooth(method='lm') +
            labs(title = "Relationship Between Median Household Income of U.S. Counties \n and Number of EV Charging Stations",
                 subtitle = "Line of Best Fit Shown",
                 caption = "American Communities Survey 2018 and \n National Renewable Energy Laboratory database"
                 ) +
            xlab("Median Household Income") +
            ylab("Number of EV Charging Stations")
            
            ggplotly(plot)
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
    
    output$stats_table_scaled <- renderTable({
        
        reg_table_scaled <- read_rds("clean-data/regression.rds") 
        
        reg_lm_scaled <- lm(formula = scale(number_stations) ~ scale(median_household_income),
                     data = reg_table_scaled)
        
        reg_tidy_scaled <- reg_lm_scaled %>% 
            tidy(conf.int = TRUE) %>% 
            select(estimate, conf.low, conf.high) %>%
            rename(
                "Coefficient" = estimate,
                "Lower Bound" = conf.low,
                "Upper Bound" = conf.high) %>% 
            tail(1)
        
        gt(reg_tidy_scaled)
        
    })
    
    output$all_stations_graphic <- renderImage({
        list(src = "all_stations.png",
             width="500", 
             height="500"
             )
    }, deleteFile = FALSE)
    
}


shinyApp(ui = ui, server = server)
