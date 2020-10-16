library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(shinythemes)
library(rgdal)
library(sf)
library(maps)
library(plotly)
library(DT)
library(giphyr)

## SEPT15

kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')
kids <- kids %>% filter(state != "District of Columbia")
kids <- kids %>% mutate(state = tolower(state))
kids <- kids %>% mutate(state = case_when(
  state == "alabama" ~ "AL",
  state == "alaska" ~ "AK",
  state == "arizona" ~ "AZ",
  state == "arkansas" ~ "AR",
  state == "california" ~ "CA",
  state == "colorado" ~ "CO",
  state == "connecticut" ~ "CT",
  state == "delaware" ~ "DE",
  state == "florida"~ "FL",
  state == "georgia" ~ "GA",
  state == "hawaii" ~ "HI",
  state == "idaho" ~ "ID",
  state == "illinois" ~ "IL",
  state == "indiana" ~ "IN", 
  state == "iowa" ~ "IA",
  state == "kansas" ~ "KS",
  state == "kentucky"~ "KY",
  state == "louisiana" ~ "LA",
  state == "maine" ~ "ME",
  state == "maryland" ~ "MD",
  state == "massachusetts" ~ "MA",
  state == "michigan" ~ "MI",
  state == "minnesota" ~ "MN",
  state == "mississippi" ~ "MS",
  state == "missouri" ~ "MO",
  state == "montana" ~ 'MT',
  state == "nebraska"~ "NE",
  state == "nevada"~ "NV",
  state == "new hampshire"~ "NH",
  state == "new jersey"~ "NJ",
  state == "new mexico" ~ "NM",
  state == "new york" ~ "NY",
  state == "north carolina" ~ "NC",
  state == "north dakota" ~ "ND",
  state == "ohio" ~ "OH",
  state == "oklahoma" ~ "OK",
  state == "oregon" ~ "OR",
  state == "pennsylvania" ~ "PA",
  state == "rhode island" ~ "RI",
  state == "south carolina" ~ "SC",
  state == "south dakota"~ "SD",
  state == "tennessee"~ "TN",
  state == "texas"~ "TX",
  state == "utah"~ "UT",
  state == "vermont"~"VT",
  state == "virginia"~"VA",
  state == "washington"~"WA",
  state == "west virginia"~"WV",
  state == "wisconsin" ~ "WI",
  state == "wyoming" ~ "WY",
))
## SEPT 25
members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

# Define UI for application that draws a histogram

ui <- fluidPage(theme = "united",
  
  #Navbar structure for UI
  navbarPage("Rachael's TidyTuesdays", theme = shinytheme("united"),
             tabPanel("",fluid = TRUE, icon = icon("home"),
                      tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
                      h3("Welcome!"),
                      br(),
                      p("Thanks for checking this page out! My TidyTuesday explorations can be navigated to above. 
                        If you are looking for my personal website you can find it at: ", span(tags$a (href = "https://rjaffe123.github.io/", 
                                                                                                      "rjaffe123.github.io."))),
                      br(),
                      p("For all Tidy Tuesday needs, please check out the ", span(tags$a (href = "https://github.com/rfordatascience/tidytuesday", "github page."))),
                      img(src="statsgif.gif", align = "middle")
                      ),
             tabPanel("Sept 15", fluid = TRUE,icon = icon("child"),
                      h3("Kids & Government Spending"),
                      br(),
                      p("This weeks TidyTuesday concerns US public spending on youth related services. The data comes from the Urban Institue. More information about the data can be found", span(tags$a (href = "https://jrosen48.github.io/tidykids/articles/tidykids-codebook.html", 
                                                                                                                                                                                                           "here."))),
                      br(),
                      br(),
                      h4("What does the data look like?"),
                      br(),
                      fluidRow(
                        dataTableOutput("data")
                      ),
                      br(),
                      p("We have data on different sectors of public spending (variable) from 1997 - 2016 by state. A data dictionary can be found on ", span(tags$a (href = "https://jrosen48.github.io/tidykids/articles/tidykids-codebook.html", 
                                                                                                                                                                      "this website."))),
                      br(),
                      h4("Let's explore the data on a map"),
                      fluidRow(
                        box(title = "Choose a public spending variable to show on the map.", width = 6,
                             selectInput('variable', label = "Variables", choices = c(
                               "PreK - 12 grade spending" = "PK12ed",
                               "Higher education spending" = "highered",
                               "Education subsidies (tuition and scholarships)" = "edsubs",
                               "Education special services spending" = "edservs",
                               "Spending on Pell Grants" = "pell",
                               "Head Start award spending" = "HeadStartPriv",
                               "TANF cash assistance payments" = "TANFbasic",
                               "Other cash assistance payments and social services" = "othercashserv",
                               "SNAP benefit payments" = "SNAP",
                               "Social Security payments" ="socsec",
                               "Federal spending on SSI payments" = "fedSSI",
                               "Federal spending on EITC" = "fedEITC",
                               "Child Tax Credit" = "CTC",
                               "Additional Child Tax credit" = "addCC",
                               "Total state spending on EITC" = "stateEITC",
                               "Unemployment Compensation spending" = "unemp",
                               "Workers Compensation spending" = "wcomp",
                               "Medicaid (for children and youth <21)" = "Medicaid_CHIP",
                               "Public health spending" = "pubhealth",
                               "Health vendor payments and public hospitals (excluding Medicaid)" = "other_health",
                               "Housing and community development" = "HCD",
                               "Public spending on Libraries" ="lib",
                               "Parks and recreation spending" = "parkrec"
                             ), selected = 1)),
                         box(title = "Choose a year to view.", width = 6,
                             selectInput("year", label = "Year", choices = unique(kids$year), selected = 1))
                        
                      ),
                      br(),
                      plotlyOutput("maps")
                      ),
             tabPanel("Sept 22", fluid = TRUE,icon = icon("mountain"),
                      h3("Himalayan Climbing Expeditions"),
                      p("This week's data is a compliation of records for all expeditions that have climbed in the Nepal Himalaya. We have data from 1905 to Spring 2019 that covers expeditions from 465 peaks in Nepal")
                      )
                      
)
)



server <- function(input, output) {
  output$data <- renderDataTable(
    kids
  )
  output$maps <- renderPlotly(
      plot_ly(type = "choropleth", locations=kids[kids$year == input$year & kids$variable == input$variable,]$state, locationmode = "USA-states", 
              z=~kids[kids$year == input$year & kids$variable == input$variable,]$inf_adj) 
                                      %>% layout(geo=list(scope='usa'))%>% colorbar(title = "Spending")
  )
}

# text = ~paste("State:", kids$state,
#               "<br> Spending per child (inflation adjusted):", kids$inf_adj_perchild)
# Run the application 
shinyApp(ui = ui, server = server)
