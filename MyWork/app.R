## Global
library(tidyverse)
library(shiny)
library(shinythemes)
library(png)
library(janitor)
library(here)
library(stringr)
library(googleCharts)

data <- read_csv(here("modified-data", "continent_data.csv")) %>% 
  clean_names() %>% 
  filter(!str_detect(year, "2050")) %>% 
  filter(year %in% c(1990:2014))
data$continent <- as.factor(data$continent)

ui <- navbarPage("Selection:",
                 theme = shinytheme("yeti"),
                 tabPanel("Introduction",
                          h1("INTRODUCTION TITLE"),
                          p("Introduction paragraphs"),
                          img(src="bren_logo2.png",height = 100, width = 300, align="right")),
                 tabPanel("World Map",
                          h1("MAP TITLE")),
                 tabPanel("Predictive Model",
                          googleChartsInit(),
                          tags$link(
                            href=paste0("http://fonts.googleapis.com/css?",
                                        "family=Source+Sans+Pro:300,600,300italic"),
                            rel="stylesheet", type="text/css"),
                          tags$style(type="text/css",
                                     "body {font-family: 'Source Sans Pro'}"
                          ),
                          h2("Bubble Chart of CO2 Emissions Per Capita and GDP Per Capita"),
                          googleBubbleChart("chart",
                                            width="100%", height = "675px",
                                            options = list(
                                              fontName = "Source Sans Pro",
                                              fontSize = 13,
                                              hAxis = list(
                                                title = "CO2 Tons Per Capita",
                                                viewWindow = xlim),
                                              vAxis = list(
                                                title = "GDP Per Capita (USD)",
                                                viewWindow = ylim),
                                              chartArea = list(
                                                top = 50, left = 75,
                                                height = "75%", width = "75%"
                                              ),
                                              explorer = list(),
                                              bubble = list(
                                                opacity = 0.5, stroke = "none",
                                                textStyle = list(
                                                  color = "none"
                                                )
                                              ),
                                              titleTextStyle = list(
                                                fontSize = 12
                                              ),
                                              tooltip = list(
                                                textStyle = list(
                                                  fontSize = 14
                                                )
                                              )
                                            )
                          ),
                          fluidRow(
                            shiny::column(4, offset = 4,
                                          sliderInput("year", "Year",
                                                      min = min(data$year), max = max(data$year),
                                                      value = min(data$year), animate = TRUE)
                            )
                          )
                          ),
                 tabPanel("Comparison Table",
                          h1("COMPARISON TABLE TITLE"))
)

server <- shinyServer(function(input, output, session) {
  defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099")
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(data$continent)
  )
  
  yearData <- reactive({
    df <- data %>% 
      filter(year == input$year) %>% 
      select(country, co2_tons_pc, gdp_per_capita_current_us,
             continent, renewable_pct) %>% 
      arrange(continent)
  })
  
  output$chart <- reactive({
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = sprintf(
          "CO2 vs GDP as a function of renewable energy",
          input$year),
        series = series
      )
    )
  })
})

  
# Put them together!

shinyApp(ui = ui, server = server)
