library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
library(shinythemes)
library(shinydashboard)

unified_df <- read.csv("unifiedCounties.csv")
source("GetPercDiff.R")

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  themeSelector(),
  tags$style(
    HTML(
      "
      .title-panel {
        background-color: #F8F8F8;
        padding: 20px;
        border-bottom: 1px solid #E5E5E5;
        font-size: 24px;
        font-weight: light;
        color: #333333;
        text-align: left;
      }
      "
    )
  ),
  titlePanel(
    div(class = "title-panel",
        "The Impact of COVID-19 Weariness on Local Retail Sales in Washington State")
  ),
  navbarPage("",
             tabPanel("Introduction",
              
             
             ),
             tabPanel("Revenue Patterns",
                br(),
                sidebarLayout(
                    sidebarPanel(
                      h3("Revenue Inputs"),
                      br(),
                      selectInput(
                        inputId = "rev_input_counties",
                        label = "Select county to display:",
                        choices = unique(unified_df$County),
                        multiple = FALSE
                      ),
                      actionButton(
                        inputId = "rev_bar",
                        label = "Click to see total sales every quarter"
                      ),
                      br(),
                    ),
                    mainPanel(
    
                      h3("Change in Revenue Over Time"),
                      plotlyOutput(outputId = "rev_plot")
                    )
                ),
             ),
             tabPanel("Correlation of Vaccinations and Revenue",
                  br(),
                  sidebarLayout(
                    sidebarPanel(
                      h3("Vaccine and Time Inputs"),
                      br(),
                      numericInput("vax_input_bucket", 
                                   "Select vaccination % bucket size", 
                                   value = 0, 
                                   min = 0, 
                                   max = 100),
                      br(),
                      radioButtons("vax_input_year", 
                                   "Select a year:", 
                                   choices = unique(unified_df$year_num)),
                      br(),
                      radioButtons("vax_input_quarter", 
                                   "Select a quarter:", 
                                   choices = unique(unified_df$quarter_num)),
                    ),
                    mainPanel(
                      h3("Vaccination and Revenue %"),
                      plotlyOutput(outputId = "vax_plot")
                    )
                  ),
                  p("Let's examine the fight songs from 65 different universities across the US 
                  (i.e. all those in the Power Five conferences (the ACC, Big Ten, Big 12, Pac-12 and SEC), plus Notre Dame). 
                  Our analysis looks at how many times certain words appear in the lyrics to see how each song compares. 
                  We also look at song length and speed of each song based on the official versions availible on spotify")
             ),
             tabPanel("Additional Factors",
                br(),
                sidebarLayout(
                  sidebarPanel(
                    h3("Population and Time Inputs"),
                    br(),
                    sliderInput("pop_input_bucket", 
                                 "Select population % bucket size", 
                                 value = 0, 
                                 min = 100000, 
                                 max = max(unified_df$Census2019),
                                 step = 100000),
                    
                    br(),
                    radioButtons("pop_input_year", 
                                 "Select a year:", 
                                 choices = unique(unified_df$year_num)),
                    br(),
                    radioButtons("pop_input_quarter", 
                                 "Select a quarter:", 
                                 choices = unique(unified_df$quarter_num)),
                  ),
                  mainPanel(
                    h3("Vaccination and Revenue"),
                    plotlyOutput(outputId = "pop_plot")
                  )
                ),
                p("Let's examine the fight songs from 65 different universities across the US 
                  (i.e. all those in the Power Five conferences (the ACC, Big Ten, Big 12, Pac-12 and SEC), plus Notre Dame). 
                  Our analysis looks at how many times certain words appear in the lyrics to see how each song compares. 
                  We also look at song length and speed of each song based on the official versions availible on spotify")
             ),
             tabPanel("Conclusion"
             
             ),
            
  )
)

  
server <- function(input, output) {
  observeEvent(input$rev_input_counties, {
    output$rev_plot <- renderPlotly({
      county <- input$rev_input_counties
      plot_perc_diff(county)
    })
  })
  
  observeEvent(input$rev_bar, {
    output$rev_plot <- renderPlotly({
      get_revenue_graph()
    })
  })
}
  
shinyApp(ui = ui, server = server)
