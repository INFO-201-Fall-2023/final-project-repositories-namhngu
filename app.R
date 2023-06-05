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
              h2("The Impact of COVID-19 Vaccinations on Local Retail Sales in Washington State"),
              br(),
              p("The severity of COVID-19 continues to be a debated topic. Many right-wing activists and politicians have 
                claimed the gravity of the pandemic is overblown and that mandatory vaccinations, masking, and “stay-at-home” 
                policies have infringed on the freedoms of citizens. In steep contrast, their left-wing counterparts believe 
                that such sacrifices are necessary to slow the infection rate of COVID-19 and preserve human life. The politicization 
                of the disease has resulted in a unique scenario in which areas with differing political perspectives will have vastly
                different COVID-19 policies. Such policies have affected local businesses in unique ways as they dictate the behavior 
                of the possible clients of businesses."),
              p("Despite the growth of e-commerce giants like Amazon and Walmart during the pandemic, many local businesses witnessed 
                a downturn in revenue due to COVID-19. These local businesses did not have a large internet presence and, consequently,
                relied on in-person customers for their revenue. In left-leaning cities like Seattle, government policies and a strong
                media presence decreased the number of people willing to go outside their homes, resulting in fewer clients for local
                businesses. Meanwhile, more right-leaning cities like Miami witnessed huge infection rates which physically stopped 
                clients from appearing at businesses with ailments and decreased employment due to fears of contracting COVID-19.
                In both cases, COVID-19 caused economic repercussions for local businesses, but how has following the separate ideologies,
                avoiding COVID-19 vs. ignoring COVID-19, affected the revenues of local businesses? Does it make a difference in the economic
                recovery of businesses? In our analysis, we hope to answer such questions."),
              p("Our group localized our observations to local retailers in Washington state where we will observe the trend of revenues per
                city. Using data about local retail sales from 2015 to 2022, we can observe how sales were affected by social isolation during
                2020 in comparison to the years prior and how the businesses recovered during 2021 and 2022. Because there isn’t necessarily a
                way to rank cities by their “reigning ideologies about COVID-19”, we have decided to organize cities by their county’s percentage
                of vaccinated citizens as vaccination rates are correlated with a population’s sentiment about the severity of COVID-19. By
                organizing cities by vaccination rate, we can observe how higher vaccination rates in some cities have affected the percentage
                increase/decrease in local retailer revenue over the prior, during, and post-COVID periods. This, consequently, will answer
                questions about how differing ideologies on how to deal with COVID-19 have affected the revenues of local businesses."),
              p("Answering questions about the economic impact of COVID-19 on local business revenue is important as it may provide insight
                into how we should deal with future pandemics/epidemics. As of right now, the U.S. is going through an economic downturn with
                rising inflation rates partially due to the repercussions of COVID-19. During the pandemic, many witnessed panic buying/saving,
                widespread isolation policies, working-from-home policies, etc. for the first time, and in consideration of these events,
                many politicians struggled with balancing the economy with the prevention of COVID-19. Our analysis may provide a good
                starting point for research into how events spawning from a pandemic can affect our economy and, consequently, help lawmakers
                make more educated choices in the policies to deal with disease outbreaks like COVID-19."),
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
