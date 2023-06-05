library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
library(shinythemes)
library(shinydashboard)

unified_df <- read.csv("unifiedCounties.csv")
source("GetPercDiff.R")
source("pieCharts.R")
source("Vaccination vs Sales Info 201.R")
options(scipen = 999)
source("scatter.R")

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
              h2("The Economic/Social Impact of COVID-19"),
              br(),
              p("The severity of COVID-19 continues to be a debated topic. Many right-wing activists and politicians have 
                claimed the gravity of the pandemic is overblown and that mandatory vaccinations, masking, and “stay-at-home” 
                policies have infringed on the freedoms of citizens. In steep contrast, their left-wing counterparts believe 
                that such sacrifices are necessary to slow the infection rate of COVID-19 and preserve human life. The politicization 
                of the disease has resulted in a unique scenario in which areas with differing political perspectives will have vastly
                different COVID-19 policies. Such policies have affected local businesses in unique ways as they dictate the behavior 
                of the possible clients of businesses."),
              p("Despite the growth of e-commerce giants like Amazon and Walmart during the pandemic, many local businesses witnessed 
                a downturn in sales due to COVID-19. These local businesses did not have a large internet presence and, consequently,
                relied on in-person customers for their sales. In left-leaning cities like Seattle, government policies and a strong
                media presence decreased the number of people willing to go outside their homes, resulting in fewer clients for local
                businesses. Meanwhile, more right-leaning cities like Miami witnessed huge infection rates which physically stopped 
                clients from appearing at businesses with ailments and decreased employment due to fears of contracting COVID-19.
                In both cases, COVID-19 caused economic repercussions for local businesses, but how has following the separate ideologies,
                avoiding COVID-19 vs. ignoring COVID-19, affected the saless of local businesses? Does it make a difference in the economic
                recovery of businesses? In our analysis, we hope to answer such questions."),
              p("Our group localized our observations to local retailers in Washington state where we will observe the trend of saless per
                city. Using data about local retail sales from 2017 to 2022, we can observe how sales were affected by social isolation during
                2020 in comparison to the years prior and how the businesses recovered during 2021 and 2022. Because there isn’t necessarily a
                way to rank cities by their “reigning ideologies about COVID-19”, we have decided to organize cities by their county’s percentage
                of vaccinated citizens as vaccination rates are correlated with a population’s sentiment about the severity of COVID-19. By
                organizing cities by vaccination rate, we can observe how higher vaccination rates in some cities have affected the percentage
                increase/decrease in local retailer sales over the prior, during, and post-COVID periods. This, consequently, will answer
                questions about how differing ideologies on how to deal with COVID-19 have affected the sales of local businesses."),
              p("We specifically looked at vaccination data from the CDC: \"COVID-19 Vaccinations in the United States,County\"
                .The Centers for Disease Control and Prevention collected this 
                data through the use of vaccination providers that partnered with the CDC: jurisdictional partner clinics, retail pharmacies, 
                long-term care facilities, dialysis centers, Federal Emergency Management Agency and Health Resources and Services Administration 
                partner sites, and federal entity facilities. Submissions made by these partners contain data about the dose number, manufacturer, 
                date administered and recipient ID of the people receiving vaccines which were then organized by county. We took special notice of the series completed
                columns which indicated percentage/number of population who have completed the primary series of vaccinations for COVID-19. We also
                took census population information from this to get an idea of the population of each county. In addition to the data from the CDC, 
                we looked at revenue data from Washington State Department of Revenue: \"Taxable Retail Sales\". The Department of Revenue of Washington State collects 
                data through tax returns where businesses are supposed to list their sales and type of store. After grouping counties together in this dataset, we
                joined the two datasets together on yearly quarters and counties to find and observe relationships between trend of sales and how vaccinations/COVID-19 weariness could play into that.")
             ),
             tabPanel("Sales Patterns",
                br(),
                sidebarLayout(
                    sidebarPanel(
                      h3("County Inputs"),
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
                      br(),
                      p("COVID 19 has had an undeniable influence on state and regional economies, this story will look at the big picture and see how business performance has changed 
                      since pre pandemic to the tail end of the pandemic. Overall the pandemic has had a negative impact on the economies in Washington 
                      state but by how much? We hope to investigate how different areas of Washington have been impacted in relation to the other areas. On this page, you will
                      be able to see how different Washington State counties performed before, during, and after the pandemic in relation to their mean economic performance. In addition, for a holistic
                      view, you are able to see the sum of all county revenues throughout the quarters. From this view, we can see most counties did not make as much sales as the usually do during 2020Q1 and
                      2020Q2, indicating a noticeable economic impact from COVID-19. Looking at total revenue chart of counties like King County, we can see that the change in total revenue was was a couple million dollars worth.")
                    ),
                    mainPanel(
                      h3("Change in Sales Over Time"),
                      plotlyOutput(outputId = "rev_plot")
                    )
                )
             ),
             tabPanel("Correlation of Vaccinations and Sales",
                  br(),
                  sidebarLayout(
                    sidebarPanel(
                      h3("Time Inputs"),
                      br(),
                      radioButtons("vax_input_year", 
                                   "Select a year:", 
                                   choices = c(2021, 2022)),
                      br(),
                      radioButtons("vax_input_quarter", 
                                   "Select a quarter:", 
                                   choices = unique(unified_df$quarter_num)),
                      br(),
                      p("In this graph we attempt to observe a correlation between the vaccination. We are attempting to observe if the increase in COVID-19 weariness 
                        across counties affected the income increase/decrease caused by COVID-19 during that year. If we see the points increase as we move 
                        to the right of the graph, we could say there is a possible positive correlation between COVID-19 weariness and income. 
                        If we see the points decrease as we move to the right of the graph, we could say there is a possible negative correlation 
                        between COVID-19 weariness and income. If the points are somewhat even, there might not be a correlation at all. This will lead 
                        to our conclusions about the dataset and how different levels COVID-19 weariness could’ve translated into differing income 
                        of retailers during the pandemic. "),
                      p("In this case, we can see a slight positive correlation in some of the quarters but mostly no correlation. HOWEVER, do notice that as we
                        move further in time the points do increase in variation, distancing themselves from eachother. This could possible mean a deviation from 
                        highly COVID-19 weary counties and not COVID-19 weary counties. It is possible that the effects of a county's decisions during COVID-19
                        has not immediately caused a difference in the economics of the county. Consequently, it is possible that an analysis later in time could prove more fruitful")
                    ),
                    mainPanel(
                      h3("Vaccination and Sales %"),
                      plotlyOutput(outputId = "vax_plot"),
                      br()
                    )
                  )
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
                                 "Select a year*:", 
                                 choices = unique(unified_df$year_num)),
                    br(),
                    radioButtons("pop_input_quarter", 
                                 "Select a quarter:", 
                                 choices = unique(unified_df$quarter_num)),
                    br(),
                    p("*Notice that vaccines weren't developed until 2021 and our data only reaches 2022Q3."),
                    br(),
                    br(),
                    p("These charts look at population of counties in relation to vaccination numbers and revenue. 
                      The counties are split up into buckets of population to see how population is possiblly correlated with 
                      factors of revenue and vaccination numbers. From this we can see how other factors such as population
                      may factor into the cause of certain economic patterns. For example, we observed that highly populated counties: \"2,200,000 <= Pop. < 2,300,000\"
                      were often correlated with a larger proportion of vaccinated peoples and larger proportion of revenue in comparison to all other counties. Areas with larger
                      populations usually have more tourists as more people means more attractions and shops. This means during the isolation-period these areas would be more heavily 
                      hit economically. This shows how COVID-19 weariness isn't neccessarily the only thing driving the economic downturns in 2020Q1 and 2020Q2 as tourism/population
                      can have an effect too. From this analysis, we realized that the COVID-19 economic trends is much more nuanced.")
                    
                  ),
                  mainPanel(
                    h3("Population, Vaccination, Sales"),
                    plotOutput(outputId = "pop_pop_plot"),
                    fluidRow(
                      column(width = 6, plotOutput(outputId = "pop_vac_plot")),
                      column(width = 6, plotOutput(outputId = "pop_sales_plot"))
                    )
                  )
                ),
             ),
             tabPanel("Conclusion",
                  mainPanel(
                      h2("The Economic/Social Impact of COVID-19"),
                      br(),
                      p(),
                      p("Answering questions about the economic impact of COVID-19 on local business sales is important as it may provide insight
                        into how we should deal with future pandemics/epidemics. As of right now, the U.S. is going through an economic downturn with
                        rising inflation rates partially due to the repercussions of COVID-19. During the pandemic, many witnessed panic buying/saving,
                        widespread isolation policies, working-from-home policies, etc. for the first time, and in consideration of these events,
                        many politicians struggled with balancing the economy with the prevention of COVID-19."),
                      p("We observed that the pandemic had a negative affect on the economy of Washington State but what was interesting was that
                        COVID vaccination rates had little impact on recovering areas, counties that had a lower vaccination percentage experienced
                        levels of economic recovery similar to that of a county with a higher vaccination rate would have."),
                      h4("In conclusion, the pandemic has had a negative impact on economies in Washington state but the severity of the effects has not differed between counties due to vaccination rates"),
                  ),
                  sidebarPanel(
                    h2("Data Sources"),
                    h3("Vaccination Rates by County in the U.S.:"),
                    p("https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh"),
                    h3("Local Retail Revenues by City (Washington):"),
                    p("https://apps.dor.wa.gov/ResearchStats/Content/\nTaxableRetailSalesLocal/Report.aspx")
                  ),
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
      get_sales_graph()
    })
  })
  
  output$pop_pop_plot <- renderPlot(getPopPieChart(unified_df, input$pop_input_bucket, input$pop_input_year, input$pop_input_quarter));
  output$pop_vac_plot <- renderPlot(getVaccinationPieChart(unified_df, input$pop_input_bucket, input$pop_input_year, input$pop_input_quarter));
  output$pop_sales_plot <- renderPlot(getSalesPieChart(unified_df, input$pop_input_bucket, input$pop_input_year, input$pop_input_quarter));
  
  output$vax_plot <- renderPlotly({
    get_scatterplot(input$vax_input_year, input$vax_input_quarter)
  })
}
  
shinyApp(ui = ui, server = server)
