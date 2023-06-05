#Noah Curtis
#ncurtis4@uw.edu

library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)


counties_df <- read.csv("unifiedCounties.csv")

#Bar graph calculations, total revenue per fiscal quarter
revenue <- summarise(group_by(counties_df, year_num, quarter_num), Total_Sales = sum(Sales))
revenue$quarter <- factor(paste(revenue$year_num, "Q", revenue$quarter_num, sep = ""))

get_revenue_graph <- function() {
  barplot <- ggplot(data = revenue, aes(x = quarter, y = Total_Sales, tooltip = Total_Sales)) + 
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Fiscal Quarter", y = "Total Revenue", title = "Total Revenue Every Fiscal Quarter")
  plotly <- ggplotly(barplot)
  return(plotly)
}

#print(get_revenue_graph())

#Percent difference against the overall mean
mean_sales_by_county <- summarize(group_by(counties_df, County), mean_sales = mean(Sales))

get_perc_diff <- function(county) {
  row <- subset(mean_sales_by_county, County == county)
  mean_sale <- row$mean_sales
  df <- filter(counties_df, County == county)
  df$perc_diff <- ((df$Sales - mean_sale) / mean_sale) * 100
  df$quarter <- factor(paste(df$year_num, "Q", df$quarter_num, sep = ""))
  return(df)
}

plot_perc_diff <- function(county) {
  row <- subset(mean_sales_by_county, County == county)
  mean_sale <- row$mean_sales
  county_df <- get_perc_diff(county)
  perc_diff <- county_df$perc_diff
  graph_title <- paste("Revenue Percent Difference against the Mean of", county)
  linegraph <- ggplot(data = county_df, aes(x = quarter, y = perc_diff, group = 1)) +
    geom_line(color = "red") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
    labs(x = "Fiscal Quarter", y = "Percent Difference", title = graph_title)
  plotly <- ggplotly(linegraph)
  return(plotly)
}

