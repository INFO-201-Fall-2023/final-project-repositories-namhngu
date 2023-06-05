library(ggplot2)
counties_dff <- read.csv("unifiedCounties.csv")
mean_sales_by_county <- summarize(group_by(counties_dff, County), mean_sales = mean(Sales))

percentDiffs <- c()
for (i in 1:nrow(counties_dff)) {
  sale<- counties_dff$Sales[i]
  mean_sale <- mean_sales_by_county[mean_sales_by_county$County == counties_dff$County[i], "mean_sales"]
  percentDiffs <- append(percentDiffs,((sale - mean_sale) / mean_sale) * 100)
}

counties_dff$percentDiff <- percentDiffs
counties_dff$percentDiff <- as.numeric(counties_dff$percentDiff)

get_scatterplot <- function(year, quarter) {
  counties_dff<- counties_dff[counties_dff$year_num == year & counties_dff$quarter_num == quarter,]
  p <- ggplot(data = counties_dff, aes(x = Series_Complete_Pop_Pct, y = percentDiff)) +
    geom_point(aes(col = County, text = County)) +
    labs(x = "% of Population that Completed Primary Series", y = "Percentage Difference from Mean Sales") +
    xlim(0, 100) + ylim(-50, 120)
  p <- ggplotly(p, tooltip = "text")
  return(p)
}

