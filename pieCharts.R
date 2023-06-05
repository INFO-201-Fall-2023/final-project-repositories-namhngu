library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)

unified_df <- read.csv("unifiedCounties.csv")

getVaccinationPieChart <- function(df, bucket, year, quarter) {
  df <- df[df$year_num == year & df$quarter_num == quarter,]
  populationBucket <- c()
  for (i in 1:nrow(df)) {
    pop <- df$Census2019[i]
    lower <- floor(pop/bucket)
    upper <- lower + 1
    populationBucket <- append(populationBucket, paste(as.character(lower*bucket), "=< Pop. <", as.character(upper*bucket)))
  }
  df$populationBucket <- populationBucket
  
  plot <- ggplot(data = df, aes(x="", y=Series_Complete_Yes, fill=populationBucket)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    labs(title = "Vaccination Distribution", x = "", y = "", fill = "Population Buckets")
    
  
  return(plot)
}

getSalesPieChart <- function(df, bucket, year, quarter) {
  df <- df[df$year_num == year & df$quarter_num == quarter,]
  populationBucket <- c()
  for (i in 1:nrow(df)) {
    pop <- df$Census2019[i]
    lower <- floor(pop/bucket)
    upper <- lower + 1
    populationBucket <- append(populationBucket, paste(as.character(lower*bucket), "=< Pop. <", as.character(upper*bucket)))
  }
  df$populationBucket <- populationBucket
  
  plot <- ggplot(data = df, aes(x="", y= Sales, fill=populationBucket)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    labs(title = "Sales Distribution", x = "", y = "", fill = "Population Buckets")
  
  return(plot)
}

getPopPieChart <- function(df, bucket, year, quarter) {
  df <- df[df$year_num == year & df$quarter_num == quarter,]
  populationBucket <- c()
  for (i in 1:nrow(df)) {
    pop <- df$Census2019[i]
    lower <- floor(pop/bucket)
    upper <- lower + 1
    populationBucket <- append(populationBucket, paste(as.character(lower*bucket), "=< Pop. <", as.character(upper*bucket)))
  }
  df$populationBucket <- populationBucket
  
  plot <- ggplot(data = df, aes(x="", y= Census2019, fill=populationBucket)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    labs(title = "Population Distribution", x = "", y = "", fill = "Population Buckets")
  
  return(plot)
}
