library(dplyr)
library(stringr)

rev_df <- read.csv("LocalRevCity.csv")
Year <- c()
Quarter <- c()

for (i in 1:nrow(rev_df)) {
  date <- strsplit(rev_df$Year[i], " +")[[1]]
  Year <- append(Year, date[1])
  Quarter <- append(Quarter, date[3])
}

rev_df$year_num <- Year
rev_df$quarter_num <- Quarter

vaccines_df <- read.csv("Washington_County_Vaccinations.csv")

year_vaccines <- c()
quarter_vaccines <- c()
for (i in 1:nrow(vaccines_df)) {
  
}