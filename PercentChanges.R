cities_df <- read.csv("unified.csv")
counties_df <- read.csv("unifiedCounties.csv")

curr_city <- ""
Percent_Diff_Num_Units <- c()
for (i in 1:nrow(cities_df)) {
  if (cities_df$City[i] != curr_city) {
    curr_city <- cities_df$City[i] 
    Percent_Diff_Num_Units <- append(Percent_Diff_Num_Units, 0)
  } else {
    Percent_Diff_Num_Units <- append(Percent_Diff_Num_Units, ((cities_df$Num_Units[i] - cities_df$Num_Units[i-1]) / cities_df$Num_Units[i-1]) * 100)
  }
}
cities_df$Percent_Diff_Num_Units <- Percent_Diff_Num_Units

curr_county <- ""
Percent_Diff_Num_Units <- c()
for (i in 1:nrow(counties_df)) {
  if (counties_df$County[i] != curr_county) {
    curr_county <- counties_df$County[i]
    Percent_Diff_Num_Units <- append(Percent_Diff_Num_Units, 0)
  } else {
    Percent_Diff_Num_Units <- append(Percent_Diff_Num_Units, ((counties_df$Num_Units[i] - counties_df$Num_Units[i-1]) / counties_df$Num_Units[i-1]) * 100)
  }
}
counties_df$Percent_Diff_Num_Units <- Percent_Diff_Num_Units

