library(dplyr)
library(stringr)

r_df <- read.csv("LocalRevCity.csv")
v_df <- read.csv("Washington_County_Vaccinations.csv")

# Functions to set up for merging/cleaning:

# Creates columns for year and quarter for the revenue and vaccines df
createYearQuarterColumnsRev <- function(rev_df) {
  Year <- c()
  Quarter <- c()
  
  for (i in 1:nrow(rev_df)) {
    date <- strsplit(rev_df$Year[i], " +")[[1]]
    Year <- append(Year, as.numeric(date[1]))
    Quarter <- append(Quarter, as.numeric(date[3]))
  }
  
  rev_df$year_num <- Year
  rev_df$quarter_num <- Quarter
  return (rev_df)
}

createYearQuarterColumnsVac <- function(vaccines_df) {
  year_vaccines <- c()
  quarter_vaccines <- c()
  for (i in 1:nrow(vaccines_df)) {
    date <- strsplit(vaccines_df$Date[i], "/")[[1]]
    year_vaccines <- append(year_vaccines, as.numeric(date[3]))
    date[1] <- str_remove(date[1], "^0+") 
    quarter_vaccines <- append(quarter_vaccines,ceiling(strtoi(date[1])/3))
  }
  vaccines_df$Year <- year_vaccines
  vaccines_df$Quarter <- quarter_vaccines
  return (vaccines_df)
}

# Cleans up VaccinesDf by removing Unknown County and filling in 0 for the NA values in the df
cleanVaccinesDf <- function(vaccines_df) {
  vaccines_df <- select(vaccines_df, Year, Quarter, Recip_County, Completeness_pct, Administered_Dose1_Recip, Administered_Dose1_Pop_Pct, Series_Complete_Yes, Series_Complete_Pop_Pct, Booster_Doses, Booster_Doses_Vax_Pct, Series_Complete_Pop_Pct_SVI, Booster_Doses_Vax_Pct_SVI, Census2019)
  vaccines_df <- vaccines_df[vaccines_df$Recip_County != 'Unknown County', ]
  index <- which(is.na(vaccines_df), arr.ind = TRUE)
  naRowIndex <- index[,1]
  naColumnIndex <- index[,2]
  for (i in 1:length(naRowIndex)) {
    vaccines_df[naRowIndex[i], naColumnIndex[i]] <- 0
  }
  na.omit(vaccines_df)
  return (vaccines_df)
}

# Attach Counties to Rev DF
attachCountiesToRev <- function(df) {
  
  # Replaces the county codes with county names
  for (i in 1:length(df$ID)) {
    cc <- as.integer(floor(df$ID[i]/100) * 100)
    df$County[i] <- str_remove(unique(unlist(df[df$ID == cc, 'City'])), "Unincorporated ")
  }
  return (df)
}

r_df <- createYearQuarterColumnsRev(r_df)

v_df <- createYearQuarterColumnsVac(v_df) 

v_df <- cleanVaccinesDf(v_df)

r_df <- attachCountiesToRev(r_df)

#Convert Total.Taxable to numeric instead of string
for (i in 1:nrow(r_df)) {
  r_df$Sales[i] <- as.numeric(str_remove_all(str_remove_all(r_df$Total.Taxable[i], "\\$"), ","))
  r_df$Num_Units[i] <- as.numeric(str_remove_all(r_df$Units[i], ","))
}

#Removes some rows/columns that aren't used in our analysis
r_df <- select(r_df, Year, year_num, quarter_num, City, County, Sales, Num_Units)
r_df <- r_df[!(str_detect(r_df$Year, "Annual")) & r_df$year_num >= 2019,]

v_df <- group_by(v_df, Year, Quarter, Recip_County)
v_df <- summarise_all(v_df, max)

#Joining Data
unified_df <- left_join(r_df, v_df, by=c('year_num'='Year', 'quarter_num'='Quarter', 'County' = 'Recip_County'))

#Final clean
index <- which(is.na(unified_df), arr.ind = TRUE)
naRowIndex <- index[,1]
naColumnIndex <- index[,2]
for (i in 1:length(naRowIndex)) {
  unified_df[naRowIndex[i], naColumnIndex[i]] <- 0
}
for (i in 1:nrow(unified_df)) {
  unified_df$Census2019[i] <- as.numeric(unlist(unique(unified_df[unified_df$County == unified_df$County[i] & unified_df$Census2019 != 0, 'Census2019'])))
}

unified_df <- arrange(unified_df, City, year_num, quarter_num)

#Added new numeric column
for (i in 1:nrow(unified_df)) {
  county = unified_df$County[i]
  year = unified_df$year_num[i]
  quarter = unified_df$quarter_num[i]
  unified_df$county_avg_quarterly_sale_per_capita[i] <-  sum(unified_df[unified_df$County == county & unified_df$year_num == year & unified_df$quarter_num == quarter,'Sales']) / unified_df$Census2019[i]
}

#Added categorial column
for (i in 1:nrow(unified_df)) {
  if (unified_df$Sales[i] > 50000000) {
    unified_df$Is_Commerce_Center[i] <- TRUE
  } else {
    unified_df$Is_Commerce_Center[i] <- FALSE
  }
}


#Summarization table
counties <- group_by(unified_df, County, year_num, quarter_num, Census2019)
counties <- summarize(counties, sum(Sales), sum(Num_Units), mean(Completeness_pct), mean(Administered_Dose1_Pop_Pct), mean(Series_Complete_Pop_Pct), mean(Booster_Doses_Vax_Pct))

write.csv(unified_df, "unified.csv", row.names = FALSE)
