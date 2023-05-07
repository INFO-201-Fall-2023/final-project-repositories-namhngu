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
    Year <- append(Year, date[1])
    Quarter <- append(Quarter, date[3])
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
    year_vaccines <- append(year_vaccines, date[3])
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

#Removes some columns that aren't used in our analysis
r_df <- select(r_df, year_num, quarter_num, City, County, Total.Taxable, Units)

v_df <- group_by(v_df, Year, Quarter, Recip_County)
v_df <- summarise_all(v_df, max)

unified_df <- merge(x = r_df, y = v_df, by.x = c("year_num", "quarter_num", "County"), by.y = c("Year", "Quarter", "Recip_County"))

unified_df <- arrange(unified_df, City, year_num, quarter_num)
