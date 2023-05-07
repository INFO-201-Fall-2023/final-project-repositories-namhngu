library(dplyr)
library(stringr)
df <- read.csv("LocalRevCity.csv")


get_county_code <- function(county, code){
  if (grepl("County", county)){
    
    return(code)
  }
}
county <- df$City[1]
code <- 100
df$county_code[1] <- get_county_code(county,code)

print(length(df$City))
for (x in 2: length(df$City)){
    if (df$City[x] != county & grepl("County", df$City[x])){
      code <- code +100
      county <- df$City[x]
    }
    df$county_code[x] <- get_county_code(county,code)
    
}
