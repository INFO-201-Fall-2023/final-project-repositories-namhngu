library(dplyr)
library(stringr)
library(ggplot2)

df <- read.csv("unifiedCounties.csv")

p <- ggplot(df)+geom_point(aes(x = quarter_num, y=Sales))+
  facet_grid(.~year_num, scales = "fixed")+labs(x = "Quarter", y = "Income")
  


f <- ggplot(df, aes(color = factor(year_num))) + geom_point(aes(x = Series_Complete_Pop_Pct, y = Sales))+
  facet_grid(.~quarter_num, scales = "fixed")+labs(x = "Complete Vaccination Percentage", y = "Income")+
  scale_color_discrete(name = "Year")

  
print(p)

print(f)