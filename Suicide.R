library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

suicide <- read_csv("suicide_rates.csv")
suicide2 <- suicide %>%  select(sex, age, suicides_no, gdp_per_capita, country)
qplot(data = suicide2, x= sex, y = suicides_no,  geom = "bar")


##suicide2 %>% select(suicides_no) %>% filter(country == max(suicide2$country))


suicide2[which(suicide2$country),3]
