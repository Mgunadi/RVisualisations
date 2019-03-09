library(openxlsx)
library(ggplot2)
library(dplyr)
library(reshape2) #reshape2 allows control of variable name and value name as opposed to reshape

data <- read.xlsx("road_fatalities.xlsx")
class(data)

data <- data[-33, -c(13,14)]
data2 <- reshape2::melt(data, id.vars ="Date", variable.name = "Age_gp", value.name = "crashes")
data2$Age_gp <- as.factor(data2$Age_gp)
data2$Date <- as.numeric(data2$Date)


levels(data2$Age_gp)

levels(data2$Age_gp) <- c("0-4", "5-15", "16-17", "18-20", "21-25", "26-29", "30-39", "40-49", "50-59", "60-69", "70+")
data2$Age_gp <- factor(data2$Age_gp, levels= c("0-4", "5-15", "16-17", "18-20", "21-25", "26-29", "30-39", "40-49", "50-59", "60-69", "70+") ,labels =c("<16", "<16", "16-20", "16-20", "21-29", "21-29", "30-39", "40-49", "50-59", "60-69","70+"))

time_gph <- ggplot(data2, aes(x=Date, y =crashes, color=Age_gp)) 
time_gph + geom_line() + 
  scale_x_continuous("Year", breaks=seq(data2$Date[1], data2$Date[length(data2$Date)], 10)) + 
  ggtitle("Victorian Road Crashes across time by age group") +
  scale_fill_brewer()##palette="Spectral")


