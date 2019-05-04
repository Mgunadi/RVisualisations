library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(ggthemes)

########################################## Load data, check dimensions, sanity check
apps <- read.csv("googleplaystore.csv", stringsAsFactors = FALSE)

f <- function(x){
  length(unique(x))
}
sapply(apps, f)
unique(apps$Category)
select_apps <- apps
##### Convert variables into their proper data types.
select_apps$Category <- as.factor(select_apps$Category)
select_apps$Reviews <- as.numeric(select_apps$Reviews)
select_apps$Installs <- factor(select_apps$Installs, levels = c("0+", "1+", "5+", "10+", "50+", "100+", "500+", "1,000+", "5,000+", "10,000+", "50,000+", "100,000+", "500,000+", "1,000,000+", "5,000,000+", "10,000,000+", "50,000,000+", "100,000,000+", "500,000,000+", "1,000,000,000+"), ordered = TRUE)
levels(select_apps$Installs) <- list(`0 - 10^2` = c("0+", "1+", "5+", "10+", "50+"), `10^2 - 10^3` = c("100+", "500+") , `10^3 - 10^4` = c("1,000+", "5,000+"), `10^4 - 10^5`= c("10,000+", "50,000+"), `10^5 - 10^6` = c("100,000+", "500,000+"), `10^6 - 10^7` = c("1,000,000+", "5,000,000+", "10,000,000+"), `> 10^7` = c("50,000,000+", "100,000,000+", "500,000,000+", "1,000,000,000+")) 
select_apps$Price <- gsub('\\$', "", select_apps$Price)
select_apps$Price <- as.numeric(select_apps$Price)

###### Filter apps with certain categories only
#Price: we only want paid apps.
select_clean <- select_apps %>% filter(Price > 0)
as.data.frame(table(select_clean$Category)) %>% arrange(Freq)
select_apps <- apps %>% filter(Category %in% c("FAMILY", "TOOLS", "MEDICAL", "BUSINESS"))
########################################### Explore data: look for missing values and skew of data in histograms
colSums(is.na(select_apps))

library(itertools)
variables <- colnames(select_apps)
unlist(as.list(enumerate(variables)))
screen <- lapply(variables, function(i) ggplot(data = select_apps, aes_string(x=i)) + geom_histogram())

#rating
screen[[3]]  #Negative skewed

#reviews
screen[[4]]  #positively skewed with very long tail - need to cut off

#price
screen[[8]]  #positively skewed with very long tail - need to cut off

#Installs
qplot(x = Installs, data = select_apps, geom = "bar")


######################################### Treat the dataset.

#Price outliers
price <- ggplot(select_clean, aes(x = factor(1), y = Price))
price <- price + geom_boxplot(width = 0.25)
qplot(Price, data = select_clean, geom = "histogram") #Flag price as positively skewed - potential outliers to remove
library(outliers)
z.scores <- select_clean$Price %>% scores(type = "z")
z.scores %>% summary()
#select_clean <- filter(select_apps, Price <22.7)


#Ratings missing values
sum(is.na(select_clean$Rating))
qplot(Rating, data = select_clean, geom = "histogram")
#Negatively skewed - impute missing values with median
select_clean$Rating <- impute(select_clean$Rating, fun = median)


#Installs
qplot(x = Installs, data = select_clean, geom = "bar") #looks normally distributed


######################################### Plots
rate_price <-ggplot(data = select_clean, aes(x = Price, y = Rating))
rate_price + geom_point(position = "jitter")
#Remove phony apps such as "I am RICH" etc.
select_clean <- select_clean %>% filter(Price <100)

#Plot
theme_set(
  theme_light() + 
    theme(legend.position = "right", plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
          x.title = element_text(face = "bold", size = 12), y.title = element_text(face = "bold", size = 12),
         strip.background = element_rect(color= "black", fill="#7DDBFA", size=1, linetype="solid"),
         strip.text = element_text(colour = "black", face = 'bold')))
rate_price <-ggplot(data = select_clean, aes(x = Price, y = Rating, size = Installs))

rate_price <- rate_price + geom_point(position = "jitter", alpha = 0.25, col = "blue") + 
  xlim(0,40) + ylim(1,5) + scale_size_ordinal(range = c(2,8), guide = "legend")
  
  #scale_size(c(1.4, 2,8, 4.2, 5.6, 7, 8.4), guide = "legend")

rate_price <- rate_price + ggtitle("Google playstore App installations - ratings vs price") + 
  labs(title = "Google playstore App installations - ratings vs price", x ="Price ($)", y = "Average Rating", size = "installs") 
    #annotate("text", x = 40, y = 4.3, label = "Norwegian - for Kids and Babies", size = 1.8)
  
rate_price

#Facetting
rate_price + facet_wrap(~ Category)


