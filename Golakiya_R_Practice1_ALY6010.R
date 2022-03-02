print("Isha Golakiya")
print("Suicide Analysis")


library(RColorBrewer) 
library(ggridges) 
library(cowplot) 
library(ggplot2) 
library(dplyr)
library(glue)
library(hrbrthemes) 
library(DescTools)
library(ggthemes) 
library(tidyverse)
library(gridExtra)
library(rworldmap)
#Read the csv file
setwd("/Users/HP/Downloads")
suicide <- read.csv("master.csv")
view(suicide)

headtail(suicide)

suicide
class(suicide)

#Understand the Dataset
summary(suicide)
str(suicide)
headtail(suicide,5)
tail(suicide,10)
str(suicide)
cat("No. of missing values:", sum(is.na(suicide)))
vis_miss(suicide)
summary(suicide)

#removing unwanted columns
#country.year= redundant HDI.for.year= unusable: too many missing values
suicide = subset(suicide, select = -c(HDI.for.year,country.year) )
suicide = subset(suicide, select = -c( generation) )
suicide

# convert using lapply function
suicide[c(1,2,3,4)] <- lapply(suicide[c(1,2,3,4)],factor)
suicide$year <- factor(suicide$year,ordered = TRUE)
# check
sapply(suicide, class)


#removed the white space as well as year from age
cide <- suicide %>% 
  mutate(age = str_remove(age,'years'))
cide <- cide %>% 
  mutate(age = str_remove(age," "))
str(cide)
head(cide$age, 5)

# Cross Tabulation
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(cide, row.vars = c("sex"), col.vars = "age", type = "f")
head(cide,3)


#frequency table 
glue("Minimum suicide Number : {min(cide$suicides_no)} | Maximun suicide Number : {max(cide$suicides_no)}")

labels_1 <- c(
  '0     -| 1332',
  '1332  -| 2652',
  '2652  -| 3962',
  '3962  -| 5282',
  '5282  -| 6592',
  '6592  -| 7902',
  '7902  -| 9222',
  '9222  -| 10522',
  '10522 -| 11822',
  '11822 -| 13122',
  '13122 -| 14522',
  '14522 -| 15822',
  '15822 -| 17122',
  '17122 -| 18422',
  '18422 -| 19722',
  '19722 -| 21000',
  '21000 -| 22400'
)

suicide_frq <- cbind(Frequency = table(cut(x = cide$suicides_no, breaks = 17,labels = labels_1, include.lowest = TRUE)),Percent = prop.table(table(cut(x = cide$suicides_no,breaks = 17,labels = labels_1,include.lowest = TRUE)))*100)
library(kableExtra)
kable(suicide_frq) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
cide

# frequency for categorical variable
colnames(cide)[1] <- "country"
glue("No.of Country : {count(unique(cide['country']))}")

country_freq <- data.frame(cbind(Frequency = table(x = cide$country), Percent = prop.table(table(cide$country))*100))
country_freq <- country_freq[order(-country_freq$Frequency),]
kable(country_freq) %>%
  kable_styling(bootstrap_options = "basic", full_width = T)

class(country_freq)

# frequency for year
glue("No. of  unique year : {count(unique(cide['year']))}")
glue("years are from: {min(cide$year)} - {max(cide$year)}")
year_freq <- data.frame(cbind(Frequency = table(x = cide$year), Percent = prop.table(table(cide$year))*100))
year_freq <- year_freq[order(-year_freq$Frequency),]
kable(year_freq) %>%
  kable_styling(bootstrap_options = c("striped","hover"), full_width = F,)

# Frequency for sex
sex_freq <- cbind(Frequency = table(cide$sex), Percent = prop.table(table(cide$sex)) *100) 
kable(sex_freq) %>%
  kable_styling(bootstrap_options = c("basic","hover"), full_width = F)

#Frequency for age
age_freq <- cbind(Frequency = table(cide$age), Percent = prop.table(table(cide$age)) * 100)
kable(age_freq) %>%
  kable_styling(bootstrap_options = c("basic","hover"), full_width = F)

#suicide related to country
avg_per_country <- cide %>% 
  group_by(country) %>% 
  summarise(suicide_avg = round(mean(suicides_no),2))

avg_per_country <- avg_per_country[order(-avg_per_country$suicide_avg),]
avg_per_country_15 <- head(avg_per_country,15)
kable(avg_per_country_15) %>%
  kable_styling(bootstrap_options = c("basic","hover"), full_width = F)

avg_per_country_suicide <- avg_per_country_15 %>% 
  ggplot(aes(x = country , y = suicide_avg)) +
  geom_bar(stat = "identity",aes(fill = country , color = country),size = 1.1,alpha = 0.7) +
  geom_label(aes(label = suicide_avg) , size = 4,fill = "#F5FFFA", fontface = "bold") +
  coord_flip()+
  theme_minimal() +
  theme(plot.background = element_rect( color = "#66CDAA")) + 
  ylab("") +
  xlab("") +
  ggtitle("10 Countries with the Highset Sucicde Rate") 
  

avg_per_country_suicide

#relation betwwen suicide number and age

avg_per_age <- cide %>% 
  group_by(age) %>% 
  summarise(suicide_avg = round(mean(suicides_no),2))

avg_per_age <- avg_per_age[order(-avg_per_age$suicide_avg), ]
avg_per_age_15 <- head(avg_per_age,15)
kable(avg_per_age_15) %>%
  kable_styling(bootstrap_options = c("basic","hover"), full_width = F)

avg_per_age_suicide <- avg_per_age_15 %>% 
  ggplot(aes(x = age , y = suicide_avg)) +
  geom_bar(stat = "identity",aes(fill = age , color = age),size = 1.1,alpha = 0.7) +
  geom_label(aes(label = suicide_avg) , size = 4,fill = "#F5FFFA", fontface = "bold") +
  coord_flip()+
  theme_minimal() +
  theme(plot.background = element_rect( color = "#66CDAA")) + 
  ylab("") +
  xlab("") +
  ggtitle("Age group with Highset Sucicde Rate") 
avg_per_age_suicide

#suicide related to sex
avg_per_sex <- cide %>% 
  group_by(sex) %>% 
  summarise(suicide_avg = round(mean(suicides_no),2))

avg_per_sex <- avg_per_sex[order(-avg_per_sex$suicide_avg),]
avg_per_sex_15 <- head(avg_per_sex,15)
kable(avg_per_sex_15) %>%
  kable_styling(bootstrap_options = c("basic","hover"), full_width = F)

avg_per_sex_suicide <- avg_per_sex_15 %>% 
  ggplot(aes(x = sex , y = suicide_avg)) +
  geom_bar(stat = "identity",aes(fill = sex , color = sex),size = 1.1,alpha = 0.7) +
  geom_label(aes(label = suicide_avg) , size = 4,fill = "#F5FFFA", fontface = "bold") +
  coord_flip()+
  theme_minimal() +
  theme(plot.background = element_rect( color = "#66CDAA")) + 
  ylab("") +
  xlab("") +
  ggtitle("Gender with the Highset Sucicde Rate") 
avg_per_sex_suicide

#suicide related to year
avg_per_year <- cide %>% 
  group_by(year) %>% 
  summarise(suicide_avg = round(mean(suicides_no),2))

avg_per_year <- avg_per_year[order(-avg_per_year$suicide_avg),]
avg_per_year_15 <- head(avg_per_year,15)
kable(avg_per_year_15) %>%
  kable_styling(bootstrap_options = c("basic","hover"), full_width = F)

avg_per_year_suicide <- avg_per_year_15 %>% 
  ggplot(aes(x = year , y = suicide_avg)) +
  geom_bar(stat = "identity",aes(fill = year , color = year),size = 1.1,alpha = 0.7) +
  geom_label(aes(label = suicide_avg) , size = 4,fill = "#F5FFFA", fontface = "bold") +
  coord_flip()+
  theme_minimal() +
  theme(plot.background = element_rect( color = "#66CDAA")) + 
  ylab("") +
  xlab("") +
  ggtitle("Year had Highset Sucicde Rate") 
avg_per_year_suicide



 
