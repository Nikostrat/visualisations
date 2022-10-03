#load libraries
library(mice)
library(dplyr)
library(caret)
#set workind directory
setwd("C:/Users/kosni/Desktop/covid_data")
# Load data
covid <- read.csv("corona_latest.csv", stringsAsFactors = F)
#Checking for missing values 
md.pattern(covid)
#Checking for duplicates
table(duplicated(covid$Country.Other))

###visualisations

#visualizing total cases per country (top 50)
cases_per_country <- covid %>% 
  group_by(Country.Other) %>% 
  summarise(TotalCases=log(TotalCases), n=n()) %>% 
  arrange(desc(n)) %>% head(n=50) %>% 
  arrange(desc(TotalCases))
ggplot(cases_per_country %>%
         group_by(Country.Other),
       aes(x = reorder(Country.Other, + TotalCases), y = TotalCases)) + 
  geom_bar(stat = "identity", fill="grey", width = 0.04)+
  geom_point(size = 10, color="grey") +
  geom_text(aes(label = round(TotalCases,2)), color = "black", size = 3.5) +
  labs(title="Number of cases per country (Top 50)", y="Total cases (log transformed)", x="Countries")+
  scale_y_continuous(expand = c(0,0), limits = c(-10, 100)) + coord_flip() 

#create 'recovery_ratio' variable (per country)
covid$recovery_ratio <- (covid$TotalRecovered/covid$TotalCases)*100
#visualizing recovery rate per country (top 50)
recovered_per_country <- covid %>% 
  group_by(Country.Other) %>% 
  summarise(recovery_ratio=recovery_ratio, n=n()) %>% 
  arrange(desc(n)) %>% head(n=50) %>% 
  arrange(desc(recovery_ratio))
ggplot(recovered_per_country %>%
         group_by(Country.Other),
       aes(x = reorder(Country.Other, + recovery_ratio), y = recovery_ratio)) + 
  geom_bar(stat = "identity", fill="grey", width = 0.04)+
  geom_point(size = 10, color="grey") +
  geom_text(aes(label = round(recovery_ratio,2)), color = "black", size = 3.5) +
  labs(title="Recovery rate per country (Top 50)", y="Recovery rate", x="Countries")+
  scale_y_continuous(expand = c(0,0), limits = c(-10, 100)) + coord_flip() 

#create 'serious_todeath_ratio' variable (calculate the death rate of serious/critical cases per country)
#fix 'Serious.Critical' column, adding 1 in every observation to avoid 'NA' and 'Inf'
covid$Serious.Critical <- covid$Serious.Critical + 1
covid$serious_todeath_ratio <- (covid$TotalDeaths/covid$Serious.Critical)*100
summary(covid$serious_todeath_ratio)
#covid$serious_todeath_ratio[is.na(covid$serious_todeath_ratio)] <- 0
#visualizing death rate of serious cases per country (top 50)
death_serious <- covid %>% 
  group_by(Country.Other) %>% 
  summarise(serious_todeath_ratio=serious_todeath_ratio, n=n()) %>% 
  arrange(desc(n)) %>% head(n=50) %>% 
  arrange(desc(serious_todeath_ratio))
ggplot(death_serious %>%
         group_by(Country.Other),
       aes(x = reorder(Country.Other, + serious_todeath_ratio), y = serious_todeath_ratio)) + 
  geom_bar(stat = "identity", fill="grey", width = 0.04)+
  geom_point(size = 10, color="grey") +
  geom_text(aes(label = round(serious_todeath_ratio,2)), color = "black", size = 3.5) +
  labs(title="Death rate of serious cases per country (Top 50)", y="Death rate", x="Countries")+
  scale_y_continuous(expand = c(0,0), limits = c(-10, 3000)) + coord_flip()

##PCA##
#standardize
covid_st <- covid
covid_st[3:15] <- data.frame(scale(covid_st[3:15]))
summary(covid_st)
#perform a PCA on the variables
ls.pc <- prcomp(covid_st[3:15])
summary(ls.pc)
plot(ls.pc, type="l")
biplot(ls.pc)