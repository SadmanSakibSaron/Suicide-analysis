library(tidyverse)
#install.packages('countrycode')
library(countrycode)
data <- read_csv("master.csv")
theme_set(theme_light()) #cause it'll look pretty 

#global trend
a<-data %>%
  group_by(year)%>%
  mutate(sum_year=sum(`suicides/100k pop`))

ggplot(a,aes(year,sum_year))+geom_line()+geom_point()+
  geom_line(col = "deepskyblue3", size = 1) + 
  geom_point(col = "deepskyblue3", size = 2) + 
  labs(title = "Global Suicides (per 100k)",
       subtitle = "Trend over time, 1985 - 2015.",
       x = "Year", 
       y = "Suicides per 100k") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) + 
  scale_y_continuous(breaks = seq(10, 20))


tidy_data <-  data %>% 
  select(-c(`HDI for year`, `suicides/100k pop`)) %>%
  rename(gdp_for_year = `gdp_for_year ($)`, 
         gdp_per_capita = `gdp_per_capita ($)`, 
         country_year = `country-year`) %>%
  as.data.frame()

#adding continent
tidy_data$continent <- countrycode(sourcevar = tidy_data[, "country"],
                              origin = "country.name",
                              destination = "continent")

tidy_data$age <- factor(tidy_data$age, 
                   ordered = T, 
                   levels = c("5-14",
                              "15-24", 
                              "25-34", 
                              "35-54", 
                              "55-74", 
                              "75+"))
tidy_data$generation <- factor(tidy_data$generation, 
                          ordered = T, 
                          levels = c("G.I. Generation", 
                                     "Silent",
                                     "Boomers", 
                                     "Generation X", 
                                     "Millenials", 
                                     "Generation Z"))

tidy_data <- as_tibble(tidy_data)


b<-tidy_data %>% group_by(continent) %>%
  summarize(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000)

ggplot(b, aes(continent,suicides_per_100k))+
         geom_bar(stat = "identity")


tidy_data %>% group_by(generation) %>%
  summarize(
    population = sum(population), 
    suicides = sum(suicides_no), 
    suicides_per_100k = (suicides / population) * 100000
  ) %>%
  ggplot(aes(generation,suicides_per_100k))+geom_bar(stat = "identity")


tidy_data %>% group_by(sex) %>%
  summarize(
    population = sum(population), 
    suicides = sum(suicides_no), 
    suicides_per_100k = (suicides / population) * 100000
  ) %>%
  ggplot(aes(sex,suicides_per_100k))+geom_bar(stat = "identity")
 




  
  
  
  
  