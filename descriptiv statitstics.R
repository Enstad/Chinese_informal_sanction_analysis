library(readxl)
library(rvest)
library(httr)
library(dplyr)
library(tidyverse)
library(zoo)
library(stringr)
library(haven)
library(devtools)
library(lubridate)
library(ggplot2)
library(httr)
library(jsonlite)
library(PxWebApiData)
library(readr)
library(countrycode)
library(imf.data)
library(gravity)
library(plm)
library(penppml)
library(fixest)
library(modelsummary)
library(stargazer)
library(viridis)



#############descriptive statsitics 



descriptive <- chsc_yearly %>%
  select( threatsanct, sender1, state2, imposesanct, imf_code_sender,imf_code_target, informalsanctions, continent_taget, fs, year, MID
         ) 

descriptive <- descriptive %>%
  mutate(state2 = ifelse(state2 == "taiwan", "Taiwan", state2))%>%
  mutate(state2 = ifelse(state2 == "USA","United States of America", state2))

descriptive<- descriptive %>% 
  mutate(informalthreat = case_when(
    informalsanctions == 1 & threatsanct == 1 ~ 1,  
    informalsanctions == 1 & threatsanct == 0 ~ 0,
    TRUE ~ NA     
  ))


descriptive <- descriptive  %>% 
  mutate(formalthreath = case_when(
    fs == 1 & threatsanct == 1 ~ 1,  
    fs == 1 & threatsanct == 1 ~ 0,
    TRUE ~ NA      
  ))


descriptive <- descriptive  %>% 
  mutate(formalnothreath = case_when(
    fs == 1 & threatsanct == 1 ~ 0,  
    fs == 1 & threatsanct == 0 ~ 1,
    TRUE ~ NA      
  ))

descriptive <- descriptive  %>% 
  mutate(infomalnothreath = case_when(
    informalsanctions == 1 & threatsanct == 0 ~ 1,  
    informalsanctions == 1 & threatsanct == 1 ~ 0,
    TRUE ~ NA     
  ))


descriptive <- descriptive  %>% 
  mutate(threathnosanctions = case_when(
    imposesanct == 0 & threatsanct == 1 ~ 1, 
    imposesanct == 1 & threatsanct == 1 ~ 0,
    imposesanct == 0 & threatsanct == 0 ~ 0,
    TRUE ~ NA      
  ))

Descriptivstatstable <- c("fs", "informalsanctions", "informalthreat", 
                          "infomalnothreath", "formalnothreath", "formalthreath","threathnosanctions")


descriptiv <- sapply(Descriptivstatstable, function(var) sum(descriptive[[var]] == 1, na.rm = TRUE))
print(descriptiv)



continent_counts <- descriptive %>%
  group_by(continent_taget,informalsanctions, fs, threatsanct ) %>%
  summarise(count_sanctions = n())

print(continent_counts, n = Inf)


continent_counts_sanctions <- descriptive %>%
  group_by(continent_taget,informalsanctions, fs) %>%
  summarise(count_sanctions = n())

print(continent_counts_sanctions)




##########sanctions types per country 
Count_effected_fs <- descriptive %>%
  filter(fs == 1) %>%
  group_by(state2, fs) %>%
  summarise(count_countries = n(), .groups = 'drop') %>%
  arrange(desc(count_countries))

print(Count_effected_fs)


Count_effected_in <- descriptive %>%
  filter(informalsanctions == 1) %>%
  group_by(state2, informalsanctions) %>%
  summarise(count_countries = n(), .groups = 'drop') %>%
  arrange(desc(count_countries))

print(Count_effected_in)


Count_effected_thre <- descriptive %>%
  filter(threatsanct  == 1) %>%
  group_by(state2,threatsanct ) %>%
  summarise(count_countries = n(), .groups = 'drop') %>%
  arrange(desc(count_countries))

print(Count_effected_thre)



############sanctions per year and country 


year_thre <- descriptive %>%
  filter(threatsanct  == 1) %>%
  group_by(year,threatsanct ) %>%
  summarise(count_countries = n(), .groups = 'drop') %>%
  arrange(desc(count_countries))

print(year_thre)


year_fs <- descriptive %>%
  filter(fs == 1) %>%
  group_by(year,  fs) %>%
  summarise(count_countries = n(), .groups = 'drop') %>%
  arrange(desc(count_countries))

print(year_fs , n = Inf)


year_inf <- descriptive %>%
  filter(informalsanctions == 1) %>%
  group_by(year,  informalsanctions) %>%
  summarise(count_countries = n(), .groups = 'drop') %>%
  arrange(desc(count_countries))

print(year_inf , n = Inf)

#######year and country 



Countries_year_informalsanctions <- descriptive %>%
  filter(informalsanctions == 1) %>%
  group_by(year, state2, informalsanctions) %>%
  summarise(count_countries = n(), .groups = 'drop') %>%
  arrange(desc(count_countries))

print(Countries_year_informalsanctions, n = Inf)

Countries_year_threatsanct <- descriptive %>%
  filter(threatsanct == 1) %>%
  group_by(year, state2, threatsanct) %>%
  summarise(count_countries = n(), .groups = 'drop') %>%
  arrange(desc(count_countries))

print(Countries_year_threatsanct, n = Inf)




Countries_year_sanctions <- descriptive %>%
  group_by(year, state2) %>%
  summarise(count_sanctions = sum(fs, na.rm = TRUE), .groups = 'drop') %>%
  filter(count_sanctions > 0) %>%  
  arrange(desc(count_sanctions))


ggplot(Countries_year_sanctions, aes(x = year, y = state2, fill = count_sanctions)) + 
  geom_tile(colour = "white", linewidth = 0.5, width = .9, height = .9) + 
  theme_minimal() +
  scale_fill_viridis(name = "Number of Sanctions",  direction = 1, trans = "log2", na.value = "grey", limits = c(1, 7)) + 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = seq(1958, 2023, by = 3),
                     limits = c(1958, 2023)) +
  theme(legend.position = "bottom",
        plot.margin = margin(.5, .5, 1.5, .5, "cm"),
        axis.text.y = element_text(size = 10, family = "Helvetica", hjust = 1),
        axis.text.x = element_text(size = 8),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_line(color = "gray80"),  # Add major gridlines
        title = element_text(hjust = -.07, face = "bold", vjust = 1, family = "Helvetica"),
        text = element_text(family = "Helvetica")) +
  ggtitle("Number of Formal Sanctions Against Countries Over the Years", ) +
  xlab("") + ylab("") +
  geom_vline(xintercept = 1976, color = "black", linewidth = 0.5)+
  geom_vline(xintercept = 2000, color = "red", linewidth = 0.5) 



Countries_year_sanctions_inf <- descriptive %>%
  group_by(year, state2) %>%
  summarise(count_sanctions = sum(informalsanctions, na.rm = TRUE), .groups = 'drop') %>%
  filter(count_sanctions > 0) %>%  
  arrange(desc(count_sanctions))


ggplot(Countries_year_sanctions_inf, aes(x = year, y = state2, fill = count_sanctions)) + 
  geom_tile(colour = "white", linewidth = 0.5, width = .9, height = .9) + 
  theme_minimal() +
  scale_fill_viridis(name = "Number of Sanctions",  direction = 1, trans = "log2", na.value = "grey", limits = c(1, 7)) + 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = seq(1958, 2023, by = 3),
                     limits = c(1958, 2023)) +
  theme(legend.position = "bottom",
        plot.margin = margin(.5, .5, 1.5, .5, "cm"),
        axis.text.y = element_text(size = 10, family = "Helvetica", hjust = 1),
        axis.text.x = element_text(size = 8),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_line(color = "gray80"),  # Add major gridlines
        title = element_text(hjust = -.07, face = "bold", vjust = 1, family = "Helvetica"),
        text = element_text(family = "Helvetica")) +
  ggtitle("Number of Informal Sanctions Against Countries Over the Years", ) +
  xlab("") + ylab("") +
  geom_vline(xintercept = 1976, color = "black", linewidth = 0.5)+
  geom_vline(xintercept = 2000, color = "red", linewidth = 0.5)




# Example: Assuming you have two counts for sanctions and informal sanctions
