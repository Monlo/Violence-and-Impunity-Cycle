rm(list = ls())

#------------------------------------------------------------------------------#
# Impunidad Cero
# Project:          Article Mexico's Violence and Impunity Cycle 
# Date:             January 2023
# Authors:          Monserrat López and Helga Jáuregui
# Section:          Intentional Murders Worldwide
# Source:           UNODC: https://dataunodc.un.org/dp-intentional-homicide-victims 
#------------------------------------------------------------------------------#

## Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # locale para Mac
Sys.setlocale("LC_ALL", "Spanish") # locale para Windows
options(scipen = 999)

## Packages ----
pacman::p_load(tidyverse, janitor, readxl, ggrepel,svglite)

## Data ----

### Import:

# Victims of intentional homicide Data 
db_1 <- read_excel("1_Data/data_cts_intentional_homicide.xlsx", 
                   skip = 2) %>% 
  clean_names() 

### |- Evolution of intentional murder rates in Latin America
db <- db_1 %>% 
  filter(indicator == "Victims of intentional homicide" & 
           subregion == "Latin America and the Caribbean" & 
           sex=="Total" &
           dimension=="Total" &
           unit_of_measurement == "Rate per 100,000 population"&
           country %in% c("Colombia", "Honduras", "Mexico", "Brazil", 
                          "Guatemala","El Salvador")) %>% 
  mutate(color = if_else(condition = country == "Mexico", true = "1", false = "0"),
         etiqueta = if_else(condition = year == 2020, 
                            true = fct_recode(.f = as_factor(country), "Mexico" = "Mexico", 
                                              "Brazil" = "Brazil"), 
                            false = as_factor(NA))) 

# 2. Graph ----  
db %>% 
  ggplot(aes(x = year, y = round(value, 1))) +
  geom_point(aes(colour = color), size = 3) + 
  geom_line(aes(color = color, group = country), size = 1) +
  scale_color_manual(values = c("grey70", "#A087C3")) +
  geom_text_repel(aes(label = etiqueta), nudge_x = .7, size = 8) +
  annotate(geom = "text", label = "El Salvador", x = 2019, y = 35, size = 8)+
  scale_y_continuous(limits = c(0,110), 
                     breaks = seq(from = 0, to = 110, by = 10)) +
  scale_x_continuous(limits = c(2008,2022),
                     breaks = 2008:2020) +
  labs(title = "Intentional homicide rates in selected countries in Latin America and the Caribbean",
       subtitle = "in number of homicides per 100,000 inhabitants",
       caption = "Source: UNODC") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title = element_blank(),
        plot.title = element_text(size = 40, face = "bold"),
        plot.subtitle = element_text(size = 30, face = "bold"),
        plot.title.position = "plot",
        axis.text = element_text(size = 25),
        plot.caption = element_text(size = 20, hjust = 0))

ggsave("3_Graphs/01_tasa_hom_latam.png", width = 18, height = 12, dpi = 200) 

