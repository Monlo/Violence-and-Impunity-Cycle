rm(list = ls())

#------------------------------------------------------------------------------#
# Impunidad Cero
# Project:          Article Mexico's Violence and Impunity Cycle 
# Date:             January 2023
# Authors:          Monserrat López and Helga Jáuregui
# Section:          Intentional Murders Committed with Firearms in Mexico
# Source:           SESNSP: https://www.gob.mx/sesnsp/acciones-y-programas/victimas-nueva-metodologia?state=published 
#------------------------------------------------------------------------------#

# 0. Setup  ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Para Mac
Sys.setlocale("LC_ALL", "Spanish") # Para Windows
options(scipen = 999)

# Packages:
pacman::p_load(readxl, tidyverse, janitor, treemapify,stringr)


# 1. Load data  ----

# Load victims database
victims <- read_excel("1_Data/Estatal-V¡ctimas-2015-2022_dic2022.xlsx")%>% 
  clean_names()

victims %>% glimpse()

# 2. Cleaning -----------------------------------------------------

# Sum by month to calculate the year total 
victims$total <- rowSums(victims[10:21])

# Filter by type of crime (Intentional murders and Feminicides)
db <- victims %>% 
  filter(subtipo_de_delito == "Homicidio doloso" | subtipo_de_delito== "Feminicidio") 

# Group by  por modality, state, sex and year

db <- db %>% 
  filter(ano != 2022 & sexo != "No identificado") %>% 
  group_by(ano, subtipo_de_delito, clave_ent, entidad, sexo, modalidad) %>% 
  summarise(total = sum(total, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(clave_ent, ano)


# Intentional murders in  2015 adn 2021 by sex and modality

db_1 <- db %>% 
  filter(subtipo_de_delito == "Homicidio doloso" & ano %in% c(2015, 2021)) %>% 
  group_by(ano,sexo, modalidad) %>% 
  summarise(tot_nacional = sum(total)) %>% 
  ungroup() %>% 
  group_by(ano,sexo) %>% 
  mutate(percentage = round(tot_nacional/sum(tot_nacional)*100, 0),
         modalidad = factor(x = modalidad, levels = c("No especificado", "Con arma blanca", "Con otro elemento","Con arma de fuego")))  %>%
  mutate(modalidad = str_replace(modalidad, "No especificado", "Not specified")) %>%
  mutate(modalidad = str_replace(modalidad, "Con arma blanca", "Handguns")) %>%  
  mutate(modalidad = str_replace(modalidad, "Con arma de fuego", "Firearms")) %>%
  mutate(modalidad = str_replace(modalidad, "Con otro elemento", "Other weapon")) %>%
  mutate(sexo = str_replace(sexo, "Hombre", "Men")) %>%
  mutate(sexo = str_replace(sexo, "Mujer", "Women")) %>%
  ungroup() 
  
  # 2. Graph ----  

db_1 %>%  
  ggplot(aes(y = percentage, x = sexo)) +
  geom_bar(aes(fill = modalidad),stat = "identity") +
  scale_fill_manual(values = c("#9F86C2", "#CA98C7", "#FAB893", "#F9CB76")) +
  facet_wrap(~ ano) +
  labs(title = str_wrap(string = "Percentage of Victims of Intentional Homicide by Type of Weapon and Sex", width = 50),
       fill = "Type of weapon",
       caption = "Source: SESNSP") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.title = element_text(size =20),
        axis.title = element_blank(),
        axis.text = element_text(size = 25),
        plot.title = element_text(face = "bold", size = 40),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 35),
        plot.caption = element_text(size = 20, hjust = 0),
        strip.text = element_text(size = 28, face = "bold"))

ggsave("3_Graphs/02_weapon.png", width = 16, height = 9, dpi = 200)
