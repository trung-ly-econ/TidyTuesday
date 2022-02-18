if (!require("tidyverse")) install.packages("tidyverse")
if (!require("janitor")) install.packages("janitor")
if (!require("rio")) install.packages("rio")
if (!require("usmap")) install.packages("usmap")
if (!require("showtext")) install.packages("showtext")
if (!require("here")) install.packages("here")


library(tidyverse)
library(here)
library(rio)
library(janitor)
library(usmap)
library(showtext)

# https://www.r-bloggers.com/2019/03/adding-custom-fonts-to-ggplot-in-r/
font_add_google(name = "Quantico") # add font
showtext_auto()

# import data
min_wage2022 <- import(here("2022-02-15--DuBoisChallenge","minimum_wage2022.xlsx"))


clean_data <- min_wage2022 %>% 
  clean_names() %>% 
  select(state_name,minimum_wage_rate) %>% 
  rename(state = state_name,
         min_wage = minimum_wage_rate) %>% 
  mutate(min_wage = as.numeric(substr(min_wage,2,nchar(min_wage)-7))) %>% 
  na.omit() %>% 
  mutate_if(is.character, str_trim) %>% # remove leading white spaces in state names
  mutate(high = if_else(min_wage > .[.$state=="Federal",2], "STATE > FEDERAL", "STATE = FEDERAL")) %>% 
  filter(state != "Federal")


font_size <- 40

final_plot <- plot_usmap(data = clean_data, values = "high", labels = TRUE) +
  labs(fill = "MINIMUM WAGES (2022)")  + 
  theme(legend.position = "right",
        panel.background = element_rect(fill = "#d2b48c",
                                        colour = "#d2b48c"),
        plot.background = element_rect(fill = "#d2b48c"),
        legend.key = element_rect(fill = "#d2b48c"),
        legend.background = element_rect(fill = "#d2b48c"),
        plot.title = element_text(hjust = .5,
                                  size = font_size,
                                  family = "Quantico", face = "bold"),
        plot.subtitle = element_text(hjust = .5,
                                  size = font_size,
                                  family = "Quantico", face = "bold"),
        legend.text = element_text(family = "Quantico", size = font_size),
        legend.title = element_text(family = "Quantico", size = font_size),
        plot.caption = element_text(family = "Quantico",
                                    size = 20)
        ) +
  labs(title = "MINIMUM WAGES IN THE UNITED STATES IN 2022",
       caption = "DATA: HTTPS://WWW.MINIMUM-WAGE.ORG/ | VISUALIZATION: TRUNG LY (@TRUNGLY_ECON)",
       subtitle = "THE FEDERAL MINIMUM WAGE IS $7.25 / HOUR"
       ) +
  scale_colour_manual(values = c("STATE > FEDERAL" = "#ffd700",
                                 "STATE = FEDERAL" = "#4682b4"),
                      aesthetics = c("fill")
                      )  


ggsave(filename = "2022-02-15--DuBoisChallenge/Minimum_wage.jpeg", 
       width = 8, height = 5, plot = final_plot)
