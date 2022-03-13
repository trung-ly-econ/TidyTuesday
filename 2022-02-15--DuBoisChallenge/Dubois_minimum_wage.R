if (!require("tidyverse")) install.packages("tidyverse")
if (!require("here")) install.packages("here")
if (!require("rio")) install.packages("rio")
if (!require("janitor")) install.packages("janitor")
if (!require("showtext")) install.packages("showtext")
if (!require("usmap")) install.packages("usmap")
if (!require("ggrepel")) install.packages("ggrepel")


library(tidyverse)
library(here)
library(rio)
library(janitor)
library(showtext)
library(usmap)
library(ggrepel)

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
  filter(state != "Federal") %>% 
  mutate(state = tolower(state))


font_size <- 40
font_size_legend <- 30

# Get centroids
centroid_labels <- utils::read.csv(system.file("extdata", paste0("us_", "states", "_centroids.csv"), package = "usmapdata"), stringsAsFactors = FALSE)

# Join data to centroids
data_labels <- centroid_labels %>% 
  mutate(full = tolower(full)) %>% 
  left_join(clean_data, by = c("full" = "state")) %>% 
  mutate(label = paste0(abbr," - $",min_wage)) %>% 
  mutate(x_repel = 0, y_repel = 0)


group1 <- c("NJ", "DE", "MD", "WV", "RI", "CT", "MA", "DC", "NH")
group2 <- c("VT")


group1_plot <- data_labels$abbr %in% group1
group2_plot <- data_labels$abbr %in% group2
no_repel <- data_labels$abbr %in% c(group1, group2)

# Plotting
final_plot <- plot_usmap(data = clean_data, values = "high", labels = FALSE) +
  labs(fill = "MINIMUM WAGES (2022)")  + 
  theme(legend.position = "top",
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
        legend.text = element_text(family = "Quantico", size = font_size_legend),
        legend.title = element_text(family = "Quantico", size = font_size_legend),
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
  )  +
  # label for group1 states
  geom_text_repel(data = data_labels[group1_plot,],
                  mapping = aes(x = data_labels$x[group1_plot], 
                                y = data_labels$y[group1_plot],
                                label = data_labels$label[group1_plot],
                                segment.color="#654321"),
                  nudge_x = 111111111111*5.5,
                  nudge_y = -11111,
                  # point.padding = NA,
                  min.segment.length = 0,
                  color = "black", size = 5) +
  # label for group2 states
  geom_text_repel(data = data_labels[group2_plot,],
                  mapping = aes(x = data_labels$x[group2_plot], 
                                y = data_labels$y[group2_plot],
                                label = data_labels$label[group2_plot],
                                segment.color="#654321"),
                  nudge_x = -111111*3.5,
                  nudge_y = 111111,
                  # point.padding = NA,
                  min.segment.length = 0,
                  color = "black", size = 5) +
  # label for all other states
  geom_text(data = data_labels[!no_repel,], 
            mapping = aes(x = data_labels$x[!no_repel], 
                          y = data_labels$y[!no_repel],
                          label = data_labels$label[!no_repel]),
            color = "black", size = 4.5)

final_plot

ggsave(filename = "2022-02-15--DuBoisChallenge/Minimum_wage.jpeg",
       width = 8, height = 5, plot = final_plot)
