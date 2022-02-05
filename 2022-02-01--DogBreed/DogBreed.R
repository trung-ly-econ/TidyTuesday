if (!require("tidyverse")) install.packages("tidyverse")
if (!require("janitor")) install.packages("janitor")
if (!require("gganimate")) install.packages("gganimate")
if (!require("viridis")) install.packages("viridis")

# Be sure to install the gifski package if you haven't already done so for this code to run
library(tidyverse)
library(janitor)
library(gganimate)
library(viridis)


breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

top_data <- breed_rank_all %>% clean_names() %>% 
  head(10) %>% 
  select(-c(image,links)) %>% 
  mutate(breed = fct_reorder(breed, x2020_rank)) %>% 
  pivot_longer(cols = starts_with("x"),
               names_to = "year",
               values_to = "rank") %>% 
  mutate(year = as.numeric(str_sub(year,2,5))) 

top10_plot <- ggplot(data = top_data, mapping = aes(x = year, y = rank, color = breed,
                                        group = breed)) +
  geom_point(mapping = aes(color = breed, group = breed), size = 5) +
  geom_line(size = 2) +
  scale_y_reverse(breaks = seq(1, max(top_data$rank), 1)) +
  scale_color_viridis(discrete=TRUE) +
  labs(x = "Year", y = "Rank", colour = "Breed") + 
  labs(title = "Historical Ranking of the Top 10 Dog Breeds in 2020",
       subtitle = "Breeds are ordered based on their 2020 rank",
       caption = "Data: Dog breeds data set #TidyTuesday | Visualization: Trung Ly (@trungly_econ)") +
  theme(legend.title = element_text(face = "bold")) +
  transition_reveal(year)
  

final_plot <- animate(top10_plot, renderer = gifski_renderer("top10breeds.gif"), 
                      width = 8, height = 5, units = "in", res = 300, dev= "png", end_pause = 10)
