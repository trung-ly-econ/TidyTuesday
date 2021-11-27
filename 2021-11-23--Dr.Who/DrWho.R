library(tidyverse)
library(viridis)
library(ggridges)
library(patchwork)

# Load data
tuesdata <- tidytuesdayR::tt_load('2021-11-23')
writers <- tuesdata[[1]]
directors <- tuesdata[[2]]
episodes <- tuesdata[[3]]
imdb <- tuesdata[[4]]

# Median rating by season
season_rating <- episodes %>% 
  group_by(season_number) %>% 
  summarize(median_rating = median(rating, na.rm = TRUE),
            average_rating = mean(rating, na.rm = TRUE),
            total_viewers = sum(uk_viewers)
            ) %>% 
  pivot_longer(c(median_rating, average_rating),
               names_to = "rating_type", values_to = "rating") %>% 
  drop_na(season_number)

# Plot
season13_label <- 'Ratings for season 13 are only\n from the first two episodes'

rating1 <- ggplot(data = season_rating, mapping = aes(x = season_number, 
                                           y = rating, 
                                           color = rating_type)) +
  geom_point(size = 3) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE, option = "turbo") +
  annotate(geom = "text", x = 11, y = 77, label = season13_label,
           size = 3.5, hjust = 1) +
  annotate(geom = "segment", x = 11.1, xend = 12.9,
           y = 77, yend = 76.7) + 
  scale_x_continuous(labels = as.character(season_rating$season_number),
                     breaks = season_rating$season_number) +
  theme(legend.position = "right", legend.direction = "vertical") +
  labs(title = "Rating of Dr. Who by season",
       x = "Season", y = "Rating", colour = NULL,
       subtitle = "Average vs Median rating by season") +
  labs(caption = "Note: Specials are not included") 


data_rating2 <- episodes %>% 
  drop_na(c(season_number,rating)) %>% 
  mutate(season_number = factor(season_number))

rating_dist <- ggplot(data = data_rating2, mapping = aes(x = rating,
                                          y = season_number, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(title = "Distribution of episode ratings by season",
    x = "Rating", y = "Season", fill = "Rating",
    caption = "Note: (i) Specials are not included\n (ii) Ratings for season 13 are only\n from the first two episodes\n")
  
(final_plot <- rating1 / rating_dist +
    plot_annotation(
      caption = "Data: Dr. Who data set #TidyTuesday | Visualization: Trung Ly (@trungly_econ)")
  )  

ggsave(filename = "2021-11-23--Dr.Who/DrWho.jpeg", 
       width = 17, height = 10, plot = final_plot)