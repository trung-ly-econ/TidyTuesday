library(tidyverse)

# read in data
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

# Total number of papers by month
total_by_month <- papers %>% 
  mutate(month = factor(month, labels = c("January", "February", "March", "April", "May", "June", "July",
                                          "August", "September", "October", "November", "December"), ordered = TRUE)) %>% 
  group_by(month) %>% 
  summarize(number_of_papers = n()) %>% 
  mutate(Summer = if_else(month %in% c("May", "June", "July"), "Summer months", "Non-summer months"))

nber <- ggplot(data = total_by_month, mapping = aes(x = month, y = number_of_papers, fill = Summer)) +
  geom_col() + 
  theme(axis.title.y.left = element_text(angle=0, vjust = 0.5)) +
  labs(x = "Month", y = "Papers") + 
  geom_text(mapping = aes(label = number_of_papers), vjust = -.5)+labs(title = "Total number of papers by month",
                                                                       caption = "Data: NBER data set #TidyTuesday | Visualization: Trung Ly (@trungly_econ)")

nber

ggsave(filename = "2021-09-28--NBER/nber.jpeg", 
       width = 13, height = 8, plot = nber)