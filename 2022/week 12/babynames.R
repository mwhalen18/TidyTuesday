library(tidyverse)
library(tidytuesdayR)

data = tt_load("2022", 12)

babynames = data$babynames

names = babynames %>% 
  slice_sample(prop = 1) %>% 
  pivot_wider(id_cols = c(name, year),
              names_from = 'sex',
              values_from = n,
              names_prefix = "total_") %>% 
  mutate(ratio = total_F/total_M) %>% 
  filter(!is.na(ratio)) %>% 
  slice_sample(prop = .1)
  
ggplot(names, aes(x = year, y = ratio, group = name)) +
  geom_line(size = 0.1, alpha = 0.5, color = "#4b0082") +
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100), labels = c("100 times as \n many males", 
                                                              "10 times as \n many males",
                                                              "Equal males \n and females",
                                                              "10 times as many males",
                                                              "100 times as \n many females")) +
  scale_x_continuous(breaks = c(seq(1880,2020,10)), sec.axis = dup_axis()) +
  labs(x = "Year", title = "Increase in Gender-Neutral Baby Names", 
       subtitle = "Ratio of girls to boys sharing the same name from 1880 to 2018. Each line represents a single name \n \n",
       caption = "Source: {babynames} R package \n Prepared by: Matthew Whalen \n @matthewrwhalen") +
  theme_minimal() +
  theme(text = element_text(family = "NimbusMon"),
        plot.background = element_rect(fill = "#f9f9f9", color = NA),
        panel.grid = element_line(linetype = "dashed"),
        panel.grid.major.x = element_line(linetype = "solid"),
        axis.title.y = element_blank(),
        axis.title.x.top = element_blank(),
        axis.title.x.bottom = element_text(margin = margin(20,0,0,0)),
        plot.margin = margin(20,20,20,20),
        plot.caption = element_text(family = "NimbusMon", face = "italic"))

ggsave(filename = "2022/week 12/babynames.png", height = 8, width = 12, units = "in")
