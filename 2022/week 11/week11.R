library(tidyverse)
library(lubridate)
library(tidytuesdayR)
library(gridExtra)

data = tt_load("2022", 11)
cran = data$cran


package_versions = cran %>% 
  group_by(package) %>% 
  mutate(versions = n_distinct(version)) %>% 
  arrange(-versions) %>% 
  select(package, versions) %>% 
  distinct() %>% 
  arrange(-versions)

cran_totals = cran %>% 
  mutate(long_day = strptime(date, format = "%a %b %d %H:%M:%S %Y"),
         dday = as_date(date),
         date = coalesce(long_day, dday),
         year = year(date)) %>% 
  group_by(year) %>% 
  summarize(total_packages = n_distinct(package))

package_versions %>% 
  head(20) %>% 
  ggplot(aes(x = reorder(package, versions), y = versions)) +
  geom_bar(stat = "identity", fill = "#2167ba", color = "#190b28") +
  coord_flip() +
  labs(x = "Package Name", y = "Total Versions pushed to CRAN",
       title = "Top 20 Packages with Most Version Updates",
       subtitle = "17,000 total packages since 2001",
       caption = "Source: {babynames} R package \n Prepared by: Matthew Whalen \n @matthewrwhalen") +
  theme_minimal() +
  theme(text = element_text(family = "NimbusMon"),
        panel.grid.major.x = element_line(color = "#815e5b", size = .1),
        panel.grid.minor.x = element_line(linetype = "dashed", color = "#815e5b", size = .1),
        axis.text.y = element_text(face = "bold", size = 11),
        plot.background = element_rect(fill = "#f3f7f0", , color = "#f3f7f0"),
        axis.title = element_text(face = 'bold', size = 12),
        plot.margin = margin(20,20,20,20),
        axis.title.x = element_text(margin = margin(20,0,0,0)),
        axis.title.y = element_text(margin = margin(0,20,0,0)),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(margin = margin(0,0,20,0)),
        plot.caption = element_text(face = "italic", size = 7))
ggsave(filename = "2022/week 11/package-versions.png", height = 8, width = 12, units = "in")

cran_totals %>% 
  filter(year %in% 2001:2022) %>% 
  ggplot(aes(x = year, y = total_packages)) +
  geom_point(color = "#2167ba", size = 3) +
  geom_line(color = "#190b28", size = 1) +
  scale_x_continuous(breaks = seq(2001,2022,2)) +
  labs(x = "Year", y = "Numer of New Packages Added",
       title = "Number of New Packages Pushed to Cran by Year",
       caption = "Source: {babynames} R package \n Prepared by: Matthew Whalen \n @matthewrwhalen") +
  theme_minimal() +
  theme(text = element_text(family = "NimbusMon"),
        plot.background = element_rect(fill = "#f3f7f0", color = "#f3f7f0"),
        panel.grid = element_line(size = .1, color = "#815e5b"),
        panel.grid.minor = element_line(linetype = "dashed"),
        plot.margin = margin(20,20,20,20),
        axis.title.x = element_text(face = "bold", size = 12, margin = margin(20,0,0,0)),
        axis.title.y = element_text(face = "bold", size = 12, margin = margin(0,20,0,0)),
        plot.title = element_text(face = "bold", margin = margin(0,0,20,0)),
        plot.caption = element_text(face = "italic", size = 7))
ggsave(filename = "2022/week 11/totals.png", height = 8, width = 12, units = "in")


        