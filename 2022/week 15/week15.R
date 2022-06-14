library(tidyverse)
library(tidytuesdayR)
library(cowplot)

dat = tidytuesdayR::tt_load(2022, 15)

#plot gdp on access to clean fuel for cooking sized by population

access = dat$fuel_access %>% 
  #filter(Year == 2016) %>% 
  select(code = Code, year = Year, access = 4) %>% 
  filter(!is.na(code))

gdp = dat$fuel_gdp %>% 
  janitor::clean_names() %>% 
  select(country = entity, code, year, gdp = 5, population = 6, continent = 7)

data = left_join(access, gdp, by = c("code", "year"))

set.seed(1)
p = data %>% 
  filter(#year == 2016,
         !str_detect(country, "World")) %>% 
  ggplot(aes(x = gdp, y = access)) +
  geom_point(aes(size = population), fill = "navyblue", alpha = 0.2, color = "black") +
  scale_x_continuous(trans = "log", labels = scales::label_dollar(), breaks = c(500, 1000, 2000, 5000, 10000, 25000, 50000, 100000),
                     expand = c(0,0.1)) +
  scale_size_continuous(trans = "sqrt", breaks = c(1e5, 1e6, 1e7, 5e7, 1e8, 1e9), 
                        name = "Population", labels = scales::label_number_si()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     expand = c(0,1)) +
  geom_text(data = . %>% 
              filter(country %in% c("United States", "China", "India", "Kenya", "Rwanda", "Somalia", "Afghanistan", "Mexico", "Brazil", "Argentina", "Honduras",
                                    "Marshall Islands", "Peru", "Albania", "Tajikistan", "Fiji")),
            aes(label = country), nudge_x = 0.2, nudge_y = 3, size = 3) +
  labs(x = "GDP (per capita)", y = "Percent of Population with access to\nclean fuels and technologies for cooking") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", color = "navyblue", size = 0.1),
        panel.grid.major.y = element_line(size = 0.1, color = "navyblue"),
        plot.margin = margin(20,20,20,20),
        axis.title.x = element_text(margin = margin(10,0,0,0))) +
  gganimate::transition_states(year, transition_length=3, state_length=1) +
  labs(title = 'Cylinders: {closest_state}')
  # gganimate::transition_time(year) +
  #gganimate::ease_aes('linear')

gganimate::animate(p, duration = 5, fps = 20, width = 600, height = 500, renderer = gganimate::gifski_renderer())
gganimate::anim_save("2022/week 15/output.gif")
