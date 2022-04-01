library(tidyverse)
library(tidytuesdayR)
data = tt_load(2022, 09)
stations = data$stations %>% 
  mutate(fuel_type = case_when(
    FUEL_TYPE_CODE == "BD"   ~ "Biodiesel",
    FUEL_TYPE_CODE == "CNG"  ~ "Compressed Natural Gas",
    FUEL_TYPE_CODE == "E85"  ~ "Electric",
    FUEL_TYPE_CODE == "ELEC" ~ "Ethanol (E85)",
    FUEL_TYPE_CODE == "HY"   ~ "Hydrogen",
    FUEL_TYPE_CODE == "LNG"  ~ "Liquefied Natural Gas",
    FUEL_TYPE_CODE == "LPG"  ~ "Propane")
  ) %>% 
  filter(!STATE %in% c("AK", "HI"),
         COUNTRY == "US")

stations %>% 
  slice_sample(prop = 1) %>% 
  ggplot(aes(X,Y)) +
  coord_map(projection = "sinusoidal", xlim = c(-130,-66), ylim = c(25, 50)) +
  theme_void() +
  theme(text = element_text(family = "Baskerville", color = "#EAFFFD")) +
  geom_point(size = 0.01, aes(color = fuel_type)) +
  scale_color_manual(values = c("#ED1c24", "#235789", "#FEE440", "#5B8266", "#ffffff", "#109648",
                                "#AE8CA3")) +
  facet_wrap(~fuel_type, ncol = 2) +
  labs(title = "Distribution of Alternative Fuel Stations in the lower U.S") +
  theme(plot.margin = margin(20,20,20,20),
        plot.background = element_rect(fill = "#020100", color = "#020100"),
        legend.position = "None",
        strip.text.x = element_text(size = 10, margin = margin(0,0,5,0)),
        plot.title = element_text(size = 16, face = "bold", margin = margin(0,0,20,0)))

ggsave(p1, filename = "2022/week 09/fuel.png", height = 9, width = 8, dpi = 300, units = "in")
