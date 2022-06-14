library(tidyverse)
library(tidytuesdayR)
library(cowplot)

data = tt_load(2022, 14)

news = data$news_orgs

parseColumn = function(.data, .var) {
  .data %>% 
    select(publication_name, {{.var}}) %>% 
    mutate(across({{.var}}, .fns = function(x) str_split(x, pattern = ", "))) %>% 
    unnest({{.var}}) %>% 
    filter(!is.na({{.var}})) %>% 
    group_by({{.var}}) %>% 
    summarize(count = n())
}

ggtheme = theme_minimal() +
  theme(text = element_text(family = "Copperplate", color = "#212227"),
        plot.margin = margin(20,20,20,20),
        plot.background = element_rect(fill = "#ddfff7", color = "#ddfff7"),
        panel.grid = element_line(color = "gray80", size = 0.3),
        panel.grid.minor = element_line(linetype = "dotted", color = "gray50"),
        axis.title.x = element_text(margin = margin(10,0,0,0), size = 12),
        axis.title.y = element_text(margin = margin(0,10,0,0), size = 12),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10))



# Yearly increases --------------------------------------------------------


news_by_year = news %>% 
  filter(year_founded < 2022) %>% 
  group_by(year_founded) %>% 
  summarize(total = n())


p2 = ggplot(news_by_year, aes(x = year_founded, y = total)) +
  geom_bar(stat = 'identity', fill = "#799bc6", width = 0.7) +
  scale_x_continuous(breaks = seq(1970,2020,10), limits = c(1970, 2021)) +
  scale_y_continuous(breaks = seq(0,87,15), limits = c(0,75)) +
  labs(x = "Year Founded", y = "Total Publications") +
  ggtheme +
  theme(panel.grid.minor.x = element_blank(),
        plot.title.position = "plot")

p1 = ggplot(data = NULL) +
  labs(title = "Digital Native News Outlets on the Rise: ",
       subtitle = "Traditional news outlets are struggling to\nmake the transition from print to digital formats. To fill this gap, numerous independent online new outlets have emerged, offering local reporting, especially in remote areas and in underrepresented communities. These organizations have seen explosive growth in the past decade, with an increase of nearly 50% in the past decade.") +
  theme_void() +
  ggtheme +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic", lineheight = 1.5, margin = margin(30,0,0,0)),
        plot.margin = margin(20,0,0,20))

r1 = plot_grid(p1,p2, nrow = 1, rel_widths = c(1/2, 1))
# Funding Types -----------------------------------------------------------

funding_types = news %>% 
  mutate(tax_status_merged = case_when(
    tax_status_current %in% c("For Profit", "LLC") ~ "For Profit",
    tax_status_current %in% c("Nonprofit 501c(3) or Canadian nonprofit",
                              "Not for Profit",
                              "Under umbrella of a 501c(3)") ~ "Not For Profit",
    TRUE ~ "other"
  )) %>% 
  filter(!is.na(revenue_stream_largest),
         tax_status_merged %in% c("For Profit", "Not For Profit")) %>% 
  group_by(tax_status_merged, revenue_stream_largest) %>% 
  summarize(total = n()) %>%
  arrange(-total) %>% 
  slice_head(n = 3) %>% 
  mutate(percentage_of_total_revenue = total/sum(total),
         revenue_stream_largest = str_replace(revenue_stream_largest, "from major", "from\nmajor")) %>% 
  arrange(tax_status_merged, percentage_of_total_revenue) %>% 
  mutate(label_y = cumsum(percentage_of_total_revenue) - 0.5 * percentage_of_total_revenue) 

p3 = ggplot(funding_types, aes(fill = revenue_stream_largest, y = percentage_of_total_revenue, x = tax_status_merged)) +
  geom_bar(stat = 'identity', width = 0.75) +
  scale_fill_manual(values = c("#799bc6", "#e3b505", "#475b5a", "#74a57f", "#b2edc5", "#fe4a49")) +
  geom_text(aes(label = revenue_stream_largest, y = label_y), color = "#212227", fontface = "bold", size = 3) +
  labs(x = "Tax Status", y = "Percentage of Total Revenue") +
  ggtheme +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16))

p4 = ggplot(data = NULL) +
  labs(title = "Funding: ",
       subtitle = "Around 70% of these publications operate as For Profit organizations, while about 20 percent operate as Not For Profit.\nDespite the higher number of For-Profit organizations, the\nNot-For-Profits tend to be larger and receive larger funding\ncontributions.\nThese differences are reflected in the primary revenue streamns for\nthe two types of organizations, with the Not-For-Profits relying on\nphilanthropic gifts and contributions, while the For-Profits have a\nmore standard revenue model.") +
  theme_void() +
  ggtheme +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic", lineheight = 2, margin = margin(25,0,0,0)),
        plot.margin = margin(20,0,0,20))

r2 = plot_grid(p3,p4, rel_widths = c(1,2/3))
plot_grid(r1,r2, nrow = 2)
ggsave(filename = "2022/week 14/test.png", height = 8, width = 12, dpi = 150)

# Products and Style ------------------------------------------------------

# products offered
products = news %>% 
  parseColumn(products) %>% 
  mutate(products = str_replace(products, "reported", "reported\n"),
         products = str_replace(products, "only", "only\n"),
         products = str_replace(products, "marketing", "marketing\n"))

p5 = ggplot(products, aes(x = reorder(products, count), y = count)) +
  geom_bar(stat = 'identity', fill = "#799bc6", color = "#212738", size = 0.2) +
  scale_y_continuous(breaks = seq(0,300,50), limits = c(0,300), expand = expansion(mult = 0)) +
  labs(y = "Number of Publications\nOffering Product", x = NULL) +
  coord_flip() +
  ggtheme 

# Core editorial style

styles = news %>% 
  parseColumn(core_editorial_strategy_characteristics) %>% 
  mutate(core_editorial_strategy_characteristics = str_replace(core_editorial_strategy_characteristics, "community comings", "community\ncomings"),
         core_editorial_strategy_characteristics = str_replace(core_editorial_strategy_characteristics, "and explanatory", "and\nexplanatory"),
         core_editorial_strategy_characteristics = str_replace(core_editorial_strategy_characteristics, "and commentary", "and\ncommentary"))

p7 = ggplot(styles, aes(x = reorder(core_editorial_strategy_characteristics, count), y = count)) +
  geom_bar(stat = 'identity', fill = "#799bc6", color = "#212738", size = 0.2) +
  scale_y_continuous(breaks = seq(0,250,50), limits = c(0,250), expand = expansion(mult = 0)) +
  labs(y = "Number of Publications\nOffering Coverage", x = NULL) +
  coord_flip() +
  ggtheme

p6 = ggplot(data = NULL) +
  labs(subtitle = "Most publications produce\noriginally-reported articles\nand newsletters, while\nadditional forms of media\nincluding video, and social\nmedia are very popular.\n\nThe majority of this content\nis focused around\nhypoerlocal community\nreporting, while they pay\nless attention to special\ntopics, breaking news, or\ninvestigative reporting") +
  theme_void() +
  ggtheme +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic", lineheight = 1, margin = margin(25,0,0,0), size = 10),
        plot.margin = margin(10,10,0,10))


r3 = plot_grid(p5,p6,p7, rel_widths = c(1,1/2,1), nrow = 1)

plot_grid(r1,r2,r3, nrow = 3)
ggsave("2022/week 14/test.png", height = 18, width = 12, dpi = 90)
# Underserved communities

communities = news %>% 
  parseColumn(underrepresented_communities) %>% 
  mutate(underrepresented_communities = str_replace(underrepresented_communities, "English as", "English\nas"))

ggplot(communities, aes(x = reorder(underrepresented_communities, count), y = count)) +
  geom_bar(stat = 'identity', fill = "#799bc6", color = "#212738", size = 0.2) +
  scale_y_continuous(breaks = seq(0,100, 20), limits = c(0,90), expand = expansion(mult = 0)) +
  labs(y = "Number of Publications\nOffering Coverage", x = NULL) +
  coord_flip() +
  ggtheme


# Locations 
locations = news %>% 
  parseColumn(geographic_area)

ggplot(locations, aes(x = reorder(geographic_area, count), y = count)) +
  geom_bar(stat = 'identity', fill = "#799bc6", color = "#212738", size = 0.2) +
  scale_y_continuous(breaks = seq(0,200, 50), limits = c(0,210), expand = expansion(mult = 0)) +
  labs(y = "Number of Publications\nOffering Coverage", x = NULL) +
  coord_flip() +
  ggtheme


