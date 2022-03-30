library(tidyverse)
library(tidytuesdayR)
library(gridExtra)

data = tt_load(2022, 13)
sports = data$sports


sports %>% 
  select(year, institution_name, state_cd, classification_code, classification_name, ef_total_count,
        sum_partic_men, sum_partic_women, rev_men, rev_women, exp_men, exp_women) %>% 
  slice_head(n = 500) %>% 
  group_by(year, institution_name) %>% 
  mutate(across(c(sum_partic_women, sum_partic_men, rev_men, rev_women, exp_men, exp_women), sum, na.rm = TRUE)) %>% 
  unique() %>% 
  View()

# Different in expenditure between mens and womens sports
bysport = sports %>% 
  mutate(sports = case_when(
    sports %in% c("Softball", "Baseball") ~ "Softball/Baseball",
    TRUE ~ sports
  )) %>% 
  group_by(sports, year) %>% 
  summarize(across(.cols = c(exp_men, exp_women),sum, na.rm = TRUE)) %>% 
  group_by(sports) %>% 
  summarize(across(.cols = c(exp_men, exp_women),mean, na.rm = TRUE)) %>% 
  mutate(difference = exp_men - exp_women) %>% 
  unique()

g1 = ggplot(subset(bysport, sports != "Football"), aes(x = reorder(sports, difference), y = difference, group = sports)) +
  geom_segment(aes(y = 0, yend = difference, x = reorder(sports, difference), xend  = sports), color = "#190b28") +
  geom_point(color = "#2167ba", size = 3) +
  #ylim(-550000,500000) +
  scale_y_continuous(breaks = c(-5e8, -2.5e8, 0, 2.5e8, 5e8), labels = c("$500 million", "$250 million", "0", "$250 million", "$500 million")) +
  labs(x = "Sport", y = "Total Difference in Average Annual Expenditure (USD)") +
  coord_flip() +
  theme_minimal() +
  theme(plot.margin = margin(20,20,20,20),
        plot.background = element_rect(fill = "#f3f7f0", color = "#f3f7f0"),
        panel.grid = element_line(size = .1, color = "#815e5b"),
        panel.grid.minor = element_line(linetype = "dashed"),
        panel.grid.major.y = element_line(linetype = "dotted"),
        text = element_text(family = "NimbusMon"),
        axis.text.y = element_text(face = "bold", size = 8, angle = 0),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(20,0,0,0)),
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(0,20,0,0)))

# Need to show football

just_foot = bysport %>% 
  group_by(sports) %>% 
  summarize(total_expenditure = sum(exp_men, exp_women)) %>% 
  ungroup() %>% 
  mutate(is_football = sports == "Football") %>% 
  group_by(is_football) %>% 
  summarize(total_expense = sum(total_expenditure))

g2 = ggplot(just_foot) +
  geom_bar(aes(x = reorder(is_football, total_expense), y = total_expense, fill = is_football), stat = "identity", color = "gray20",
           width = 0.75) +
  scale_x_discrete(labels = c("Football", "Non-Football\nSports")) +
  scale_fill_manual(values = c("#190b28", "#2167ba")) +
  scale_y_continuous(breaks = c(0, 3.75e+09, 7.5e+09), labels = c("0", "3.75", "7.5")) +
  labs(x = NULL, y = "Total Annual Spending by Sport\n(In Billions USD)") +
  coord_flip() +
  theme_void() +
  theme(legend.position = "None",
        plot.margin = margin(rep(10,4)),
        plot.background = element_rect(fill = "#f3f7f0", color = "#f3f7f0"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", color = "gray50"),
        text = element_text(family = "NimbusMon"),
        axis.text.y = element_text(face = "bold", size = 8),
        axis.text.x = element_text(size = 6, margin = margin(5,0,0,0)),
        axis.title.x = element_text(face = "bold", size = 8, margin = margin(15,0,0,0))) 

g1 + annotation_custom(ggplotGrob(g2), xmin = 1, xmax = 18, ymin = 2.5e+08, ymax = 7.5e+08) +
  labs(title = "Difference in Spending on Men's vs Women's athletcs",
       subtitle = bquote(atop(~ bold('Main Panel:') ~ "Difference in Spending on Men's vs Women's athletcs", 
                              ~ bold('Inset:') ~ "Football (men's only) accounts for 29% of all spending        " )))
         
         
         
         
         "Difference in Spending on Men's vs Women's athletcs",
       subtitle = bquote(~underline('Main Panel')~ ": Difference in average annual spending on men's vs women's athletics\n"))       #subtitle = bquote(~bold("Main Panel")": Difference in average annual spending on men's vs women's athletics\nInset: Football (men's only) accounts for 29% of all spending")) +
  annotate("text", x = 21, y= -3.75e+08, label = "Higher Women's\nSpending", family = "NimbusMon", size = 3, fontface = "bold") +
  annotate("text", x = 25, y= 3.75e+08, label = "Higher Men's\nSpending", family = "NimbusMon", size = 3, fontface = "bold") +
  theme(plot.title = element_text(margin = margin(0,0,10,0, unit = "pt"), face = "bold"))

        