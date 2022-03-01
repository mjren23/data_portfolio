## Data Visualization (GOVT16-QSS17) Spring 2021
## Data Visualization Project 2, Step 2
##
## Name: Megan Ren
## Date: 5/7/21


# Initial settings --------------------------------------------------------
library(tidyverse)
library(ggthemes)

fig_h = 8
fig_w = 11

# Load data ---------------------------------------------------------------

df1 <- readRDS("output/democracy_and_women.RDS")


# Transform ---------------------------------------------------------------

# make bins of values in order to be able to use geom_tile
buckets <- df1 %>% 
  filter(!is.na(avg)) %>% 
  mutate(bucket_women = cut(avg, breaks = c(-Inf, 0.06, 0.12, 0.18, 0.24, 
                                            0.30, 0.36, Inf)),
         bucket_dem = cut(v2x_libdem, breaks = c(-Inf, 0.16, 0.32, 0.48,
                                                 0.64, 0.80, Inf))) %>% 
  count(bucket_women, bucket_dem)
  
buckets


# Plot --------------------------------------------------------------------

title1 <- "Liberal democracy scores are roughly positively correlated with"
title2 <- "percentages of women in parliaments"

subtitle1 <- "Countries with high liberal democracy scores are unlikely to"
subtitle2 <- "have low percentages of women in parliaments"

caption1 <- "Sources: V-Dem Dataset,"
caption2 <- "Women in National Parliaments archives"


ggplot(data = buckets) +
  geom_tile(aes(x = bucket_dem, y = bucket_women, fill = n), color = "white") +
  scale_fill_gradient(low = "white", high = "midnightblue") +
  scale_x_discrete(labels = c("0.08", "0.24", "0.40", "0.56", "0.72", "0.88")) +
  scale_y_discrete(labels = c("3%", "9%", "15%", "21%", "27%", "33%", "39%")) + 
  labs(x = "Liberal Democracy Scale",
       y = "Percentage of Women in Parliament",
       fill = "Number of Countries",
       title = paste(title1, title2),
       subtitle = paste(subtitle1, subtitle2),
       caption = paste(caption1, caption2)) +
  coord_fixed() +
  theme_few() +
  theme(plot.caption = element_text(hjust = 2.5))
  
ggsave("figures/figure.pdf", width = fig_w, height = fig_h)




