# ## Data Visualization (GOVT16-QSS17) Spring 2021
# ## Workshop: Good and Bad Data Visualization
# ##
# ## Name: Megan Ren
# ## Date: 4/23/21
# 
# 
# # Initial settings --------------------------------------------------------
# 
# library(tidyverse)
# library(ggthemes)
# 
# 
# fig_w = 10
# fig_h = 7.5
# 
# # Load data ---------------------------------------------------------------
# 
# df <- read_csv("workshop/data/ranking.csv") 
# 
# 
# # Wrangle ---------------------------------------------------------------
# 
# df2 <- df %>% 
#   pivot_longer(c("Princeton", "Harvard", "Yale", "U Penn", "Columbia", "Dartmouth", "Brown", "Cornell"), 
#                names_to = "school",
#                values_to = "ranking") %>% 
#   mutate(is_dartmouth = ifelse(school == "Dartmouth", "green", "lightgray"))
# 
#   
# # Figure 1 ----------------------------------------------------------------
# 
# ggplot(data = df2, aes(year, ranking, group = school)) +
#   geom_line(aes(color = school))
# 
# ggsave("workshop/figures/figure1.pdf", width = fig_w, height = fig_h)
# 
# 
# 
# # Figure 2 ----------------------------------------------------------------
# 
# ggplot(data = df2, aes(year, ranking, color = is_dartmouth)) +
#   scale_color_manual(values = c("lightgray", "darkgreen")) +
#   geom_line() +
#   geom_point() +
#   scale_y_reverse() +
#   geom_hline(yintercept = 10, lty = 2) +
#   scale_x_continuous(breaks = c(2006, 2021)) +
#   coord_cartesian(xlim = c(2005, 2022)) +
#   facet_wrap(~school, nrow = 2) +
#   labs(x = "Year",
#        y = "U.S. News and World Report Rankings") +
#   theme_few()
# 
# ggsave("workshop/figures/figure2.pdf", width = fig_w, height = fig_h)
# 
# 
# 
# # Figure 3 ----------------------------------------------------------------
# 
# df3 <- df2 %>% 
#   mutate(id = school)
# 
# ggplot(data = df3, aes(year, ranking)) +
#   geom_line(data = df2, aes(group = school), color = "lightgray", alpha = 0.8, na.rm = TRUE) +
#   scale_y_reverse(breaks = c(15, 10, 5, 1)) +
#   geom_line(na.rm = TRUE) +
#   geom_point(na.rm = TRUE) +
#   geom_hline(yintercept = 10, lty = 2) +
#   scale_x_continuous(breaks = c(2006, 2019)) +
#   coord_cartesian(xlim = c(2005, 2020), ylim = c(17, 0)) +
#   facet_wrap(~id, nrow = 2) +
#   labs(x = "Year",
#        y = "U.S. News and World Report Rankings") +
#   theme_few()
# 
# ggsave("workshop/figures/figure3.pdf", width = fig_w, height = fig_h)

library(tidyverse)
library(ggthemes)

df <- read_csv("workshop/data/ranking.csv")

df1 <- df %>% 
  pivot_longer(2:ncol(df), names_to = "school",
               values_to = "rank")

ggplot(data = df1) +
  geom_line(aes(year, rank, color = school)) 

df2 <- df1 %>% 
  mutate(dartmouth = ifelse(school == "Dartmouth",
                            "green",
                            "other"))

ggplot(data = df2, aes(year, rank, color = dartmouth)) +
  geom_line() +
  geom_point(size = 2) + 
  scale_y_reverse(breaks = c(15, 10, 5, 1)) +
  scale_color_manual(values = c("darkgreen", "lightgray")) +
  geom_hline(yintercept = 10, lty = 2) +
  facet_wrap(~ school, nrow = 2) +
  theme_few() +
  theme(legend.position = "none")


df3 <- df1 %>%
  mutate(id = school)

ggplot(data = df3) +
  geom_line(data = df2, aes(year, rank, group = school), color = "lightgray") +
  scale_y_reverse(breaks = c(15, 10, 5, 1),
                  limits = c(17, 0)) +
  geom_line(aes(year, rank), color = "black") +
  geom_point(aes(year, rank), color = "black") +
  facet_wrap(~id, nrow = 2) +
  theme_few()

