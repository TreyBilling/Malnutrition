library(tidyverse)
library(sf)
library(gridExtra)
#library(here)
theme_set(theme_bw())



# Data --------------------------------------------------------------------

load(here::here("Data", "dhs_working.Rdata"))

# Remove values that are missing country locations
df_m <- filter(df_m, !is.na(NAME_0))

df_m <- df_m %>% 
  mutate(wealth = case_when(hv270 == "poorest" ~ 1,
                            hv270 == "poorer" ~ 2,
                            hv270 == "middle" ~ 3,
                            hv270 == "richer" ~ 4,
                            hv270 == "richest" ~ 5))



# Descriptive variation ---------------------------------------------------

# Wasting z scores
ggplot(data = df_m) +
  geom_vline(xintercept = -2, color = "darkred", linetype = 3) +
  geom_density(aes(x = hc72, color = hv000), size = 1) +
  facet_wrap(~NAME_0) +
  scale_colour_viridis_d(guide = guide_legend(title = "DHS wave")) +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_x_continuous(breaks = seq(-5,5, 1))


# wasting ~ age
ggplot(data = df_m) +
  geom_point(aes(x = hc1, y = hc72, color = hv000),
             alpha = 0.05, pch = 21, size = 0.75) +
  geom_smooth(aes(x = hc1, y = hc72, color = hv000),
             se = F, method = "lm") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_colour_viridis_d(guide = guide_legend(title = "DHS wave")) +
  facet_wrap(~NAME_0) +
  labs(x = "Child's age (months)",
       y = "Weight-for-Height z-score")

# wealth
### non-linear relationship
ggplot(data = df_m) +
  geom_vline(xintercept = -2, color = "darkred", linetype = 3) +
  geom_density(aes(x = hc72, color = factor(wealth)), size = 1) +
  facet_wrap(~NAME_0) +
  scale_colour_viridis_d(guide = guide_legend(title = "Wealth index")) +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_x_continuous(breaks = seq(-5,5, 1))

gg_w1 <- ggplot(data = df_m) +
  geom_point(aes(x = hv271, y = hc72, color = factor(wealth)),
             alpha = 0.1, pch = 21, size = 0.75) +
  geom_smooth(aes(x = hv271, y = hc72),
              se = F, method = "lm", color = "gray10") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_colour_viridis_d(guide = guide_legend(title = "Wealth quintile")) +
  facet_wrap(~NAME_0) +
  labs(x = "Wealth (factor score)",
       y = "Weight-for-Height z-score",
       title = "Baseline linear")

gg_w2 <- ggplot(data = df_m) +
  geom_point(aes(x = hv271, y = hc72, color = factor(wealth)),
             alpha = 0.1, pch = 21, size = 0.75) +
  geom_smooth(aes(x = hv271, y = hc72, group = factor(wealth)),
              se = F, method = "lm", color = "gray10") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_colour_viridis_d(guide = guide_legend(title = "Wealth quintile")) +
  facet_wrap(~NAME_0) +
  labs(x = "Wealth (factor score)",
       y = "Weight-for-Height z-score",
       title = "Linear, by wealth quintile")

 gg_w3 <- ggplot(data = df_m) +
  geom_point(aes(x = hv271, y = hc72, color = factor(wealth)),
             alpha = 0.1, pch = 21, size = 0.75) +
  geom_smooth(aes(x = hv271, y = hc72),
              se = F, method = "loess", color = "gray30") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_colour_viridis_d(guide = guide_legend(title = "Wealth quintile",override.aes = list(alpha = 0.75, pch = 16)),
                         option = "magma", end = 0.8) +
  facet_wrap(~NAME_0) +
  labs(x = "Wealth (factor score)",
       y = "Weight-for-Height z-score")


#grid.arrange(gg_w1, gg_w2, gg_w3, ncol = 1)

ggsave(here::here("Figures", "wealthgrob.png"),
       dpi = 600, width = 6, height = 5, 
       arrangeGrob(gg_w3, ncol = 1))




# birth interval
### no descriptive relationship, linear or non-linear
df_m <- df_m %>% 
  mutate(birth_interval = ifelse(hc63 == "missing", NA, as.numeric(hc63)))

ggplot(data = df_m) +
  geom_histogram(aes(x = birth_interval, fill = NAME_0),
                 color = "white") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_fill_viridis_d(guide = F) +
  facet_wrap(~NAME_0, scales = "free_y") 

ggplot(data = df_m) +
  geom_point(aes(x = log(birth_interval), y = hc72, color = NAME_0),
             alpha = 0.1, pch = 21, size = 0.75) +
  geom_smooth(aes(x = log(birth_interval), y = hc72),
              se = F, method = "lm", color = "gray10") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_colour_viridis_d(guide = F) +
  facet_wrap(~NAME_0) +
  labs(x = "Birth interval",
       y = "Weight-for-Height z-score")

ggplot(data = df_m) +
  geom_point(aes(x = log(birth_interval), y = hc72, color = NAME_0),
             alpha = 0.1, pch = 21, size = 0.75) +
  geom_smooth(aes(x = log(birth_interval), y = hc72),
              se = F, method = "loess", color = "gray10") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_colour_viridis_d(guide = F) +
  facet_wrap(~NAME_0) +
  labs(x = "Birth interval",
       y = "Weight-for-Height z-score")


# birth order
### no descriptive relationship, linear or non-linear
df_m <- df_m %>% 
  mutate(birth_order = ifelse(hc64 == "missing", NA, as.numeric(hc64)))

ggplot(data = df_m) +
  geom_histogram(aes(x = birth_order, fill = NAME_0),
                 color = "white") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_fill_viridis_d(guide = F) +
  facet_wrap(~NAME_0, scales = "free_y") 

ggplot(data = df_m) +
  geom_point(aes(x = log(birth_order), y = hc72, color = NAME_0),
             alpha = 0.1, pch = 21, size = 0.75) +
  geom_smooth(aes(x = log(birth_order), y = hc72),
              se = F, method = "lm", color = "gray10") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_colour_viridis_d(guide = F) +
  facet_wrap(~NAME_0) +
  labs(x = "Birth order",
       y = "Weight-for-Height z-score")

ggplot(data = df_m) +
  geom_point(aes(x = log(birth_order), y = hc72, color = NAME_0),
             alpha = 0.1, pch = 21, size = 0.75) +
  geom_smooth(aes(x = log(birth_order), y = hc72),
              se = F, method = "loess", color = "gray10") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_colour_viridis_d(guide = F) +
  facet_wrap(~NAME_0) +
  labs(x = "Birth order",
       y = "Weight-for-Height z-score")

# mean by ADM1
df_m %>% group_by(NAME_1, hv000, NAME_0) %>% 
  summarise(m = mean(hc72, na.rm = T)) %>% 
  arrange(-m, NAME_0) %>% 
  ggplot(.) +
  geom_point(aes(x = m, y = forcats::fct_inorder(NAME_1), color = hv000, group = NAME_0)) +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_colour_viridis_d(guide = guide_legend(title = "DHS wave")) +
  facet_wrap(~NAME_0, scales = "free_y")

table(df_m$year, df_m$NAME_0)

# nigeria only
df_nigeria <- filter(df_m, NAME_0 == "Nigeria")

df_nigeria %>% group_by(NAME_1, year) %>% 
  summarise(m_2013 = mean(hc72, na.rm = T)) %>% 
  group_by(NAME_1) %>% 
  arrange(NAME_1, year) %>% 
  mutate(m_2008 = lag(m_2013, 1)) %>% 
  filter(year == 2013) %>% select(-year) %>% 
  ggplot(.) +
  geom_segment(aes(y = m_2008, yend = m_2013, x = "2008", xend = "2013")) +
  geom_text(aes(y = m_2008, x = "2008", label = NAME_1), nudge_x = 0, size = 2, hjust = 1, check_overlap = T) +
  geom_text(aes(y = m_2013, x = "2013", label = NAME_1), nudge_x = 0, size = 2, hjust = -0, check_overlap = T)


ggplot(data = df_nigeria) +
  geom_density(aes(x = hc72, color = factor(year)), size = 1) +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_colour_viridis_d(guide = guide_legend(title = "DHS wave")) 



# The difference by survey wave seems very large
summary(lm(hc72 ~ factor(year), data = df_nigeria))
### the average child's z-score was ~0.39 std deviations lower in 2013 than 2008
summary(lm(hc72 ~ factor(year) + factor(NAME_1), data = df_nigeria))

df_nigeria$year <- as.factor(df_nigeria$year)
df_nigeria$NAME_1 <- as.factor(df_nigeria$NAME_1)
library(lme4)

lmer1 <- lmer(hc72 ~ hc1 + (1 + hc1 | year) + (1 | NAME_1),
              data = df_nigeria)
summary(lmer1)
ranef(lmer1)


ggplot(data = df_nigeria) +
  geom_point(aes(x = hc1, y = hc72, color = year),
             alpha = 0.1, pch = 21, size = 0.75) +
  geom_smooth(aes(x = hc1, y = hc72, color = year),
              se = F, method = "lm") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_colour_viridis_d(guide = guide_legend(title = "DHS wave"),
                         option = "magma", end = 0.5) +
  facet_wrap(~year) +
  labs(x = "Child's age (months)",
       y = "Weight-for-Height z-score")



ggplot(data = df_nigeria) +
  geom_point(aes(x = hc71, y = hc72, color = year),
             alpha = 0.1, pch = 21, size = 0.75) +
  geom_smooth(aes(x = hc71, y = hc72, color = year),
              se = F, method = "lm") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_colour_viridis_d(guide = F,
                         option = "magma", end = 0.5) +
  facet_wrap(~year) +
  labs(x = "Weight-for-Age z-score",
       y = "Weight-for-Height z-score")



ggplot(data = df_nigeria) +
  geom_point(aes(x = hc71, y = hc70, color = year),
             alpha = 0.1, pch = 21, size = 0.75) +
  geom_smooth(aes(x = hc71, y = hc70, color = year),
              se = F, method = "lm") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "gray97")) +
  scale_colour_viridis_d(guide = F,
                         option = "magma", end = 0.5) +
  facet_wrap(~year) +
  labs(x = "Weight-for-Age z-score",
       y = "Height-for-Age z-score")



