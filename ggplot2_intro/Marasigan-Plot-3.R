library(tidyverse)
library(ggplot2)
trafficstops <- read.csv('MS_stops.csv')
head(trafficstops)

ggplot(data = trafficstops)

ggplot(data = trafficstops, aes(x = female, y = male))

ggplot(trafficstops, aes(violation)) +
  geom_bar()

ggplot(trafficstops, aes(violation)) +
  geom_bar(fill = "green")

ggplot(trafficstops, aes(violation)) +
  geom_bar(aes(fill = driver_gender))

ggplot(trafficstops, aes(violation)) +
  geom_bar(aes(fill = driver_gender), position = "fill")

ggplot(trafficstops, aes(violation)) +
  geom_bar(aes(fill = driver_gender), position = "fill") +
  coord_flip()


trafficstops %>%
  group_by(violation) %>%
  summarize(mean_age = mean(driver_age, na.rm = TRUE)) %>%
  ggplot(aes(x = violation, y = mean_age)) + # reorder
  geom_col() +
  coord_flip() # flip the two axes

trafficstops %>%
  group_by(violation) %>%
  summarize(mean_age = mean(driver_age, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(violation, mean_age), y = mean_age)) + # reorder
  geom_col() +
  coord_flip() # flip the two axes

Yazoo_stops <- trafficstops %>%
  filter(county_name == "Yazoo County", !is.na(driver_age))

ggplot(Yazoo_stops, aes(x = violation, y = driver_age)) +
  geom_boxplot()

ggplot(data = Yazoo_stops, aes(x = violation, y = driver_age)) +
  geom_boxplot() +
  geom_jitter()

ggplot(data = Yazoo_stops, aes(x = violation, y = driver_age)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.5, color = "tomato")

ggplot(data = Yazoo_stops, aes(x = violation, y = driver_age)) +
  geom_jitter(alpha = 0.1, color = "tomato") +
  geom_boxplot()

ggplot(data = Yazoo_stops, aes(x = violation, y = driver_age)) +
  geom_jitter(alpha = 0.1, color = "tomato") +
  geom_boxplot(alpha = 0) 



library(lubridate)
trafficstops <- trafficstops %>%
  mutate(stop_date = ymd(stop_date), wk_day = wday(stop_date, label = TRUE))

trafficstops %>%
  count(wk_day, violation) 

trafficstops %>%
  count(wk_day, violation) %>%
  ggplot(aes(wk_day, n)) +
  geom_line()

trafficstops %>%
  count(wk_day, violation) %>%
  ggplot(aes(wk_day, n, group = violation)) +
  geom_line()

trafficstops %>%
  count(wk_day, violation) %>%
  ggplot(aes(wk_day, n, group = violation, color = violation)) +
  geom_line()

trafficstops %>%
  count(wk_day, violation) %>%
  ggplot(aes(wk_day, n, group = violation)) +
  geom_line() +
  facet_wrap(~ violation)

trafficstops %>%
  count(wk_day, violation, driver_race) %>%
  ggplot(aes(wk_day, n, color = driver_race, group = driver_race)) +
  geom_line() +
  facet_wrap(~ violation)

stops_facet_plot <- trafficstops %>%
  count(wk_day, violation, driver_race) %>%
  ggplot(aes(wk_day, n, color = driver_race, group = driver_race)) +
  geom_line() +
  facet_wrap(~ violation)

stops_facet_plot +
  theme_bw()

my_plot <- stops_facet_plot +
  labs(title = 'Observed violations per day of week',
       x = 'Weekday of observation',
       y = 'Number of violations') +
  theme_bw() +
  theme(axis.text.x = element_text(colour="grey40", size=12, angle=90,
                                   hjust=.5, vjust=.5),
        axis.text.y = element_text(colour="grey40", size=12),
        strip.text = element_text(size=14),
        text = element_text(size=16))
ggsave("MS_weekday_stops_facets.png", my_plot, width=15, height=10)

