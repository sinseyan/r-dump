library(dplyr)
gapminder <- read.csv("RProjects/dplyr_intro/gapminder-FiveYearData.csv")
dim(gapminder)
str(gapminder)
names(gapminder)
colnames(gapminder)
nlevels(gapminder$country)
class(gapminder$country)
gapminder <- as_tibble(gapminder)
gapminder
australia <- filter(gapminder, country == "Australia")
australia
life81 <- filter(gapminder, lifeExp > 81)
dim(life81)
str(life81)
life81
arrange(life81,  lifeExp)
(gap_small <- select (gapminder, year, country, gdpPercap))
summary(gapminder, maxsum = 10)
gapminder %>% summary(maxsum = 10)
gapminder %>% filter(country != "France")
filter(gapminder, country != "France")
gap_small_97 <- gapminder %>%
  select(year, country, gdpPercap) %>%
  filter(year == 1997)
gap_small_97
eritrea_2002 <- gapminder %>%
  select(year,country,lifeExp) %>% 
  filter(country == "Eritrea", year== 2002)
eritrea_2002
gapminder %>%
  select(gdpPercap, pop)
gap_gdp <- gapminder %>%
  mutate(gdp = gdpPercap * pop)
dim(gap_gdp)
head(gap_gdp)
(gap_gdp <- gapminder %>%
    mutate(gdp = gdpPercap * pop, gdpMil = gdp / 10^6))
gapminder %>%
  summarise(meanLE = mean(lifeExp))
gapminder %>%
  group_by(continent)
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(pop = sum(pop))
gapminder %>%
  group_by(country) %>%
  summarise(maxLE = max(lifeExp))
data(starwars)
starwars
head(starwars)
starwars %>%
  mutate(bmi = mass / ((height / 100) ^ 2)) %>%
  select(name:mass, bmi) # we can select ranges
starwars %>%
  group_by(species) %>%
  summarise(
    n = n(), # this counts the number of rows in each group
    mass = mean(mass, na.rm = TRUE)
  ) %>%
  filter(n > 1)
library(ggplot2)
gapminder %>%
  group_by(continent, year) %>%
  summarise(pop = sum(pop)) %>%
  ggplot(aes(x = year,
             y = pop,
             color = continent)) +
  geom_line()
gapminder %>%
  group_by(country) %>%
  summarise(maxLifeExp = max(lifeExp),
            minLifeExp = min(lifeExp)) %>%
  mutate(dif = maxLifeExp - minLifeExp) %>%
  arrange(desc(dif)) %>%
  slice(1:10, (nrow(.)-10):nrow(.)) %>%
  ggplot(aes(x = reorder(country, dif), y = dif)) +
  geom_col() +
  coord_flip()
