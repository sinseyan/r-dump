library(dplyr)
library(ggplot2)

df <- read.csv("crime.csv")
glimpse(df)

summary(df)

cleaned_data <- df %>%
  rename(overall_cr = X1) %>% 
  rename(violent_cr = X2) %>% 
  rename(police_fund = X3) %>% 
  rename(perc25_4HS = X4) %>%
  rename(perc1619_NHS = X5) %>% 
  rename(perc1824_OC = X6) %>% 
  rename(perc25_4C = X7)

glimpse(cleaned_data)

model <- lm(overall_cr ~ police_fund + perc25_4HS + perc1619_NHS + perc1824_OC + perc25_4C, data = cleaned_data)
summary(model)


summary(model)$coefficient

ggplot(cleaned_data, aes(x = police_fund + perc25_4HS + perc1619_NHS + perc1824_OC + perc25_4C, y = overall_cr)) +
  geom_point()+
  stat_smooth(method = lm)

sigma(model)/mean(cleaned_data$overall_cr)

ggplot(df, aes(x = X3 + X4 + X5 + X6 + X7, y = X2)) +
  geom_point()+
  stat_smooth(method = lm)
