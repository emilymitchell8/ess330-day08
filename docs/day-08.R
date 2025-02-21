# Emily Mitchell
# 2-21-2025
# ESS 330 Daily Assignment 8

library(tidyverse)

covid <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")


state_info <- data.frame(region = state.region,
                         state_name = state.name,
                         state_abbr = state.abb)

covid_data_joined <- covid %>%
  inner_join(state_info, by = c('state' = 'state_name'))

covid_region <- covid_data_joined %>%
  mutate(date = as.Date(date)) %>%
  group_by(region, date) %>%
  summarise(cumulative_cases = sum(cases, na.rm = TRUE), cumulative_deaths = sum(deaths, na.rm = TRUE)) %>%
  arrange(region, date)

covid_long <- covid_region %>%
  pivot_longer(cols = c(cumulative_cases, cumulative_deaths), names_to = 'metric', values_to = 'count')

plot <- ggplot(covid_long, aes(x = date, y = count, color = metric)) +
  geom_line() +
  facet_grid(region ~ metric, scales = 'free_y') +
  labs(title = 'Cumulative COVID-19 Cases and Deaths by USA Region',
       x = 'Date', y = 'Count', color = 'Metric') +
  theme_minimal()


print(plot)


