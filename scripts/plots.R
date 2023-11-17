library(tidyverse)
library(ICplots)

theme_set(theme_ic())

data_file <- "data/Brazil with Bahia supplemental data_2023-11-13.csv"
bahia <- read_csv(data_file) %>%
  filter(admin1 == "Bahia")

ggplot(subset(bahia, year == 2023)) +
  geom_line(aes(x = event_date, y = fatalities), group = 1) +
  hline

killed_by_actor <- bahia %>%
  filter(year == 2023 & fatalities > 0) %>%
  group_by(actor1) %>%
  summarise(fatalities = sum(fatalities)) %>%
  mutate(actor1 = ifelse(grepl("Military Police", killed_by_actor$actor1) == TRUE,
                         "Military Police",
                         ifelse(grepl("GAECO", killed_by_actor$actor1) == TRUE,
                                "GAECO", killed_by_actor$actor1)))

ggplot(killed_by_actor) +
  geom_col(aes(x = reorder(actor1, fatalities), y = fatalities)) +
  xlab("") +
  ylab("Fatalities") +
  labs(title = "People Killed per Group, 2023") +
  hline +
  coord_flip()