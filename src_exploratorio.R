library(tidyverse)
library(comp3)

eucalyptus

resumo <- eucalyptus %>%
  group_by(site, age) %>% 
  mutate(
    util = available_tree(x, y, 4),
    g = pi * dbh^2 / 40000 * (10000 / area),
    dead = cod1 %in% c(1,2)
  ) %>% 
  summarise(
    dbh = mean(dbh, na.rm =TRUE),
    g = sum(g, na.rm = TRUE),
    surv = (length(tree) - sum(dead)) / length(tree) * 100 
  )

resumo


ggplot(resumo, aes(age, dbh)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site) +
  theme_bw()


ggplot(resumo, aes(age, g)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site) +
  theme_bw()


ggplot(resumo, aes(age, surv)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site) +
  theme_bw()

