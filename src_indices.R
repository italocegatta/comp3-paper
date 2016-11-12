library(tidyverse)
library(forestr)
library(corrr)
library(forcats)
library(viridis)

base_index <- eucalyptus %>%
  group_by(site, age) %>% 
  mutate(
    g = pi * dbh^2 / 40000,
    daniels = dd_daniels(tree, x, y, dbh, n = 6),
    hegyi = dd_hegyi(tree, x, y, dbh, n = 6),
    alemdag = dd_alemdag(tree, x, y, dbh, n = 6),
    martin = dd_martin(tree, x, y, dbh, n = 6),
    corrona = di_corrona(dbh, area),
    steneker = di_steneker(dbh, area),
    vanlcay = di_vanclay(dbh, area),
    wikoff = di_wikoff(dbh, area),
    hamilton = di_hamilton(dbh),
    lorimer = di_lorimer(dbh)
  ) %>%
  arrange(site, tree, age) %>% 
  group_by(site, tree) %>% 
  mutate(
    cai_dbh = c(0, diff(dbh)),
    cai_g = c(0, diff(g))
  ) %>% 
  ungroup()

base_index


base_index_sml <- base_index %>%
  filter(util == TRUE, age > 17, !is.na(class)) %>% 
  select(-c(area, util, x, y, tree, m, sd))

base_index_sml

base_corr <- base_index_sml %>%
  #group_by(site, class, age) %>%
  group_by(site, age) %>%
  nest() %>%
  mutate(
    corr = map(data, correlate),
    corr_dbh = map(corr, focus, cai_dbh),
    corr_g = map(corr, focus, cai_g)
  )

base_corr$corr[[1]] %>%
  focus(cai_dbh) %>%
  filter(! rowname %in% c("g", "dbh", "cai_g")) %>% 
  ggplot(aes(fct_reorder(rowname, cai_dbh), cai_dbh)) +
  geom_col(aes(fill = cai_dbh)) +
  coord_flip() +
  scale_fill_distiller(palette = "Spectral") +
  theme_bw()

df_corr <- base_corr %>% 
  unnest(corr_g) %>% 
  filter(!rowname %in% c("g", "dbh", "cai_g", "cai_dbh"))

df_corr %>% 
  mutate(good = cut(cai_g, breaks = c(-1, -0.6, 0, 0.6, 1))) %>% 
  #filter(good %in% c("(-1,-0.6]", "(0.6,1]")) %>% 
  ggplot(aes(factor(age), cai_g, label = round(cai_g, 1))) +
  geom_text(size = 2) +
  facet_grid(rowname~site, scale = "free_x") +
  theme_bw(base_size = 8)

df_corr %>% 
  mutate(good = cut(cai_g, breaks = c(-1, -0.6, 0, 0.6, 1))) %>% 
  # filter(good %in% c("(-1,-0.6]", "(0.6,1]")) %>% 
  ggplot(aes(factor(age), reorder(rowname, cai_g), fill = cai_g)) +
  geom_tile() +
  facet_wrap(~site, scales = "free") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "blue") +
  theme_bw()

df_corr %>%
  filter(rowname == "daniels") %>% 
  group_by(age) %>% 
  summarise(corr = mean(cai_g)) %>%
  arrange(corr)

a <- base_index %>%
  filter(util == TRUE, !is.na(class), cai_g > 0) %>% 
  select(site, age, tree, dbh, class, cai_g, daniels:lorimer) %>%
  gather(index, value, 7:16) %>%
  filter(index != "martin")

ggplot(a, aes(cai_g, value, color =class %>% factor)) +
  geom_point() +
  facet_wrap(~index, scales = "free")

base_corr_g <- base_index_sml %>%
  select(daniels:cai_g) %>% 
  correlate() %>% 
  focus(cai_g)
#focus(cai_dbh)

base_corr_g %>% arrange()

base_corr_s <- base_index_sml %>%
  group_by(site) %>%
  nest() %>%
  mutate(
    corr = map(data, correlate),
    corr_dbh = map(corr, focus, cai_dbh),
    corr_g = map(corr, focus, cai_g)
  )

df_corr_s <- base_corr_s %>% 
  unnest(corr_g) %>% 
  filter(!rowname %in% c("g", "dbh", "cai_g", "cai_dbh"))

df_corr_s %>% 
  group_by(rowname) %>% 
  summarise(corr = mean(cai_g)) %>%
  arrange(corr)

base_corr_sc <- base_index_sml %>%
  group_by(site, class) %>%
  nest() %>%
  mutate(
    corr = map(data, correlate),
    corr_dbh = map(corr, focus, cai_dbh),
    corr_g = map(corr, focus, cai_g)
  )

df_corr_sc <- base_corr_sc%>% 
  unnest(corr_g) %>% 
  filter(!rowname %in% c("g", "dbh", "cai_g", "cai_dbh"))

df_corr_sc %>% 
  group_by(rowname) %>% 
  summarise(corr = mean(cai_g)) %>%
  arrange(corr)

base_corr_si <- base_index_sml %>%
  group_by(site, age) %>%
  nest() %>%
  mutate(
    corr = map(data, correlate),
    corr_dbh = map(corr, focus, cai_dbh),
    corr_g = map(corr, focus, cai_g)
  )

df_corr_si <- base_corr_si %>% 
  unnest(corr_g) %>% 
  filter(!rowname %in% c("g", "dbh", "cai_g", "cai_dbh"))

df_corr_si %>% 
  group_by(rowname) %>% 
  summarise(corr = mean(cai_g)) %>%
  arrange(corr)