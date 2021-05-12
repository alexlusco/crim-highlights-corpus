pacman::p_load(tidyverse)

ch_df <- read_csv("data/ch_df_preprocessed.csv")

ch_df %>%
  select(reference_year) %>%
  filter(!is.na(reference_year)) %>%
  count(reference_year) %>%
  ggplot(aes(x = reference_year, y = n)) +
  geom_col()
