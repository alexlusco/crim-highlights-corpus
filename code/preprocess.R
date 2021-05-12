pacman::p_load(stringr, dplyr, readr, tidyr)

ch_df <- read_csv("data/ch_df_raw.csv")

ch_df <- ch_df %>%
  mutate(reference_full = str_extract(main_text, pattern = "(?<=Reference:)(.*)(?=Back to top)"),
         reference_full = str_squish(reference_full))

ch_df <- ch_df %>%
  mutate(authors_raw = str_extract(reference_full, pattern = "^(.*?)(?=\\.|\\()"),
         authors_raw = str_squish(authors_raw))

ch_df <- ch_df %>%
  mutate(authors_raw = str_replace(authors_raw, "( and |&)", "\\,")) %>%
  mutate(number_of_authors = str_count(authors_raw, "\\,"),
         number_of_authors = as.numeric(number_of_authors)) %>%
  mutate(others = str_extract(authors_raw, pattern = "[0-9]+"),
         others = as.numeric(others)) %>%
  mutate(others = case_when(
    is.na(others) ~ number_of_authors,
    TRUE ~ (others+number_of_authors-1)
  )) %>%
  mutate(number_of_authors = others) %>%
  select(-others)

ch_df <- ch_df %>%
  mutate(reference_year = str_extract(reference_full, pattern = "[0-9]{4}(?=\\))"))

write_csv(ch_df, "data/ch_df_preprocessed.csv")

