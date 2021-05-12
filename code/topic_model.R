pacman::p_load(tidytext, stm, furrr, ggthemes)

set.seed(123)

ch_df <- read_csv("data/ch_df_preprocessed.csv")

ch_text <- ch_df %>%
  mutate(highlightID = row_number())

ch_text <- ch_text %>%
  unnest_tokens(word, main_text, token = "words") %>%
  anti_join(get_stopwords()) %>%
  filter(!str_detect(word, "[0-9]+")) %>%
  add_count(word) %>%
  filter(n > 100) %>%
  select(-n)

ch_text_sparse <- ch_text %>%
  count(highlightID, word) %>%
  cast_sparse(highlightID, word, n)

plan(multiprocess)

many_models <- data_frame(K = c(1:20) %>%
  mutate(topic_model = future_map(K, ~stm(ch_text_sparse, K = .,
                                          verbose = FALSE)))

heldout <- make.heldout(ch_text_sparse)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, ch_text_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, ch_text_sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicate that a good number of topics would be around 20")

k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(20, 60, 100)) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")

topic_model <- k_result %>% 
  filter(K == 20) %>% 
  pull(topic_model) %>% 
  .[[1]]

td_beta <- tidy(topic_model)

td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(ch_text_sparse))

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.20),
                     labels = scales::percent_format()) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 20 topics by prevalence in the Hacker News corpus",
       subtitle = "With the top words that contribute to each topic")

gamma_terms %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3, 
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))

