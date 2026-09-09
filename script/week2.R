# load packages -----
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(forcats)
library(tibble)
library(scales)

# read files ----
# You will need the correct file paths if you don't follow my naming conventions:
file_a <- "texts/A07594__Circle_of_Commerce.txt"
file_b <- "texts/B14801__Free_Trade.txt"

# Read the raw text files into R
text_a <- read_file(file_a)
text_b <- read_file(file_b)

# Combine into a tibble for tidytext workflows
texts <- tibble(
  doc_title = c("Text A", "Text B"),
  text = c(text_a, text_b)
)

texts

# build stopwords ----
# Start with tidytext's built-in stopword list
data("stop_words")

# Add our own project-specific stopwords (you can, and will, expand this list later)
custom_stopwords <- tibble(
  word = c(
    "vnto", "haue", "doo", "hath", "bee", "ye", "thee"
  )
)

all_stopwords <- bind_rows(stop_words, custom_stopwords) %>%
  distinct(word)

all_stopwords %>% slice(1:10)

# word count -----
word_counts <- texts %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  anti_join(all_stopwords, by = "word") %>%
  count(doc_title, word, sort = TRUE)

word_counts

# plot ----

plot_n_words <- 20  # you can change this as needed

# Select the most frequent words overall
word_comparison_tbl <- word_counts %>%
  pivot_wider(
    names_from = doc_title,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(max_n = pmax(`Text A`, `Text B`)) %>%
  arrange(desc(max_n))

word_plot_data <- word_comparison_tbl %>%
  slice_head(n = plot_n_words) %>%
  pivot_longer(
    cols = c(`Text A`, `Text B`),
    names_to = "doc_title",
    values_to = "n"
  ) %>%
  mutate(word = fct_reorder(word, n, .fun = max))

ggplot(word_plot_data, aes(x = n, y = word)) + #black magic happens thanks to ggplot
  geom_col() +
  facet_wrap(~ doc_title, scales = "free_x") +
  labs(
    title = "Most frequent words (stopwords removed)",
    subtitle = paste0(
      "Top ", plot_n_words,
      " words by maximum frequency across both texts"
    ),
    x = "Word frequency",
    y = NULL
  ) +
  theme_minimal()

# context tokenization ----

bigrams <- texts %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams

# separate bigram -----
bigrams_separated <- bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")

bigrams_separated

# remove stopwords -----

bigrams_filtered <- bigrams_separated %>%
  filter(
    !word1 %in% all_stopwords$word,
    !word2 %in% all_stopwords$word
  )

bigrams_filtered

# count bigram -----
bigram_counts <- bigrams_filtered %>%
  count(doc_title, word1, word2, sort = TRUE)

bigram_counts

# regroup bigram -----
bigram_counts <- bigram_counts %>%
  unite(bigram, word1, word2, sep = " ")

bigram_counts

# compare bigrams
bigram_relative <- bigram_counts %>%
  group_by(doc_title) %>%
  mutate(
    total_bigrams = sum(n),
    proportion = n / total_bigrams
  ) %>%
  ungroup()

bigram_wide <- bigram_relative %>%
  select(doc_title, bigram, proportion) %>%
  pivot_wider(
    names_from = doc_title,
    values_from = proportion,
    values_fill = 0
  )

bigram_wide

# bigram diff
bigram_diff <- bigram_wide %>%
  mutate(
    diff = `Text A` - `Text B`
  ) %>%
  arrange(desc(abs(diff)))

bigram_diff %>% slice(1:20)

# homework 1 part1 ----
words <- texts %>%
  unnest_tokens(word, text)%>%
  mutate(word = str_to_lower(word))
head(words)
# total tokens ----
total_tokens <- words%>%
  count(doc_title, name = "total_tokens")

# tokens after stopwords ----
words_after_stopwords <- words %>%
  anti_join(all_stopwords, by = "word")

tokens_after_stopwords <- words_after_stopwords%>%
  count(doc_title, name = "tokens_after_stopwords")
tokens_after_stopwords

# attach together ----
corpus_diagnostics <- total_tokens%>%
  left_join(tokens_after_stopwords, by = "doc_title")%>%
  mutate(tokens_removed = total_tokens - tokens_after_stopwords,
         percent_removed = tokens_removed/total_tokens *100)
corpus_diagnostics

# homework1 part2 ----

# A. before stopwords removal ----
before_removal <- words%>%
  count(doc_title, word, sort = TRUE)

# plot before removal ----

plot_n_words <- 15

word_comparison_before <-before_removal%>%
  pivot_wider(
    names_from = doc_title,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(max_n = pmax(`Text A`, `Text B`)) %>%
  arrange(desc(max_n))

word_plot_before <- word_comparison_before %>%
  slice_head(n = plot_n_words) %>%
  pivot_longer(
    cols = c(`Text A`, `Text B`),
    names_to = "doc_title",
    values_to = "n"
  ) %>%
  mutate(word = fct_reorder(word, n, .fun = max))

ggplot(word_plot_before, aes(x = n, y = word)) + #black magic happens thanks to ggplot
  geom_col() +
  facet_wrap(~ doc_title, scales = "free_x") +
  labs(
    title = "Most frequent words before stopwords removed",
    subtitle = paste0(
      "Top ", plot_n_words,
      " words by maximum frequency across both texts"
    ),
    x = "Word frequency",
    y = NULL
  ) +
  theme_minimal()

# B. after stopwords removal ----
after_removal <- words_after_stopwords%>%
  count(doc_title, word, sort = TRUE)
after_removal

 # plot after removal ----

word_comparison_after <- after_removal%>%
  pivot_wider(
    names_from = doc_title,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(max_n = pmax(`Text A`, `Text B`)) %>%
  arrange(desc(max_n))

word_plot_after <- word_comparison_after %>%
  slice_head(n = plot_n_words) %>%
  pivot_longer(
    cols = c(`Text A`, `Text B`),
    names_to = "doc_title",
    values_to = "n"
  ) %>%
  mutate(word = fct_reorder(word, n, .fun = max))

ggplot(word_plot_after, aes(x = n, y = word)) + #black magic happens thanks to ggplot
  geom_col() +
  facet_wrap(~ doc_title, scales = "free_x") +
  labs(
    title = "Most frequent words after stopwords removed",
    subtitle = paste0(
      "Top ", plot_n_words,
      " words by maximum frequency across both texts"
    ),
    x = "Word frequency",
    y = NULL
  ) +
  theme_minimal()

# homework1 part3 ----
# remove clean stopwords ----
after_removal_clean <- texts %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  anti_join(stop_words, by = "word") %>%
  count(doc_title, word, sort = TRUE)
after_removal_clean

word_comparison_clean <- after_removal_clean %>%
  pivot_wider(
    names_from = doc_title,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(max_n = pmax(`Text A`, `Text B`)) %>%
  arrange(desc(max_n))

tokens_after_standard <- word_comparison_clean%>%
  slice_head(n = 15)
tokens_after_standard

# check all tables ----
top20_before <- word_comparison_before %>%
  slice_head(n = 20)

top20_after <- word_comparison_after %>%
  slice_head(n = 20)

top20_clean <- word_comparison_clean %>%
  slice_head(n = 20)

top20_before
top20_after
top20_clean
