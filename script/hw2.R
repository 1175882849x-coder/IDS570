
getwd()


list.files("texts")

file.exists(c(
  "texts/A07594__Circle_of_Commerce.txt",
  "texts/B14801__Free_Trade.txt"
))

# set up ----

# Load packages
library(readr)
library(dplyr)
library(tibble)

# Read the two texts
circle_raw <- read_file("texts/A07594__Circle_of_Commerce.txt")
free_raw <- read_file("texts/B14801__Free_Trade.txt")

# Combine into one table
texts <- tibble(
  doc_title = c("Circle of Commerce", "Free Trade"),
  text = c(circle_raw, free_raw)
)

# Check that both texts were read
texts %>%
  transmute(
    doc_title,
    characters = nchar(text),
    preview = substr(text, 1, 80)
  )

# Part 1 ----
# Read the third document
malynes_raw <- read_file("texts/A06785.txt")

# Combine all three documents
texts <- tibble(
  doc_title = c(
    "Circle of Commerce",
    "Free Trade",
    "A06785"
  ),
  text = c(circle_raw, free_raw, malynes_raw)
)

# Check all three documents
texts %>%
  transmute(
    doc_title,
    characters = nchar(text),
    preview = substr(text, 1, 80)
  )

# STEP 1 ----
library(stringr)
library(tidytext)

# Normalize long s and convert to lowercase
texts <- texts %>%
  mutate(
    text_clean = text %>%
      str_replace_all("ſ", "s") %>%
      str_to_lower()
  )

# Use the same stopwords as your Week 02 script
data("stop_words")

custom_stopwords <- tibble(
  word = c("vnto", "haue", "doo", "hath", "bee", "ye", "thee")
)

all_stopwords <- bind_rows(stop_words, custom_stopwords) %>%
  distinct(word)

# Create one row per word occurrence, then remove stopwords
tokens_clean <- texts %>%
  select(doc_title, text_clean) %>%
  unnest_tokens(word, text_clean) %>%
  anti_join(all_stopwords, by = "word")

# Check the number of remaining tokens per document
tokens_clean %>%
  count(doc_title, name = "remaining_tokens")

# Inspect a few tokens
tokens_clean %>%
  slice_head(n = 10)

# STEP 2----
# Load the Bing sentiment dictionary
bing <- get_sentiments("bing")

# Match words in Texts A and B to the dictionary
sentiment_tokens <- tokens_clean %>%
  filter(doc_title %in% c("Circle of Commerce", "Free Trade")) %>%
  inner_join(bing, by = "word")

# Inspect the matched words and sentiment labels
sentiment_tokens %>%
  slice_head(n = 10)

# STEP 3 ----

library(tidyr)

# Count positive and negative word occurrences
raw_sentiment <- sentiment_tokens %>%
  count(doc_title, sentiment) %>%
  pivot_wider(
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    raw_net = positive - negative
  ) %>%
  rename(
    raw_positive = positive,
    raw_negative = negative
  )

# View the summary
raw_sentiment

# Part 2 ----
library(quanteda)

# Group the existing cleaned tokens by document
tokens_by_doc <- split(
  tokens_clean$word,
  tokens_clean$doc_title
)

# Convert the token lists to a quanteda tokens object
tokens_quanteda <- as.tokens(tokens_by_doc)

# Build the document-feature matrix
dfm_counts <- dfm(tokens_quanteda)

# Check the document names and token totals
docnames(dfm_counts)
ntoken(dfm_counts)

# STEP 4 ----
# Calculate TF-IDF across all three documents
dfm_tfidf_all <- dfm_tfidf(dfm_counts)

# Inspect a small section of the result
print(dfm_tfidf_all, max_ndoc = 3, max_nfeat = 10)

# STEP 5 ----

# Convert the TF-IDF matrix to a data frame
tfidf_wide <- convert(dfm_tfidf_all, to = "data.frame")

# Reshape to one row per document-word combination
tfidf_tidy <- tfidf_wide %>%
  rename(doc_title = doc_id) %>%
  pivot_longer(
    cols = -doc_title,
    names_to = "word",
    values_to = "tfidf"
  ) %>%
  filter(
    tfidf > 0,
    doc_title %in% c("Circle of Commerce", "Free Trade")
  )

# Inspect the result
tfidf_tidy %>%
  slice_head(n = 10)

# Count words with nonzero weights in each document
tfidf_tidy %>%
  count(doc_title, name = "nonzero_words")

# STEP 6 ----

# Match the previously calculated TF-IDF weights to Bing
tfidf_sentiment_words <- tfidf_tidy %>%
  inner_join(bing, by = "word")

# Find the 10 highest-weight sentiment words per document
top_sentiment_words <- tfidf_sentiment_words %>%
  group_by(doc_title) %>%
  slice_max(order_by = tfidf, n = 10, with_ties = FALSE) %>%
  arrange(desc(tfidf), .by_group = TRUE) %>%
  ungroup()

# Display all 20 rows
print(top_sentiment_words, n = 20)

# STEP 7 ----
# Sum sentiment weights for each document
tfidf_sentiment <- tfidf_sentiment_words %>%
  group_by(doc_title) %>%
  summarise(
    tfidf_positive = sum(tfidf[sentiment == "positive"]),
    tfidf_negative = sum(tfidf[sentiment == "negative"]),
    .groups = "drop"
  ) %>%
  mutate(
    tfidf_net = tfidf_positive - tfidf_negative
  )

# View the results
tfidf_sentiment

# Part 3 ----
# Combine the two sentiment summaries
sentiment_comparison <- raw_sentiment %>%
  left_join(tfidf_sentiment, by = "doc_title") %>%
  select(
    doc_title,
    raw_positive,
    raw_negative,
    raw_net,
    tfidf_positive,
    tfidf_negative,
    tfidf_net
  )

# Inspect the final table
print(sentiment_comparison, width = Inf)

# Export the table for submission
write_csv(
  sentiment_comparison,
  "sentiment_comparison.csv"
)

# Confirm that the file was created
file.exists("sentiment_comparison.csv")