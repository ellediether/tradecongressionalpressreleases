# Load libraries
library(readr)
library(dplyr)
library(text2vec)
library(caret)
library(ggplot2)
library(tidytext)
library(stringr)
library(tm)
library(stopwords)
library(wordcloud)
library(RColorBrewer)
library(lubridate)
library(glmnet)
library(tidyr)

# --- Load and Prepare Gold Dataset ---
gold_data <- read_csv("C:/Users/Elle/Documents/press_sample_1000.csv") %>%
  select(Body, is_trade) %>%
  mutate(is_trade = factor(is_trade))


gold_data <- gold_data %>%
  mutate(is_trade = if_else(is.na(as.numeric(as.character(is_trade))), 0, as.numeric(as.character(is_trade))))


# --- Clean UTF-8 Encoding ---
gold_data$Body <- iconv(gold_data$Body, from = "latin1", to = "UTF-8", sub = "")
gold_data$Body <- gsub("[^[:print:]]", " ", gold_data$Body)

# --- Train/Test Split ---
set.seed(428)
train_index <- createDataPartition(gold_data$is_trade, p = 0.8, list = FALSE)
train_data <- gold_data[train_index, ]
test_data  <- gold_data[-train_index, ]

# --- Tokenize and Create Vocabulary ---
it_train <- itoken(train_data$Body, 
                   preprocessor = tolower,
                   tokenizer = word_tokenizer)

vocab <- create_vocabulary(it_train, stopwords = stopwords("en")) %>%
  prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.5)

vectorizer <- vocab_vectorizer(vocab)

# --- Create DTM ---
dtm_train <- create_dtm(it_train, vectorizer)
dtm_train <- dtm_train[, colSums(dtm_train) > 0]  # Remove zero-variance terms

# --- Fit Regularized Logistic Regression ---
x <- dtm_train
y <- train_data$is_trade

cv_model <- cv.glmnet(x, y, family = "binomial", alpha = 0)  # Ridge (alpha = 0)
best_lambda <- cv_model$lambda.min
final_model <- glmnet(x, y, family = "binomial", alpha = 0, lambda = best_lambda)

# --- Evaluate on Test Set ---
it_test <- itoken(test_data$Body, preprocessor = tolower, tokenizer = word_tokenizer)
dtm_test <- create_dtm(it_test, vectorizer)
dtm_test <- dtm_test[, colnames(dtm_test) %in% colnames(dtm_train)]  # match train features

probs <- predict(final_model, newx = dtm_test, type = "response")
preds <- ifelse(probs > 0.5, "1", "0")

confusionMatrix(
  factor(preds, levels = c("0", "1")),
  factor(as.character(test_data$is_trade), levels = c("0", "1"))
)

# --- Load Full Dataset ---
press_data_full <- read_csv("C:/Users/Elle/Downloads/press_releases_details_full.csv")

# --- Clean UTF-8 and remove non-printable characters ---
press_data_full$Body <- iconv(press_data_full$Body, from = "latin1", to = "UTF-8", sub = "")
press_data_full$Body <- gsub("[^[:print:]]", " ", press_data_full$Body)

# --- Create DTM for Full Dataset ---
it_full <- itoken(press_data_full$Body,
                  preprocessor = tolower,
                  tokenizer = word_tokenizer)

dtm_full <- create_dtm(it_full, vectorizer)

# --- Match columns to training DTM (required for glmnet) ---
common_terms <- intersect(colnames(dtm_full), colnames(dtm_train))
dtm_full_aligned <- dtm_full[, common_terms, drop = FALSE]

# Add zero columns for any missing terms in test data
missing_terms <- setdiff(colnames(dtm_train), colnames(dtm_full_aligned))
for (term in missing_terms) {
  dtm_full_aligned <- cbind(dtm_full_aligned, Matrix::Matrix(0, nrow = nrow(dtm_full_aligned), ncol = 1, sparse = TRUE))
  colnames(dtm_full_aligned)[ncol(dtm_full_aligned)] <- term
}

# Reorder columns to match training matrix
dtm_full_aligned <- dtm_full_aligned[, colnames(dtm_train)]

# --- Predict Trade-Related Probabilities ---
full_probs <- predict(final_model, newx = dtm_full_aligned, type = "response")
press_data_full <- press_data_full %>%
  mutate(is_trade_pred = as.numeric(full_probs) > 0.5)

# Optional: View or save the predicted subset
tariff_data <- press_data_full %>%
  filter(is_trade_pred == TRUE)

# Optional: Save to CSV
write_csv(tariff_data, "predicted_trade_press_releases.csv")


# --- Visualization A: % Tariff Mentions by Member ---
total_tariff <- nrow(tariff_data)

tariff_data %>%
  count(Member, sort = TRUE) %>%
  mutate(percent = 100 * n / total_tariff) %>%
  ggplot(aes(x = reorder(Member, percent), y = percent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Percent of Tariff-Related Press Releases by Member",
       x = "Member", y = "Percent") +
  theme_minimal(base_size = 14)


# Fix and parse Date column (if not already done)
tariff_data <- tariff_data %>%
  mutate(Date = mdy(Date))  # or use ymd(Date) depending on format

# Confirm it worked:
glimpse(tariff_data$Date)  # should show <date> not <chr>


# --- Visualization B: Timeline ---
ggplot(tariff_data, aes(x = Date)) +
  geom_histogram(binwidth = 30, fill = "darkgreen", color = "white") +
  labs(title = "Frequency of Tariff-Related Releases Over Time",
       x = "Date", y = "Number of Press Releases") +
  theme_minimal(base_size = 14)

# --- Visualization C: Top Tariff Terms ---
tariff_terms <- c("tariff", "tariffs", "duty", "duties", "levy", "levies", "section", "buy", "america", "export", "import")
term_counts <- tariff_data %>%
  unnest_tokens(word, Body) %>%
  filter(word %in% str_replace_all(tariff_terms, " ", "")) %>%
  count(word, sort = TRUE) %>%
  mutate(percent = 100 * n / sum(n))

ggplot(term_counts, aes(x = reorder(word, percent), y = percent)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(title = "Top Tariff Terms (as % of mentions)", x = "Term", y = "Percent") +
  theme_minimal(base_size = 14)

# --- Word Cloud ---
words_df <- tariff_data %>%
  unnest_tokens(word, Body) %>%
  filter(!word %in% stopwords("en"),
         !word %in% c("said", "will", "also", "one", "new", "can", "must", "may", "just", "get", "hassan")) %>%
  count(word, sort = TRUE)

par(mar = rep(0, 4))
par(xpd = TRUE)
set.seed(123)
wordcloud(words = words_df$word, freq = words_df$n,
          min.freq = 2, max.words = 100, random.order = FALSE,
          rot.per = 0.25, scale = c(3, 0.6),
          colors = brewer.pal(8, "Dark2"))

# --- Load 118th Congress Member Info ---
members <- read_csv("C:/Users/Elle/Downloads/118thCongressMembers.csv") %>%
  mutate(
    Member_clean = str_to_lower(str_c(first_name, last_name, sep = " ")),
    Party = party,
    Chamber = case_when(
      type == "sen" ~ "Senate",
      type == "rep" ~ "House",
      TRUE ~ NA_character_
    )
  ) %>%
  select(Member_clean, Party, Chamber)

# --- Clean Member Name in Press Dataset ---
press_data_full <- press_data_full %>%
  mutate(Member_clean = str_to_lower(Member))

# --- Join Member Info to Press Releases ---
press_data_full <- press_data_full %>%
  left_join(members, by = "Member_clean")

# --- Optional: Subset to Trade-Related Only (if not already) ---
trade_data <- press_data_full %>%
  filter(is_trade_pred == TRUE)

# --- Visualization: Party Differences in Trade Mentions ---
trade_data %>%
  count(Party, sort = TRUE) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ggplot(aes(x = reorder(Party, percent), y = percent, fill = Party)) +
  geom_col() +
  coord_flip() +
  labs(title = "Trade-Related Press Releases by Party",
       x = "Party", y = "Percent") +
  scale_fill_manual(values = c("Republican" = "firebrick", "Democrat" = "dodgerblue", "Independent" = "gray50")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# --- Optional: Chamber Breakdown (e.g., House vs Senate) ---
trade_data %>%
  count(Chamber, Party) %>%
  ggplot(aes(x = Chamber, y = n, fill = Party)) +
  geom_col(position = "dodge") +
  labs(title = "Trade-Related Press Releases by Chamber and Party",
       x = "Chamber", y = "Count") +
  scale_fill_manual(values = c("Republican" = "firebrick", "Democrat" = "dodgerblue", "Independent" = "gray50")) +
  theme_minimal(base_size = 14)


# --- Justification Flags ---
security_terms <- c("national security", "defense", "homeland security", "foreign threats", "espionage")
protectionist_terms <- c("protect american jobs", "buy american", "reshoring", "dumping", "domestic industry")

tariff_data <- tariff_data %>%
  mutate(
    justification_security = str_detect(tolower(Body), str_c(security_terms, collapse = "|")),
    justification_protectionist = str_detect(tolower(Body), str_c(protectionist_terms, collapse = "|"))
  )

# --- Cue Flags ---
bipartisan_terms <- c("bipartisan", "across the aisle", "both parties", "working together", "consensus")
partisan_terms <- c("democrat agenda", "republican agenda", "liberal", "conservative", "partisan politics")

tariff_data <- tariff_data %>%
  mutate(
    cue_bipartisan = str_detect(tolower(Body), str_c(bipartisan_terms, collapse = "|")),
    cue_partisan = str_detect(tolower(Body), str_c(partisan_terms, collapse = "|"))
  )

# --- Justification & Cue Counts ---
long_justifications <- tariff_data %>%
  pivot_longer(cols = starts_with("justification_"), names_to = "Justification", values_to = "Present") %>%
  filter(Present) %>%
  count(Justification)

long_cues <- tariff_data %>%
  pivot_longer(cols = starts_with("cue_"), names_to = "Cue", values_to = "Present") %>%
  filter(Present) %>%
  count(Cue)

# Compute percentages for justification types
long_justifications <- long_justifications %>%
  mutate(percent = 100 * n / sum(n))

ggplot(long_justifications, aes(x = reorder(Justification, percent), y = percent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Percent of Justification Types", x = "Justification", y = "Percent") +
  theme_minimal(base_size = 14)

# Compute percentages for cue types
long_cues <- long_cues %>%
  mutate(percent = 100 * n / sum(n))

ggplot(long_cues, aes(x = reorder(Cue, percent), y = percent)) +
  geom_col(fill = "pink") +
  coord_flip() +
  labs(title = "Percent of Cue Types", x = "Cue", y = "Percent") +
  theme_minimal(base_size = 14)


# --- Sentiment Analysis ---
sentiment_data <- tariff_data %>%
  select(Body, cue_bipartisan, cue_partisan, justification_security, justification_protectionist) %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, Body) %>%
  filter(!word %in% stopwords("en")) %>%
  inner_join(get_sentiments("bing"), by = "word")

sentiment_summary <- sentiment_data %>%
  pivot_longer(cols = starts_with("cue_") | starts_with("justification_"),
               names_to = "Type", values_to = "Flag") %>%
  filter(Flag) %>%
  count(Type, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(Net_Sentiment = positive - negative)

# Recalculate sentiment as percent of total sentiment words
sentiment_summary <- sentiment_summary %>%
  mutate(
    total_words = positive + negative,
    percent_positive = 100 * positive / total_words,
    percent_negative = 100 * negative / total_words,
    net_sentiment_percent = percent_positive - percent_negative
  )

# Plot net sentiment as percent difference
ggplot(sentiment_summary, aes(x = reorder(Type, net_sentiment_percent), y = net_sentiment_percent)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Net Sentiment (% Positive - % Negative)",
       x = "Justification or Cue Type", y = "Net Sentiment (%)") +
  theme_minimal(base_size = 14)



# China Hypothesis 2 ------------------------------------------------------

# --- China and Related Terms ---
china_terms <- c("china", "chinese", "beijing", "xi jinping", "prc", "cpc")

# --- Other Foreign Countries/Regions ---
other_country_terms <- c(
  "russia", "putin", "kremlin", "iran", "tehran", "north korea", "pyongyang",
  "mexico", "canada", "europe", "european union", "germany", "france", "uk", "britain", 
  "saudi arabia", "india", "brazil", "turkey", "venezuela", "cuba"
)
# Add flags for mentions
tariff_data <- tariff_data %>%
  mutate(
    mentions_china = str_detect(tolower(Body), str_c(china_terms, collapse = "|")),
    mentions_other_foreign = str_detect(tolower(Body), str_c(other_country_terms, collapse = "|")),
    foreign_mention_type = case_when(
      mentions_china & mentions_other_foreign ~ "China + Other",
      mentions_china ~ "China Only",
      mentions_other_foreign ~ "Other Only",
      TRUE ~ "No Mention"
    )
  )
tariff_data %>%
  count(foreign_mention_type) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ggplot(aes(x = reorder(foreign_mention_type, percent), y = percent, fill = foreign_mention_type)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(
    "China Only" = "#e41a1c",
    "Other Only" = "#377eb8",
    "China + Other" = "#4daf4a",
    "No Mention" = "#999999"
  )) +
  labs(title = "Foreign Country Mentions in Press Releases",
       x = "Mention Type", y = "Percent") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")



