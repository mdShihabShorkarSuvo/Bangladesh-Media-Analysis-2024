# Load necessary libraries
library(dplyr)
library(tm)
library(SnowballC) # For stemming
library(topicmodels)
library(tidytext)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(RColorBrewer)
library(sentimentr)
library(Rtsne)


# Step 1: Load and Prepare Data
# Load the dataset (replace with your actual file path)
file_path <- "E:/scraped_articles_df (1).csv"
data <- read.csv(file_path, stringsAsFactors = FALSE)

# Preview the data to ensure it loaded correctly
View(data)

# Extract the 'content' column
text_data <- data$content

# Step 2: Preprocess Text Data
# Create a Corpus
corpus <- Corpus(VectorSource(text_data))

# Define custom stop words to remove
custom_stopwords <- c("said", "year", "will", "bangladesh","s","go","told","al","jazeera",'"',"t","sat","hasn","also","many","says") 

# Text preprocessing steps
corpus <- tm_map(corpus, content_transformer(tolower))       # Convert to lowercase
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^a-z ]", " ", x))) # Remove non-alphabetic characters
corpus <- tm_map(corpus, removePunctuation)                 # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)                     # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("english")) # Remove stop words
corpus <- tm_map(corpus, removeWords, custom_stopwords)     # Remove custom stop words
corpus <- tm_map(corpus, stripWhitespace)                   # Remove extra whitespace
#corpus <- tm_map(corpus, stemDocument)                      # Perform stemming

# Tokenize the preprocessed data
tokenized_data <- lapply(corpus, function(doc) unlist(strsplit(as.character(doc), "\\s+")))

# Display the first few tokenized documents
print("Tokenized Data:")
print(tokenized_data[1:5])  # Show tokens for the first 5 documents

# Step 3: Create Document-Term Matrix
# Create a Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(corpus)

# Remove sparse terms to improve model performance
dtm <- removeSparseTerms(dtm, 0.95) # Adjust sparsity threshold as needed

print(dtm)



# Step 2: Calculate TF-IDF
tfidf <- weightTfIdf(dtm)  # Assuming 'dtm' is your Document-Term Matrix
tfidf_matrix <- as.matrix(tfidf)

# Step 3: View the structure of the TF-IDF matrix
dim(tfidf_matrix)   # Check dimensions of the matrix (documents x terms)
head(tfidf_matrix)  # View the first few rows and columns

# Step 4: Summarize TF-IDF values by term
tfidf_summary <- data.frame(
  term = colnames(tfidf_matrix),
  tfidf_sum = colSums(tfidf_matrix),
  tfidf_mean = colMeans(tfidf_matrix)
)

# Step 5: Identify the most important terms
top_tfidf_terms <- tfidf_summary %>%
  arrange(desc(tfidf_sum)) %>%
  head(10)  # Adjust to get the desired number of terms

print(top_tfidf_terms)

# Step 6: Visualize TF-IDF values for the top terms
ggplot(top_tfidf_terms, aes(x = reorder(term, tfidf_sum), y = tfidf_sum)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top Terms by TF-IDF Score",
    x = "Terms",
    y = "TF-IDF Score"
  ) +
  theme_minimal()


# Step 4: Perform Topic Modeling with LDA
num_topics <- 6 # Ensure this matches your LDA model's number of topics
# Define topic levels
topic_labels <- c(
  "1" = "1:Protests and Students",
  "2" = "2: Governance and Leadership",
  "3" = "3: Religion and Minorities",
  "4" = "4: Sports and Cricket",
  "5" = "5: Refugees and Migration",
  "6" = "6: Election and Politics"
)


# Ensure the number of topic labels matches num_topics
if (length(topic_labels) != num_topics) {
  stop("Number of topic labels does not match the number of topics in the LDA model.")
}

# Fit the LDA model
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 42))

# Step 5: Examine Topics
# Extract the most probable words for each topic
top_terms <- terms(lda_model, 10) # Extract top 10 terms for each topic


cat("Top Terms for Each Topic:\n")
print(top_terms)


# Convert LDA output to a tidy format for visualization
lda_tidy <- tidy(lda_model)

# Step 6: Visualizations

## Visualization 1: Bar Plot of Top Terms
# Prepare data for plotting with updated topic names
top_terms_plot <- lda_tidy %>%
  group_by(topic) %>%
  top_n(10, beta) %>% # Adjust the number of terms to display
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(topic = as.factor(topic)) # Ensure topics are factors

# Bar plot of top terms for each topic with custom labels
ggplot(top_terms_plot, aes(term, beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", labeller = labeller(topic = topic_labels)) +
  coord_flip() +
  labs(
    title = "Top Terms in Each Topic",
    x = "Terms",
    y = "Beta"
  )

# Ensure month is matched correctly to lda_tidy
lda_tidy_with_month <- lda_tidy %>%
  mutate(month = rep(data$month, length.out = nrow(lda_tidy)))

# Select top 3 topics per month based on average beta
top_topics_per_month <- lda_tidy_with_month %>%
  group_by(month, topic) %>%
  summarize(avg_beta = mean(beta, na.rm = TRUE)) %>%
  top_n(3, avg_beta) %>% # Select top 3 topics per month
  ungroup()

# Filter the original data to include only the top 3 topics per month
top_terms_monthly_filtered <- lda_tidy_with_month %>%
  semi_join(top_topics_per_month, by = c("month", "topic")) %>%
  group_by(month, topic) %>%
  top_n(5, beta) %>% # Adjust the number of terms to display per topic
  ungroup() %>%
  arrange(month, topic, -beta)


# Create a list of plots, one for each month
plots <- lapply(unique(top_terms_monthly_filtered$month), function(m) {
  ggplot(filter(top_terms_monthly_filtered, month == m), aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    labs(
      title = paste("Top Terms in Each Topic - Month", m),
      x = "Terms",
      y = "Beta"
    ) +
    theme_minimal()
})



# Display plots (if using RStudio or Jupyter Notebook)
plots[[1]] 
plots[[2]]
plots[[3]] 
plots[[4]] 
plots[[5]] 
plots[[6]] 
plots[[7]] 
plots[[8]] 
plots[[9]] 
plots[[10]] 
plots[[11]] 
plots[[12]] 

# Extract topic proportions for each document
topic_proportions <- posterior(lda_model)$topics
cat("\nTopic Proportions for Each Document:\n")
print(head(topic_proportions))


# Add the month column to the topic proportions data
topic_proportions_with_month <- as.data.frame(topic_proportions) %>%
  mutate(month = data$month)  # Add the month column from your dataset

# Reshape data into a long format for aggregation
topic_proportions_long <- topic_proportions_with_month %>%
  pivot_longer(cols = -month, names_to = "topic", values_to = "proportion")

# Aggregate topic proportions by month
topic_proportions_monthly <- topic_proportions_long %>%
  group_by(month, topic) %>%
  summarise(mean_proportion = mean(proportion, na.rm = TRUE), .groups = "drop")

# Map topic numbers to labels in the data
topic_proportions_monthly$topic <- recode(topic_proportions_monthly$topic, !!!topic_labels)

# Plot topic proportions over months
ggplot(topic_proportions_monthly, aes(x = month, y = mean_proportion, color = topic, group = topic)) +
  geom_line(linewidth = 1) +
  labs(title = "Topic Proportions Over Time (Monthly)",
       x = "Month",
       y = "Mean Proportion",
       color = "Topic") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +  # Use month abbreviations for x-axis
  theme_minimal()




## Visualization 2: Line Graph for Topic Proportions
# Create a tidy format of topic proportions for documents
# Add a document index column to topic_proportions
# Create a tidy format of topic proportions for documents
topic_proportions_tidy <- as.data.frame(topic_proportions) %>%
  mutate(document = row_number()) %>%  # Generate unique indices
  pivot_longer(cols = -document, names_to = "topic", values_to = "proportion") %>%
  mutate(topic = as.factor(topic))

# Map topic numbers to labels in the tidy data
topic_proportions_tidy$topic <- recode(topic_proportions_tidy$topic, !!!topic_labels)

# Line graph for topic proportions across documents
ggplot(topic_proportions_tidy, aes(x = document, y = proportion, color = topic, group = topic)) +
  geom_line(size = 1) +
  labs(title = "Topic Proportions Across Documents",
       x = "Document Index",
       y = "Proportion") +
  theme_minimal()





## Visualization 3: Dominant Topics in Documents (Pie Chart)
# Count dominant topics
dominant_topic_count <- dominant_topics %>%
  count(topic_label)

# Pie chart of dominant topics
ggplot(dominant_topic_count, aes(x = "", y = n, fill = topic_label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Dominant Topics in Documents",
       fill = "Topic") +
  theme_void()

# Additional Analysis Ideas:



## 2. Analyze sentiment within each topic

sentiments <- sentiment_by(data$content)
dominant_topics_sentiment <- data.frame(dominant_topics, sentiment = sentiments$ave_sentiment)
print(dominant_topics_sentiment)


# Load necessary libraries


# Assuming 'dominant_topics_sentiment' is your data frame
# Calculate average sentiment per topic
avg_sentiment_per_topic <- dominant_topics_sentiment %>%
  group_by(topic_label) %>%
  summarise(avg_sentiment = mean(sentiment, na.rm = TRUE))

# Plot the data using ggplot2
ggplot(avg_sentiment_per_topic, aes(x = reorder(topic_label, avg_sentiment), y = avg_sentiment, fill = avg_sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + # Flip coordinates for better readability if topics are many
  labs(
    title = "Average Sentiment per Topic",
    x = "Topic",
    y = "Average Sentiment"
  ) +
  theme_minimal() +
  scale_fill_gradient(low = "red", high = "green")

# Visualization 5: t-SNE plot of topics



set.seed(42) # Setting seed for reproducibility

tsne_out <- Rtsne(topic_proportions, perplexity = 10, check_duplicates = FALSE) # You can adjust perplexity

tsne_df <- data.frame(x = tsne_out$Y[,1], 
                      y = tsne_out$Y[,2],
                      topic = factor(apply(topic_proportions, 1, which.max)),
                      topic_label = topic_labels[apply(topic_proportions, 1, which.max)])

ggplot(tsne_df, aes(x=x, y=y, color=topic_label)) +
  geom_point(size = 3) +
  labs(title="t-SNE Visualization of Topics",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme_minimal() +
  guides(color = guide_legend(title="Topic"))



# Visualization 4: Word Clouds for each topic
for(i in 1:num_topics){
  topic_words <- lda_tidy %>%
    filter(topic == i) %>%
    arrange(desc(beta)) %>%
    top_n(50, beta)
  
  wordcloud(words = topic_words$term, freq = topic_words$beta,
            max.words = 100, scale = c(2, 0.5),  random.order = FALSE, colors = brewer.pal(8, "Dark2"),
            main = paste("Word Cloud for Topic", i, topic_labels[i]))
}

