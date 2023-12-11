library(gutenbergr)
library(dplyr)
library(textdata)
library(tidytext)
library(stringr)
library(ggplot2)

#Post 1600 Sample #feng, minor edits by marcin
books_after_1600<-gutenberg_authors%>%
  filter(birthdate >= 1500) #Filter by authors born after 1500 to get books published after 1600
set.seed(444) #set seed for reproducibility

index <- sample(books_after_1600$gutenberg_author_id, 70) #get random sample of authors' id

books_after_1600_1 <- filter(books_after_1600, gutenberg_author_id %in% index) #get corresponding books' title

Sample2 <- gutenberg_works(author %in% books_after_1600_1$author)[1:50,]  #the first 50 books as sample

books_post_1600 <- gutenberg_download(Sample2$gutenberg_id, meta_fields = "title") #download texts

tidy_books_post_1600 <- books_post_1600 %>%
 group_by(title) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) #grouped by titles of book, tokenized by words, and eliminate stop words

bing_positive <- get_sentiments("bing") %>% 
  filter(sentiment == "positive") #get positive words

post_1600_positive<-tidy_books_post_1600 %>%
  inner_join(bing_positive) %>%
  summarize(positive_words = length(word)) #use inner join to find out number of positive words in each books

post_1600_negative<-tidy_books_post_1600 %>%
  anti_join(bing_positive) %>%
  summarize(negative_words = length(word)) #use outer join to find out number of negative words in each books

post_1600_count = merge(post_1600_positive, post_1600_negative, by='title') #merge together to get a table

#Antiquity Sample #marcin
authors_from_antiquity <- c( 
  c(
    "Aeschylus",
    "Ammianus Marcellinus",
    "Aristophanes",
    "Aristotle",
    "Cicero, Marcus Tullius",
    "Demosthenes",
    "Epictetus",
    "Euripides",
    "Herodotus",
    "Hesiod",
    "Homer",
    "Ovid",
    "Plato",
    "Plotinus",
    "Plutarch",
    "Pythagoras",
    "Sappho",
    "Sophocles",
    "Theocritus",
    "Thucydides",
    "Virgil",
    "Xenophon"
  )
) #list of authors from antiquity whose works are featured in the gutenberg project

#data cleaning #marcin
set.seed(123) #seed for the sample: this is important, because the duplicates list later on depends on the seed used for sampling
gutenberg_authors_antiquity <- gutenberg_works(author %in% authors_from_antiquity) #creates subset of all works written by authors in authors_from_antiquities vector

index2 <- sample(gutenberg_authors_antiquity$title, 50) #samples 50 titles from the all of the works from antiquity

sample_titles <- gutenberg_works(title %in% index2) %>% #creates a subset tibble containing the 50 sample works
  arrange(author)

#seed 123. This vector includes manually excluded gutenberg_ids of duplicate works, secondary literature, or conflicting anthologies. This also includes works that have been archived. 1738, 66350
duplicates <- c(19559, 11080, 24856, 41935, 3052, 14140, 8418, 2199, 26073, 16452, 29459, 1738)

sample_titles <- sample_titles[!sample_titles$gutenberg_id %in% duplicates, ] %>% #removes duplicates from sample_titles
  arrange(author)

new_gaa <- subset(gutenberg_authors_antiquity[!gutenberg_authors_antiquity$gutenberg_id %in% sample_titles$gutenberg_id & !gutenberg_authors_antiquity$gutenberg_id %in% duplicates, ]) %>% #removes works in sample_titles as well duplicates from the original "population" of works from antiquity. This subset contains works not used in the sample, and is used to add unique samples to the sample_titles set.
  arrange(author) 
print(new_gaa, n=100)

additional_books <- new_gaa %>% #New unique books are selected from the new_gaa set. 
  filter(gutenberg_id %in% c(42543, 232, 1181, 348, 230, 28621, 2131, 2456, 66350))  

sample_titles <- bind_rows(sample_titles, additional_books) #additional_books is merged with sample_titles; the sample_titles tibble now contains 50 unique books. 

books_antiquity <- gutenberg_download(sample_titles$gutenberg_id, meta_fields = "title") #downloads the works from antiquity

# unique_ids <- unique(books_antiquity$gutenberg_id) #checks if any books were archived; if archived, their id is noted and they are added to the duplicates list above. Process is iterated until data is clean, available, and unique. 
# missing_books <- sample_titles[!sample_titles$gutenberg_id %in% unique_ids,]

#sentiment analysis and tokenization #marcin

tidy_books_antiquity <- books_antiquity %>% #groups works by title, tokenizes by words, and removes stop words
  group_by(title) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

antiquity_positive <- tidy_books_antiquity %>% #counts the positive words for each book
  inner_join(bing_positive) %>%
  summarize(positive_words = length(word)) 

antiquity_negative <- tidy_books_antiquity %>% #counts the negative words for each book
  anti_join(bing_positive) %>%
  summarize(negative_words = length(word)) 

counts_antiquity <- merge(antiquity_positive, antiquity_negative, by='title') %>% #Creates new tibble that includes the counts for positive and negative words, as well as the titles of the books
  left_join(sample_titles %>% select(title, author), by = "title") %>% 
  arrange(author)

#score calculation #marcin

score_function <- function(data) { #creates new column that contains the positive to negative score for each book. 
  data %>%
    group_by(title) %>%
    mutate(sentiment_score = (100 * (positive_words/negative_words))) %>%
    ungroup()
}

post_1600_count <- score_function(post_1600_count)
counts_antiquity <- score_function(counts_antiquity)

#plotting of distributions #marcin
ggplot(counts_antiquity, aes(x = sentiment_score)) +
  coord_cartesian(xlim = c(0, 18), ylim = c(0, 8)) +
  geom_histogram(binwidth = 0.7, fill = "#4B9CD3", color = "black") +
  labs(title = "Distribution of Sentiment Scores for Books Written in Antiquity",
       x = "Sentiment Score",
       y = "Frequency")

ggplot(post_1600_count, aes(x = sentiment_score)) +
  coord_cartesian(xlim = c(0, 18), ylim = c(0, 8)) +
  geom_histogram(binwidth = 0.7, fill = "red", color = "black") +
  labs(title = "Distribution of Sentiment Scores for Books Written after 1600",
       x = "Sentiment Score",
       y = "Frequency")

#some exploratory data analysis [EDA] #feng
counts_antiquity <- counts_antiquity[,-4]
summary(post_1600_count)
summary(counts_antiquity)

# violin plot #feng
counts_antiquity$category <- 'Antiquity'
post_1600_count$category <- 'Post-1600'# Add a new column to each dataset to indicate the category
combined_data <- rbind(counts_antiquity, post_1600_count) # Combine the two datasets
#The code prepares data for a violin plot comparing sentiment scores between books from antiquity and after 1600.
#It adds a new column 'category' to each dataset to indicate its category.

ggplot(combined_data, aes(x = category, y = sentiment_score, fill = category)) +
  geom_violin(trim = TRUE) +
  labs(title = "Comparison of Sentiment Scores",
       x = "Category",
       y = "Sentiment Score") +
  scale_fill_manual(values = c("#4B9CD3", "red")) #draw a violin plot to provide a better understanding of the density distribution of the sentiment scores
#The x-axis represents the category (Antiquity or Post-1600), and the y-axis represents the sentiment score.
#The plot provides a visual comparison of the density distribution of sentiment scores between the two categories.

# scatter plot #feng
ggplot(combined_data[-35,], aes(x = positive_words, y = negative_words, color = category)) + #exclude outlier
  geom_point(alpha = 0.7) + #transparency
  geom_smooth(method = "lm", se = FALSE) + #fit line
  scale_color_manual(values = c("#4B9CD3", "red")) + #different colors for two samples
  labs(title = "Scatter Plot of Word Counts with Sentiment Scores across different books in two sample",
       x = "Positive Word Count",
       y = "Negative Word Count",
       color = "Category")

# Comparative Cumulative Distribution Plot #feng 
ggplot(combined_data, aes(x = sentiment_score, color = category)) +
  stat_ecdf(geom = "step") +
  labs(title = "Comparative CDF of Sentiment Scores",
       x = "Sentiment Score",
       y = "Cumulative Probability") +
  scale_color_manual(values = c("#4B9CD3", "red"))

#in each sample, about 80% of books have sentiment scores lower than 10

#t test #arman 
t_test_result <- t.test(
  x = post_1600_count$sentiment_score,
  y = counts_antiquity$sentiment_score,
  alternative = "two.sided",
  mu = 0,
  paired = FALSE,
  var.equal = FALSE
)

# Print the t-test result
print(t_test_result)
