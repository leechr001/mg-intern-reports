library(dplyr)
library(tidyverse)
library(stringr)
#install.packages("tidytext")
#install.packages("textdata")
library(tidytext)

linkedIn <- read.csv("C:\\Users\\eelrs\\Downloads\\MG_Data_Combined - Update engagement.csv", stringsAsFactors = FALSE)
instagram <- read.csv("C:\\Users\\eelrs\\Downloads\\MG_Data_Combined - Instagram.csv", stringsAsFactors = FALSE)
blog <- read.csv("C:\\Users\\eelrs\\Downloads\\MG_Data_Combined - Blogs.csv", stringsAsFactors = FALSE)

titles <- c("LinkedIn", "Instagram", "Blog")

sources <- list(linkedIn$Update.title, instagram$PostTitle, blog$Content)

series <- tibble()

for(i in seq_along(titles)) {
  
  clean <- tibble(Title = seq_along(sources[[i]]),
                  text = sources[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(source = titles[i]) %>%
    select(source, everything())
  
  series <- rbind(series, clean)
}

# set factor to keep sources in original order
series$source <- factor(series$source, levels = rev(titles))

# plot
series %>%
  anti_join(stop_words) %>%
  group_by(source) %>%
  count(word, sort = TRUE) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(source = factor(source, levels = titles),
         text_order = nrow(.):1) %>%
  ggplot(aes(reorder(word, text_order), n, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ source, scales = "free_y") +
  labs(x = NULL, y = "Frequency") +
  coord_flip() +
  theme(legend.position="none")

# Calculate word frequency within each source
# Compare to overall frequency to test for significance

# calculate percent of word use across all media
media_pct <- series %>%
  anti_join(stop_words) %>%
  count(word) %>%
  transmute(word, all_words = n / sum(n))

# calculate percent of word use within each source
frequency <- series %>%
  anti_join(stop_words) %>%
  count(source, word) %>%
  mutate(source_words = n / sum(n)) %>%
  left_join(media_pct) %>%
  arrange(desc(source_words)) %>%
  ungroup()

# visualize what is happening
ggplot(frequency, aes(x = source_words, y = all_words, color = abs(all_words - source_words))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~ source, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Media Sources", x = NULL)

# Calculate correlation and p-values
CorrelationBetween <- frequency %>%
              group_by(source) %>%
              summarize(correlation = cor(source_words, all_words),
                        p_value = cor.test(source_words, all_words)$p.value)
# notice all are very close to 0 so there is significant correlation between
# Note: This is better suited for more than one source, plan to update across all platforms

# Begin Sentiment Analysis
series %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

# Look at change in sentiments over posts
series %>%
  group_by(source) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(source, Title = Title , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         source = factor(source, levels = titles)) %>%
  ggplot(aes(-Title, sentiment, fill = source)) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
  labs(y = "sentiment", x = "Post (0 is most recent)") +
  facet_wrap(~ source, ncol = 2, scales = "free_x")


# Find most common sentiment workds
bing_word_counts <- series %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

# Set up regressions
# Save sentiment by post
linkedInAnalysis <- series %>%
  group_by(source) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(source, Title = Title , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         source = factor(source, levels = titles)) %>% 
  filter(source == "LinkedIn") %>%
  merge(linkedIn, by.x = 0, by.y = 0) %>%
  select(Update.link, sentiment, Engagement.rate, Click.through.rate..CTR., Impressions, Likes, Clicks)

InstagramAnalysis <- series %>%
  group_by(source) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(source, Title = Title , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         source = factor(source, levels = titles)) %>% 
  filter(source == "Instagram") %>%
  merge(instagram, by.x = 0, by.y = 0) %>%
  select(sentiment, Likes)

BlogAnalysis <- series %>%
  group_by(source) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(source, Title = Title , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         source = factor(source, levels = titles)) %>% 
  filter(source == "Blog") %>%
  merge(blog, by.x = 0, by.y = 0) %>%
  select(Title.y, Content, sentiment)
  

# plot relationship between sentiment, engagement rate, likes for linkedIn
ggplot(linkedInAnalysis, aes(sentiment, Engagement.rate)) +
  geom_point() +
  stat_smooth()

ggplot(linkedInAnalysis, aes(sentiment, Click.through.rate..CTR.)) +
  geom_point() +
  stat_smooth()

ggplot(linkedInAnalysis, aes(sentiment, Likes)) +
  geom_point() +
  stat_smooth()

ggplot(linkedInAnalysis, aes(sentiment, Clicks)) +
  geom_point() +
  stat_smooth()

ggplot(linkedInAnalysis, aes(sentiment, Impressions)) +
  geom_point() +
  stat_smooth()

# plot for instagram
ggplot(InstagramAnalysis, aes(sentiment, Likes)) +
  geom_point() +
  stat_smooth()

# Examples of ideal sentiment, tiebreaking by engagement rate
temp <- linkedInAnalysis %>% 
  mutate(ideal = abs(5 - sentiment)) %>%
  arrange(ideal, -Engagement.rate) %>%
  select(Update.link, ideal) %>%
  head(3)
