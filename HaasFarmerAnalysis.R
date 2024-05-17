library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(tidyverse)
library(gridExtra)
library(tidytext)
#install.packages("utf8")
library(utf8)

farmer <- read.csv("FarmerPosts.csv", stringsAsFactors = F)
haas <- read.csv("C:HaasPosts.csv", stringsAsFactors = F)
mg <- read.csv("C:MG_Insta(v2).csv", stringsAsFactors = F)

# find outliers
which(farmer$likes.views %in% boxplot(as.numeric(farmer$likes.views))$out)
which(haas$likes.views %in% boxplot(as.numeric(haas$likes.views))$out)
which(mg$likes.views %in% boxplot(as.numeric(mg$likes.views))$out)

# take out videos
farmer <- farmer %>% 
  filter(type == "photo")
haas <- haas %>% 
  filter(type == "photo")
mg <- mg %>% 
  filter(type == "photo")


farmer$likes.views <- farmer$likes.views %>% 
  str_remove_all(pattern = ",") %>%
  as.numeric()

haas$likes.views <- haas$likes.views %>% 
  str_remove_all(pattern = ",") %>%
  as.numeric()

mg$likes.views <- mg$likes.views %>% 
  str_remove_all(pattern = ",") %>%
  as.numeric()

boxplot(as.numeric(farmer$likes.views), as.numeric(haas$likes.views), as.numeric(mg$likes.views),
        names = c("Farmer School Likes", "Haas School Likes", "MG Likes")) 

lm_f <- lm(farmer$likes.views ~ farmer$X, data = farmer)
lm_h <- lm(haas$likes.views ~ haas$X, data = haas)
lm_m <- lm(mg$likes.views ~ mg$X, data = mg)

plot(y=farmer$likes.views, x=farmer$X, ylab = "likes", xlab = "post") #Plot the results
abline(lm_f)
title("Farmer Likes vs Time")

plot(y=haas$likes.views, x=haas$X, ylab = "likes", xlab = "post") #Plot the results
abline(lm_f)
title("Haas Likes vs Time")

plot(y=mg$likes.views, x=mg$X, ylab = "likes", xlab = "post") #Plot the results
abline(lm_m)
title("MG Likes vs Time")

farmer <- farmer %>% mutate(follower_est = 6449 - (X * lm_f$coefficients[2]))
haas <- haas %>% mutate(follower_est = 17500 - (X * lm_h$coefficients[2]))
mg <- mg %>% mutate(follower_est = 402 - (X * lm_h$coefficients[2]))

# explore topics
explore_topics <- function(posts){
  posts$hashtags <- posts$hashtags %>% 
    str_remove_all(pattern = "\\[|\\]|\\'|\\'") %>% 
    str_split(pattern = ',')
  
  posts <- posts %>% unnest(cols = hashtags)
  posts$hashtags <- posts$hashtags %>% str_remove_all(pattern = " ")
  
  posts$likes.views <- posts$likes.views %>% 
    str_remove_all(pattern = ",") %>%
    as.numeric()
  
  topics <- posts %>% filter(type == "photo") %>%
    group_by(hashtags) %>%
    summarise(avg_likes = sum(likes.views) / n(), n = n()) %>%
    arrange(-n)
  
  return(topics)
}

explore_mentions <- function(posts){
  posts$mentions <- posts$mentions %>% 
    str_remove_all(pattern = "\\[|\\]|\\'|\\'") %>% 
    str_split(pattern = ',')
  
  posts <- posts %>% unnest(cols = mentions)
  posts$mentions <- posts$mentions %>% str_remove_all(pattern = " ")
  
  posts$likes.views <- posts$likes.views %>% 
    str_remove_all(pattern = ",") %>%
    as.numeric()
  
  topics <- posts %>% filter(type == "photo") %>%
    group_by(mentions) %>%
    summarise(avg_likes = sum(likes.views) / n(), n = n()) %>%
    arrange(-n)
  
  return(topics)
}

grid.table(head(explore_topics(posts = haas), 10))
grid.table(head(explore_topics(posts = farmer),10))
grid.table(head(explore_topics(posts = mg),10))

get_stats <- function(df){
  stats <- df %>% summarise(
    like_rate = sum(likes.views / follower_est) / n(),
    like_trend = lm(likes.views ~ X, data = df)$coefficients[2],
  )
  caption = df %>% unnest(cols = comment)
  caption = caption %>% summarise(l = sum(nchar(comment), na.rm = T) / nrow(df))
  hashtags = explore_topics(df) %>% summarise(h = sum(n) / nrow(df))
  mentions = explore_mentions(df) %>% summarise(m = sum(n) / nrow(df))
  stats <- stats %>% cbind(caption) %>% cbind(hashtags) %>% cbind(mentions)
  return(stats)
}

haas_t <- get_stats(haas)
farmer_t <- get_stats(farmer)
mg_t <- get_stats(mg)

f <- which(farmer$likes.views %in% boxplot(as.numeric(farmer$likes.views))$out)
h <- which(haas$likes.views %in% boxplot(as.numeric(haas$likes.views))$out)
links_f <- c()
for (n in 1:length(f)){
  links_f[n] <- farmer$link[f[n]]
}
links_h <- c()
for (n in 1:length(h)){
  links_h[n] <- farmer$link[h[n]]
}

# Find topics in comments
get_wordlist <- function(titles, sources){
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
  
  series %>%
    anti_join(stop_words) %>%
    group_by(source) %>%
    count(word, sort = TRUE) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(source = factor(source, levels = titles),
           text_order = nrow(.):1) %>%
    ggplot(aes(reorder(word, text_order), n, fill = source)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ source, scales = "free_y") +
    labs(x = NULL, y = "Frequency") +
    coord_flip() +
    theme(legend.position="none")
  
  t <- series %>%
    anti_join(stop_words) %>%
    group_by(source) %>%
    count(word, sort = TRUE) %>%
    ungroup() %>%
    mutate(source = factor(source, levels = titles),
           text_order = nrow(.):1)
  
  return(t)
}

# explore differences in topics
full <- get_wordlist(c("Haas", "Farmer"), 
                     list(haas$comment[-h], farmer$comment[-f]))
full_h <- full %>%
  filter(source == "Haas") %>%
  mutate(n = n / nrow(.)) %>%
  select(word, n)

colnames(full_h) <- c("word", "n_full")
  
full_f <- full %>%
  filter(source == "Farmer") %>%
  mutate(n = n / nrow(.)) %>%
  select(word, n)

colnames(full_f) <- c("word", "n_full")

select <- get_wordlist(c("Haas", "Farmer"), 
                        list(haas$comment[h], farmer$comment[f]))
select_h <- select %>%
  filter(source == "Haas") %>%
  mutate(n = n / nrow(.)) %>%
  select(word, n)
select_f <- select %>%
  filter(source == "Farmer") %>%
  mutate(n = n / nrow(.)) %>%
  select(word, n)


word_dif_h <- full_join(full_h, select_h) 
word_dif_h[is.na(word_dif_h)] <- 0
word_dif_h <- word_dif_h %>% mutate(dif = n - n_full) %>% arrange(-dif)

word_dif_f <- full_join(full_f, select_f) 
word_dif_f[is.na(word_dif_f)] <- 0
word_dif_f <- word_dif_f %>% mutate(dif = n - n_full) %>% arrange(-dif)

# Examine differnce between topics 
intern_test <- farmer[grepl("internship", farmer$comment, fixed = T),]


# Comments
farmer <- read.csv("C:\\Users\\eelrs\\Documents\\MG\\farmer.csv", stringsAsFactors = F)
haas <- read.csv("C:\\Users\\eelrs\\Documents\\MG\\berkeleyhaas.csv", stringsAsFactors = F)

get_wordlist(c("mg"), list(mg$comments))
get_comments <- function(posts){
  posts$comments <- posts$comments %>% 
    str_remove_all(pattern = "\\[|\\]|\\'|\\'") %>% 
    str_split(pattern = ',')
  
  posts <- posts %>% unnest(cols = comments)
  return(posts)
}

fix_likes <- function(posts) {
  posts$likes.views <- posts$likes.views %>% 
    str_remove_all(pattern = ",") %>%
    as.numeric()
  return(posts)
}

haas <- haas %>% fix_likes()
farmer <- farmer %>% fix_likes()

f <- which(farmer$likes.views %in% boxplot(as.numeric(farmer$likes.views))$out)
h <- which(haas$likes.views %in% boxplot(as.numeric(haas$likes.views))$out)

full <- get_wordlist(c("Haas", "Farmer"), 
                     list(haas$comments[-h], farmer$comments[-f]))
outliers <- get_wordlist(c("Haas", "Farmer"), 
                       list(haas$comments[h], farmer$comments[f]))
outliers <- outliers %>% 
  group_by(word) %>% 
  summarise(n = sum(n), avg_per_post = n / (length(f)+length(h))) %>% 
  arrange(-n)

View(outliers[-c(1,2,3,9,17),])

full_h <- full %>%
  filter(source == "Haas") %>%
  mutate(n = n / nrow(.)) %>%
  select(word, n)

colnames(full_h) <- c("word", "n_full")

full_f <- full %>%
  filter(source == "Farmer") %>%
  mutate(n = n / nrow(.)) %>%
  select(word, n)

colnames(full_f) <- c("word", "n_full")

####
outliers_h <- outliers %>%
  filter(source == "Haas") %>%
  mutate(n = n / nrow(.)) %>%
  select(word, n)

colnames(outliers_h) <- c("word", "n_full")

outliers_f <- outliers %>%
  filter(source == "Farmer") %>%
  mutate(n = n / nrow(.)) %>%
  select(word, n)

colnames(outliers_f) <- c("word", "n_full")

full <- get_wordlist(c("Haas", "Farmer"), 
                     list(haas$comments, farmer$comments))
full <- full %>% 
  group_by(word) %>% 
  summarise(n = sum(n), avg_per_post = n / 600) %>% 
  arrange(-n)
full_h <- full %>%
  filter(source == "Haas") %>%
  mutate(n = n / nrow(.)) %>%
  select(word, n)
full_f <- full %>%
  filter(source == "Farmer") %>%
  mutate(n = n / nrow(.)) %>%
  select(word, n)

full_emojis <- c(1,2,3,4,12,13,17,19)
View(full[-full_emojis,])
