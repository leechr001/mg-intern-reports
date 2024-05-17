library(dplyr)
#install.packages("countcolors")
#install.packages("colordistance")
#install.packages("imager")
library(imager)
library(countcolors)
library(colordistance)
library(ggplot2)

df <- read.csv("\MG_Data_Combined - Instagram (1).csv",
               stringsAsFactors = FALSE)

df <- df %>% select(-Discovery, -Interactions)
#set up image functionality
path <- "Data_Instagram\\"

fill_path <- function(n) {
  return(paste0(path,n,".jpg"))
}

image_path <- c()

for (i in 1:47) {
  # 2 is video, post 36 irrelevant
  if (i == 2 || i > 36) {
    image_path[i] = NA
    next
  }
  image_path[i] = fill_path(i)
}

df <- cbind(df, image_path, stringsAsFactors = FALSE)

# explore/test image functions
sample_pic <- df$image_path[1]
plot(load.image(sample_pic))
colordistance::plotPixels(sample_pic, lower = NULL, upper = NULL, n = 5000)

kmeans.clusters <- colordistance::getKMeanColors(sample_pic, n = 5, plotting = TRUE)
clusters <- colordistance::extractClusters(kmeans.clusters) %>% arrange(-Pct)
colordistance::plotClusters(clusters)


# Read the image into the R environment
samp <- jpeg::readJPEG(sample_pic)

# Find all the pixels within a 10% radius
color_1 <- countcolors::sphericalRange(samp,
                                       center = c(clusters$R[1], clusters$G[1], clusters$B[1]),
                                       radius = 0.15, color.pixels = FALSE, plotting = TRUE); names(samp.spherical)
color_2 <- countcolors::sphericalRange(samp,
                                       center = c(clusters$R[2], clusters$G[2], clusters$B[2]),
                                       radius = 0.15, color.pixels = FALSE, plotting = TRUE); names(samp.spherical)
color_3 <- countcolors::sphericalRange(samp,
                                       center = c(clusters$R[3], clusters$G[3], clusters$B[3]),
                                       radius = 0.15, color.pixels = FALSE, plotting = TRUE); names(samp.spherical)

# write function to find nth most common color
for(n in 1:5) {
  c <- c()
  for (i in 1:47){
    if (!is.na(df$image_path[i])){
      print(i)
      pic <- df$image_path[i]
      kmeans.clusters <- colordistance::getKMeanColors(pic, n = 5, plotting = FALSE)
      clusters <- colordistance::extractClusters(kmeans.clusters) %>% arrange(-Pct)
      
      c[i] <- rgb(clusters$R[n],clusters$G[n],clusters$B[n])
    } else {
      c[i] <- NA
    }
  }
  df <- cbind(df, c)
}

colnames(df)[13:17] <- c("c1","c2","c3","c4","c5")

caption.length <- c()
for (i in 1:nrow(df)){
  caption.length[i] <- nchar(df$PostTitle[i])
}

df <- cbind(df, caption.length)

df <- df %>% mutate(Visit.rate = Profile.Visits / Impressions) %>% 
  mutate(like.rate = Likes / Impressions)

# basic correlation testing
nums <- unlist(lapply(df, is.numeric)) 
correlation <- df[,nums] %>% 
  filter(Impressions != 0) %>%
  cor()

# correlation of image focus
people <- df %>% 
  filter(Impressions != 0) %>%
  group_by(People) %>% 
  summarise(Average.Visit.rate = sum(Visit.rate) / n(),
            sd.visit.rate = sd(Visit.rate),
            Average.like.rate = sum(like.rate) / n(),
            sd.like.rate = sd(like.rate)
  )

product <- df %>% 
  filter(Impressions != 0) %>%
  group_by(Product) %>% 
  summarise(Average.Visit.rate = sum(Visit.rate) / n(),
            sd.visit.rate = sd(Visit.rate),
            Average.like.rate = sum(like.rate) / n(),
            sd.like.rate = sd(like.rate)
  )

insight <- df %>% 
  filter(Impressions != 0) %>%
  group_by(Quote.Insight) %>% 
  summarise(Average.Visit.rate = sum(Visit.rate) / n(),
            sd.visit.rate = sd(Visit.rate),
            Average.like.rate = sum(like.rate) / n(),
            sd.like.rate = sd(like.rate)
  )

question <- df %>% 
  filter(Impressions != 0) %>%
  group_by(Question) %>% 
  summarise(Average.Visit.rate = sum(Visit.rate) / n(),
            sd.visit.rate = sd(Visit.rate),
            Average.like.rate = sum(like.rate) / n(),
            sd.like.rate = sd(like.rate)
  )

# plots for caption length
ggplot(df[1:35,], aes(caption.length, Visit.rate)) +
  geom_point() +
  stat_smooth()

ggplot(df[1:35,], aes(caption.length, like.rate)) +
  geom_point() +
  stat_smooth()


