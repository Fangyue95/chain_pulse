
########### Codes for extracting YouTube analytic information ##########
##Install packages##
install.packages("httr")
library(httr)

citation("httr")
install.packages("httr2")
library(httr2) #Communicate with API and get the raw data from YouTube in JSON format
citation("httr2")
install.packages("jsonlite")
library(jsonlite) # Convert JSON data into readable format
install.packages("dplyr") # For manipulating data
library(dplyr)
citation("jsonlite")
install.packages("here")
library(here)
citation("here")
install.packages("dplyr")
library(dplyr)

install.packages("vosonSML")
library(vosonSML)
citation("vosonSML")


###API call
## {Reousce}: information we want to extract from Yt
# cHANNELS
# PlaylistItems
# Videos

## {Parameters}: further customize the results
# Key(required)
# id, forUsername, playlistId
# Part for the specific data points

key <- "API Key" #API key sought from Google cloud
channel_id <- "CHANNEL ID" #channel ID sought from YouTube url#
base <- "https://www.googleapis.com/youtube/v3/"

required_packages <- c("httr", "jsonlite", "here", "dplyr")
for(i in required_packages) {
  if(!require(i, character.only = T)) {
    #  if package is not existing, install then load the package
    install.packages(i, dependencies = T, repos = "http://cran.us.r-project.org")
    # install.packages(i, dependencies = T, repos = "https://cran.stat.upd.edu.ph/")
    require(i, character.only = T)
  }
}

# Construct the API call
api_params <- 
  paste(paste0("key=", key), 
        paste0("id=", channel_id), 
        "part=snippet,contentDetails,statistics",
        sep = "&")
api_call <- paste0(base, "channels", "?", api_params)
api_result <- GET(api_call)
json_result <- content(api_result, "text", encoding="UTF-8")

# Process the raw data into a data frame
channel.json <- fromJSON(json_result, flatten = T)

channel.df <- as.data.frame(channel.json)
channel.df

playlist_id <- "PLAYLIST_ID" #PLAYLIST_ID from playlist URL

# temporary variables
nextPageToken <- ""
upload.df <- NULL
pageInfo <- NULL

# Loop through the playlist while there is still a next page
while (!is.null(nextPageToken)) {
  # Construct the API call
  api_params <- 
    paste(paste0("key=", key), 
          paste0("playlistId=", playlist_id), 
          "part=snippet,contentDetails,status",
          "maxResults=300",
          sep = "&")
  
  # Add the page token for page 2 onwards
  if (nextPageToken != "") {
    api_params <- paste0(api_params,
                         "&pageToken=",nextPageToken)
  }
  
  api_call <- paste0(base, "playlistItems", "?", api_params)
  api_result <- GET(api_call)
  json_result <- content(api_result, "text", encoding="UTF-8")
  upload.json <- fromJSON(json_result, flatten = T)
  
  nextPageToken <- upload.json$nextPageToken
  pageInfo <- upload.json$pageInfo
  
  curr.df <- as.data.frame(upload.json$items)
  if (is.null(upload.df)) {
    upload.df <- curr.df
  } else {
    upload.df <- bind_rows(upload.df, curr.df)
  }
}

video.df<- NULL
# Loop through all uploaded videos
for (i in 1:nrow(upload.df)) {
  # Construct the API call
  video_id <- upload.df$contentDetails.videoId[i]
  api_params <- 
    paste(paste0("key=", key), 
          paste0("id=", video_id), 
          "part=id,statistics,contentDetails",
          sep = "&")
  
  api_call <- paste0(base, "videos", "?", api_params)
  api_result <- GET(api_call)
  json_result <- content(api_result, "text", encoding="UTF-8")
  video.json <- fromJSON(json_result, flatten = T)
  
  curr.df <- as.data.frame(video.json$items)
  
  if (is.null(video.df)) {
    video.df <- curr.df
  } else {
    video.df <- bind_rows(video.df, curr.df)
  }
}  

# Combine all video data frames
video.df$contentDetails.videoId <- video.df$id


video_final.df2 <- merge(x = upload.df, 
                         y = video.df,
                         by = "contentDetails.videoId")

video_final.df2 <- data.frame(video_final.df2)

video_final.df2

video_final.df2 <- apply(video_final.df2,2,as.character)

video_final.df2


write.csv(video_final.df2, "FILE_NAME.csv",row.names=FALSE) # FILE_NAME = name of the file


###########Analysis of paired differences ######
##Install packages#
install.packages("tidyr")
install.packages("broom")
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
library(broom)

# Read in the data
pulse <- read_excel("unga_videos.xlsx") #UNGA_VIDEOS: analytics of "Pulse" videos
sporadic <- read_excel("paired_by_topic.xlsx") #PAIRED_BY_TOPIC: analytics of "Sporadic" videos paired by topics
sporadic2 <- read_excel("Paired_by_recency.xlsx") #PAIRED_BY_RECENCY: analytics of "sporadic" videos paired by recency


pulse$likeCount <- as.numeric(pulse$likeCount) # changing variables to numeric
sporadic$likeCount <- as.numeric(sporadic$likeCount)
# Rename with suffixes # label pulse or sporadic
pulse <- pulse %>% rename_with(~paste0(., "_pulse"), -id) 
sporadic <- sporadic %>% rename_with(~paste0(., "_sporadic"), -id)

# Merge by video ID based on the same creator
df <- inner_join(pulse, sporadic, by = "id")

# Function to test normality + run appropriate test
compare_metric <- function(var) {
  # Extract paired vectors
  x <- df[[paste0(var, "_pulse")]]
  y <- df[[paste0(var, "_sporadic")]]
  
  # Remove NAs pairwise
  paired_data <- na.omit(data.frame(x, y))
  x <- paired_data$x
  y <- paired_data$y
  
  # Differences
  diffs <- x - y
  
  # Shapiro-Wilk test for normality of differences
  shapiro <- shapiro.test(diffs)
  
  if (shapiro$p.value > 0.05) {
    # If normal, use paired t-test
    test <- t.test(x, y, paired = TRUE)
    method <- "Paired t-test"
    ci <- test$conf.int
    est <- mean(diffs)
    pval <- test$p.value
  } else {
    # If not normal, use paired Wilcoxon test
    # Use exact = FALSE to allow conf.int
    test <- wilcox.test(x, y, paired = TRUE, conf.int = TRUE, conf.level = 0.95, exact = FALSE)
    method <- "Wilcoxon signed-rank test"
    ci <- test$conf.int
    est <- mean.default(diffs)
    pval <- test$p.value
  }
  
  tibble(
    metric = var,
    method = method,
    estimate = est,
    ci_lower = ci[1],
    ci_upper = ci[2],
    p_value = pval,
    shapiro_p = shapiro$p.value
  )
}
# Apply to each metric 
results <- bind_rows( compare_metric("viewCount"), compare_metric("likeCount"), compare_metric("commentCount") ) 

print(results)

##The above was repeated with sporadic2 for sensitivity analysis

###Plotting the paired graphs

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)


# Prepare long-format data for view count
plot_data_view <- df %>%
  select(id, viewCount_pulse, viewCount_sporadic) %>%
  pivot_longer(
    cols = c(viewCount_pulse, viewCount_sporadic),
    names_to = "group",
    values_to = "value"
  ) %>%
  mutate(group = ifelse(grepl("pulse", group), "Pulse", "Sporadic"),
         value_adj = value + 1)  # for log scale

# Extract results from computed table
res_view <- results %>% filter(metric == "viewCount")

estimate <- round(res_view$estimate, 1)
ci_lower <- round(res_view$ci_lower, 1)
ci_upper <- round(res_view$ci_upper, 1)
p_val <- signif(res_view$p_value, 3)
estimate

# Plot view 
view_plot <- ggplot(plot_data_view, aes(x = group, y = value_adj, fill = group)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2, alpha = 0.7)  +
  geom_point(aes(color = group, y = value_adj), position = position_jitter(width = 0.1), size = 2, alpha = 0.7) +
  scale_y_log10(breaks = c(10, 100, 1000, 10000), limits = c(10, NA)) +
  stat_compare_means(
    method = "wilcox.test",
    paired = TRUE,
    label = "p.format",
    label.y = max(plot_data_view$value_adj) * 1.05
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("Pulse" = "#1f77b4", "Sporadic" = "#ff7f0e")) +
  scale_color_manual(values = c("Pulse" = "#1f77b4", "Sporadic" = "#ff7f0e")) +
  labs(
    x = NULL,
    y = "View Count"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

view_plot


# Prepare long-format data for like count
plot_data_like <- df %>%
  select(id, likeCount_pulse, likeCount_sporadic) %>%
  pivot_longer(
    cols = c(likeCount_pulse, likeCount_sporadic),
    names_to = "group",
    values_to = "value"
  ) %>%
  mutate(group = ifelse(grepl("pulse", group), "Pulse", "Sporadic"),
         value_adj = value + 1)  # for log scale

# Extract results from your computed table
res_like <- results %>% filter(metric == "likeCount")

estimate <- round(res_like$estimate, 1)
ci_lower <- round(res_like$ci_lower, 1)
ci_upper <- round(res_like$ci_upper, 1)
p_val <- signif(res_like$p_value, 2)

# Plot like count
plot_like <- ggplot(plot_data_like, aes(x = group, y = value, fill = group)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2, alpha = 0.7)  +
  geom_point(aes(color = group, y = value), position = position_jitter(width = 0.1), size = 2, alpha = 0.7)+
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("Pulse" = "#1f77b4", "Sporadic" = "#ff7f0e")) +
  scale_color_manual(values = c("Pulse" = "#1f77b4", "Sporadic" = "#ff7f0e")) +
  labs(
    x = NULL,
    y = "Like Count"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

plot_like


# Prepare long-format data for comment count
plot_data_comment <- df %>%
  select(id, commentCount_pulse, commentCount_sporadic) %>%
  pivot_longer(
    cols = c(commentCount_pulse, commentCount_sporadic),
    names_to = "group",
    values_to = "value"
  ) %>%
  mutate(group = ifelse(grepl("pulse", group), "Pulse", "Sporadic"),
         value_adj = value + 1)  # for log scale

# Extract results from your computed table
res_comment <- results %>% filter(metric == "commentCount")

estimate <- round(res_comment$estimate, 1)
ci_lower <- round(res_comment$ci_lower, 1)
ci_upper <- round(res_comment$ci_upper, 1)
p_val <- signif(res_comment$p_value, 2)

# Plot comment 
comment_plot <- ggplot(plot_data_comment, aes(x = group, y = value, fill = group)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2, alpha = 0.7)  +
  geom_point(aes(color = group), position = position_jitter(width = 0.1), size = 2, alpha = 0.7) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("Pulse" = "#1f77b4", "Sporadic" = "#ff7f0e")) +
  scale_color_manual(values = c("Pulse" = "#1f77b4", "Sporadic" = "#ff7f0e")) +
  labs(
    title = "",
    x = NULL,
    y = "Comment Count"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

comment_plot

##################
####### Video engagement in the context of creator's baseline engagement #######
#Install packages#

install.packages("readxl")
library(readxl)
library(tidyverse)

##### Installing all the files for each creator ###
###Label each video as normal or coordinated ###


unga <- pulse
# file containing the videos released during "Pulse"

#Loading individual creator's files
c1 <- read_csv("1.csv") 
c1

c2 <- read_csv("2.csv")
c2

c2 <- c2 %>%
  rename(l_s = ...8)

c2 <- c2 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))
glimpse(c2)

c3 <- read_csv("3.csv")
c3

c3 <- c3 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))
glimpse(c3)


c4 <- read_csv("4.csv")
c4
c4 <- c4 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))


c5 <- read_csv("5.csv")
c5

c5 <- c5 %>%
  rename(l_s = ...8)
c5 <- c5 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))


c6 <- read_csv("6.csv")
c6
c6 <- c6 %>%
  rename(l_s = `L/S`)

c6 <- c6 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))

c7 <- read_csv("7.csv")
c7
c7 <- c7 %>%
  rename(l_s = ...8)
c7 <- c7 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))
c7

c8 <- read_csv("8.csv")
c8
c8 <- c8 %>%
  rename(l_s = ...8)
c8 <- c8 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))
c8

c9 <- read_csv("9.csv")
c9
c9 <- c9 %>%
  rename(l_s = s_l)

c9 <- c9 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))
c9

c10 <- read_csv("10.csv")
c10
c10 <- c10 %>%
  rename(l_s = ...8)

c10 <- c10 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))
c10

c11 <- read_csv("11.csv")

c11
c11 <- c11 %>%
  rename(l_s = ...8)
c11 <- c11 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))
c11

c12 <- read_csv("12.csv")
c12
c12 <- c12 %>%
  rename(l_s = ...8)
c12 <- c12 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))
c12

c13 <- read_csv("13.csv")
c13 <- c13 %>%
  rename(l_s = ...8)
c13 <- c13 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))

c14 <- read_csv("14.csv")
c14
c14 <- c14 %>%
  rename(l_s = ...8)
c14 <- c14 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))


c15 <- read_csv("15.csv")
c15 <- c15 %>%
  rename(l_s = s_l)
c15 <- c15 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))


c16 <- read_csv("16.csv")
c16

c16 <- c16 %>%
  rename(l_s = s_l)
c16 <- c16 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))

c17 <- read_csv("17.csv")
c17


c17 <- c17 %>%
  rename(l_s = s_l)
c17 <- c17 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))

c17

c18 <- read_csv("18.csv")
c18


c18 <- c18 %>%
  rename(l_s = s_l)
c18 <- c18 %>%
  mutate(type = if_else(videoId  %in% unga$contentDetails.videoId, "coordinated", "normal"))

c18

#####Statistical comparisons ####

library(readxl)
library(dplyr)
library(ggplot2)
library(purrr)

install.packages("writexl")
library(writexl)

#combining all files into a list
creator_dfs <- list(
  creator1 = c1,
  creator2 = c2,
  creator3 = c3,
  creator4 = c4,
  creator5 = c5,
  creator6 = c6,
  creator7 = c7,
  creator8 = c8,
  creator9 = c9,
  creator10 = c10,
  creator11 = c11,
  creator12 = c12,
  creator13 = c13,
  creator14 = c14,
  creator15 = c15,
  creator16 = c16,
  creator17 = c17,
  creator18 = c18
)

creator_dfs

dir.create("creator_excels",showWarnings = FALSE)

# Loop through and save each data frame
for (name in names(creator_dfs)) {
  write_xlsx(creator_dfs[[name]], path = file.path("creator_excels", paste0(name, ".xlsx")))
}


### Get list of excel files###
files <- list.files("creator_excels/", pattern = "\\.xlsx$", full.names = TRUE)
files

# Function to process a single creator file
process_creator <- function(file_path) {
  df <- read_excel(file_path) %>%
    mutate(
      type = tolower(type),
      creator = tools::file_path_sans_ext(basename(file_path))
    )
  
  # One coordinated video
  coord <- df %>% filter(type == "coordinated")
  
  # Average baseline from normal videos
  baseline <- df %>% filter(type == "normal") %>%
    summarise(
      avg_views = mean(views, na.rm = TRUE),
      sd_views = sd(views,na.rm = TRUE),
      avg_likes = mean(likes, na.rm = TRUE),
      sd_likes = sd(likes,na.rm = TRUE),
      avg_comments = mean(comments, na.rm = TRUE),
      sd_comments = sd(comments,na.rm=TRUE)
    )
  
  # Combine coordinated video with baseline averages
  result <- coord %>%
    select(creator, views, likes, comments) %>%
    bind_cols(baseline) %>%
    mutate(
      "View" = (views-avg_views) / sd_views,
      "Like" = (likes-avg_likes)/ sd_likes,
      "Comment" = (comments-avg_comments) / sd_comments
    )
  
  return(result)
}

# Process all 18 creator files
comparison_data <- map_dfr(files, process_creator)
comparison_data


glimpse(comparison_data)

plot_data <- comparison_data %>%
  select(creator, "View", "Like", "Comment") %>%
  pivot_longer(cols = -creator, names_to = "metric", values_to = "standardised difference")
plot_data

plot_data$metric <- factor(plot_data$metric, levels = c("View", "Like", "Comment"))

plot_data
library(dplyr)

###Calculation of standardised mean difference
summary_results <- plot_data %>%
  group_by(metric) %>%
  summarise(
    estimate = mean(`standardised difference`, na.rm = TRUE),
    sd_D = sd(`standardised difference`, na.rm = TRUE),
    se = sd_D/sqrt(18),
    ci_lower = estimate - 1.96 * se,
    ci_upper = estimate + 1.96* se)

summary_results

install.packages("ggbreak")
library(ggbreak)

# Plotting of the graph labelled with summary statistics
creatorplot_1 <- ggplot(plot_data, aes(x = metric, y = `standardised difference`)) +
  geom_boxplot(fill = "#1f77b4", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6, color = "#1f78b4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_y_break(c(1, 7)) + # 👈 hides the empty middle region
  scale_y_continuous(
    breaks = c(-1, -0.5, 0, 0.5, 1, 7, 7.5),
    labels = c("-1","-0.5","0","0.5","1","7","7.5"))+
  theme_minimal(base_size = 18) +
  theme(
    axis.line.y.right = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank()
  ) +
  labs(x = "", y = "Mean standardised difference")

creatorplot_1


########Compare with only videos of the same format #####
#combining all files that contain videos of different formats (Excluding creator 1, 15 and 17) into a list
creator_dfs_2 <- list(
  creator2 = c2,
  creator3 = c3,
  creator4 = c4,
  creator5 = c5,
  creator6 = c6,
  creator7 = c7,
  creator8 = c8,
  creator9 = c9,
  creator10 = c10,
  creator11 = c11,
  creator12 = c12,
  creator13 = c13,
  creator14 = c14,
  creator16 = c16,
  creator18 = c18
)

creator_dfs_2

#creation of a directory
dir.create("creator_excels_2",showWarnings = FALSE)

# Loop through and save each data frame
for (name in names(creator_dfs_2)) {
  write_xlsx(creator_dfs[[name]], path = file.path("creator_excels_2", paste0(name, ".xlsx")))
}


### Get list of excel files###
files_2 <- list.files("creator_excels_2/", pattern = "\\.xlsx$", full.names = TRUE)
files_2


files_2
creator_dfs

process_creator2 <- function(file_path) {
  df <- readxl::read_excel(file_path) %>%
    mutate(
      type = tolower(type),  # ensure consistent format casing
      creator = tools::file_path_sans_ext(basename(file_path))
    )
  
  # Only one coordinated video
  coord <- df %>% filter(type == "coordinated") %>% slice(1)
  
  coord_format <- coord$l_s

  
  # Filter normal videos with the same format
  baseline <- df %>%
    filter(type == "normal", l_s == coord_format)
  
  baseline <- baseline %>%
    summarise(
      avg_views    = mean(views, na.rm = TRUE),
      sd_views     = sd(views, na.rm = TRUE),
      avg_likes    = mean(likes, na.rm = TRUE),
      sd_likes     = sd(likes, na.rm = TRUE),
      avg_comments = mean(comments, na.rm = TRUE),
      sd_comments  = sd(comments, na.rm = TRUE)
    )
  
  
  # Combine coordinated with matching-format baseline
  result <- coord %>%
    select(creator, l_s, views, likes, comments) %>%
    bind_cols(baseline) %>%
    mutate(
      "View" = (views-avg_views) / sd_views,
      "Like" = (likes-avg_likes)/ sd_likes,
      "Comment" = (comments-avg_comments) / sd_comments
    )
  
  return(result)
}



# Process all 18 creator files
comparison_data2 <- map_dfr(files_2, process_creator2)
comparison_data2
glimpse(comparison_data2)


# Prepare data for plotting

plot_data2 <- comparison_data2 %>%
  select(creator, "View", "Like", "Comment") %>%
  pivot_longer(cols = -creator, names_to = "metric", values_to = "standardised difference")

plot_data2
names(plot_data2)

plot_data2$metric <- factor(plot_data2$metric, levels = c("View", "Like", "Comment"))

library(dplyr)
# Wilcoxon test comparing "Pulse" videos with the ones of the same format
#with estimates, confidence intervals, and P values

summary_results2 <- plot_data2 %>%
  group_by(metric) %>%
  summarise(
    estimate = mean(`standardised difference`, na.rm = TRUE),
    sd_D = sd(`standardised difference`, na.rm = TRUE),
    se = sd_D/sqrt(18),
    ci_lower = estimate - 1.96 * se,
    ci_upper = estimate + 1.96* se)

summary_results2

write_xlsx(summary_results2, "standardised meand difference_CI_2.xlsx")

creator_plot2 <- ggplot(plot_data2, aes(x = metric, y = `standardised difference`)) +
  geom_boxplot(fill = "#1f77b4", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6, color = "#1f78b4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_y_break(c(1.5, 7)) + # 👈 hides the empty middle region
  scale_y_continuous(
    breaks = c(-1, -0.5, 0, 0.5, 1, 7, 8),
    labels = c("-1","-0.5","0","0.5","1","7","8"))+
  theme_minimal(base_size = 18) +
  theme(
    axis.line.y.right = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank()
  ) +
  labs(x = "", y = "")

creator_plot2

combined_plot <- (creatorplot_1|creator_plot2) + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A')

combined_plot
final_plot <- plot_grid(
  combined_plot,
  ggdraw() + draw_label("", x = 0.5, y = 1, hjust = 0.5, size = 18),
  ncol = 1,
  rel_heights = c(1, 0.05) # space for label
)

final_plot



write_xlsx(summary_results, "wilcoxon_results_CI.xlsx")
library(ggplot2)
library(dplyr)
library(scales)

# Remove zero or negative ratios for log scale
plot_data2_filtered <- plot_data2 %>% filter(ratio > 0)

summary_results2 <- summary_results %>%
  mutate(
    ypos = 10^2 * 0.98,  # place labels just below the y-axis max
    label = paste0(
      "Median = ", round(estimate, 2),
      "\n95% CI [", round(conf_low, 2), ", ", round(conf_high, 2), "]",
      "\nP = ", signif(p_value, 3)
    )
  )

# Plot graph
ggplot(plot_data2_filtered, aes(x = metric, y = ratio)) +
  geom_boxplot(fill = "#1f77b4", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6, color = "#1f78b4") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  scale_y_continuous(
    trans = "log10",
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(min(plot_data2_filtered$ratio, na.rm = TRUE) * 0.9, 10^2)
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "",
    x = "Engagement Metrics",
    y = "Engagement Ratio"
  ) +
  geom_text(
    data = summary_results2,
    aes(x = metric, y = ypos, label = label),
    inherit.aes = FALSE,
    size = 4
  )










