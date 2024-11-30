#################### Read in data and clean. ###################################

## 1. Read in the dataset as downloaded from Kaggle. 
## https://www.kaggle.com/datasets/maharshipandya/-spotify-tracks-dataset

library(dplyr)
spotify_kaggle_data <- read.csv("kaggledownload.csv") #note: there are 114k observations of 21 variables

## 2. Select specific metadata fields to make summary
summary_statistics <- spotify_kaggle_data %>%
  select(danceability, energy, loudness, speechiness, acousticness, instrumentalness,liveness, valence, tempo) %>%
  summarise_all(list(
    mean = mean,
    median = median,
    sd = sd,
    min = min,
    max = max
  ))
summary_statistics #tells us that danceability_mean = 0.5668001, energy_mean = 0.6413828, etc. Print this out to save in project folder

#Save the summary statistics as CSV so it is easily inserted into a Quarto paper later on.
write.csv(summary_statistics, "output1_summary_statistics.csv", row.names = FALSE)

## 3. Visualise some patterns in the data to see if anything interesting pops up

library(ggplot2)

# 3.1. Histogram for distribution of danceability
# This histogram tells us way this feature (how suitable a track is for dancing, on a scale of 0 to 1) is distributed across the dataset. 

ggplot(spotify_kaggle_data, aes(x = danceability)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(
    title = "Distribution of 'danceability'",
    x = "'danceability'",
    y = "Frequency"
  ) +
  theme_minimal()
# We see a peak at around 0.60 danceability, which means that most tracks
# are more than moderately dancable. And the graph is negatively skewed,
# so there are more tracks with high danceability. It is possible that 
# these are upbeat, danced-focused genres. Let's check the most common
# genres with higher danceabilities:

# categorize tracks by danceability levels
spotify_kaggle_data <- spotify_kaggle_data %>%
  mutate(danceability_level = case_when(
    danceability < 0.4 ~ "Low",
    danceability >= 0.4 & danceability <= 0.7 ~ "Moderate", #assign: between 0.4 and 0.7
    danceability > 0.7 ~ "High"
  ))

# Count genres for high-danceability tracks
high_danceability_genres <- spotify_kaggle_data %>%
  filter(danceability_level == "High") %>%
  count(track_genre, sort = TRUE) %>%
  head(10) # top 10 genres in terms of danceability 

high_danceability_genres # tells us that the top ten genres are chicago-house, latino, raggaeton, kids, reggae, hip-hop, latin, detroit-techno, dancehall, minimal-techno.
# findings make sense; they confirm that the most danceable genres include those commonly associated with high energy, rhythm-driven music tailored for dancing
# i.e. chicago-house, detroit-techno, and minimal-techno are all foundations of EDM and have lots of repetitive beats, strong rhyms, high temps. And they are designed for club and party
# i.e. latin, reggaeton, and latino are often syncopated beats and daceable grooves. reggaeton has lots of hip-influences too

#graph these high danceability genres to put in paper
ggplot(high_danceability_genres, aes(x = reorder(track_genre, n), y = n)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  coord_flip() +
  labs(
    title = "Top 10 genres in high-'danceability' tracks",
    x = "Genre",
    y = "Count"
  ) +
  scale_y_continuous(breaks = seq(0, max(high_danceability_genres$n), by = 50)) +
  theme_minimal()

# 3.2. Boxplot: Energy by Genre (limited to top 10 genres for clarity)

top_10_genres <- c("chicago-house", "latino", "raggaeton", "kids", "reggae","hip-hop", "latin", "detroit-techno", "dancehall", "minimal-techno")

energy_by_genre <- spotify_kaggle_data %>%
  filter(track_genre %in% top_10_genres) %>%          # filter for the top 10 genres
  group_by(track_genre) %>%                           # group by genre
  summarise(mean_energy = mean(energy, na.rm = TRUE)) # calculate mean energy

energy_by_genre # tells us chicago-house= 0.733, dancehall = 0.685, detroit-techno= 0.711, etc
#export that for usage in paper later
write.csv(energy_by_genre, "output4_energybygenretop10genres.csv", row.names = FALSE)

# Create the plot
ggplot(energy_by_genre, aes(x = reorder(track_genre, mean_energy), y = mean_energy)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  coord_flip() +
  labs(
    title = "Mean energy by genre, for the top 10 danceable genres)",
    x = "Genre",
    y = "Mean energy"
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) + #energy range is from 0 to 1
  theme_minimal()

# 3. Scatterplot: Danceability vs. Energy
# Since danceability and energy both influence how people perceive music for dancing, graphing these features might tell us how they interact. Are the most danceable tracks also the most energetic? Or do certain genres manage to be danceable even at lower energy levels?
ggplot(spotify_kaggle_data, aes(x = danceability, y = energy)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatter points with blue color and transparency
  labs(
    title = "Danceability vs Energy",
    x = "Danceability",
    y = "Energy"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 14)  # Adjust axis title size
  )
# Hm, that is too many points on a graph (114k points) so there is too
# much visual overlap to see anything useful. Instead we can sample
# 10,000 random points (about 1 in 10) to make it clearer to see.

sampled_data <- spotify_kaggle_data %>% sample_n(10000)

#scatter plot of danceability vs energy for the sampled data
ggplot(sampled_data, aes(x = danceability, y = energy)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(
    title = "Danceability vs energy (sampled data)",
    x = "Danceability",
    y = "Energy"
  ) +
  theme_minimal()

# I see in the graph that:
# ---  points are distributed across the entire range of both danceability (x-axis) and energy (y-axis), from 0 to 1.
# --- most of the data points fall somewhere in the middle of the axes, with danceability and energy showing moderate to high values.
# --- there doesn't seem a be a perfect consistent relationship between the two axes though, so
#     danceability and energy are relatively independent but show some overlap in trends. higher energy tracks (e.g., more intense, fast-paced music) are not necessarily highly danceable, and vice versa.
