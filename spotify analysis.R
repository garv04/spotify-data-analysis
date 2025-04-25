install.packages("dplyr")                                   # Install & load dplyr
library(dplyr)

install.packages("ggplot2")                                 # Install & load ggplot2
library(ggplot2)

my_path <- "C:/Users/HP/OneDrive/Desktop/most played spotify/"  # Fixed path to match your working directory

my_spotify <- read.csv(paste0(my_path,                        # Import data
                              "Most Streamed Spotify Songs 2024.csv"))

my_spotify <- my_spotify %>%                                  # Modify data
  select("Spotify.Streams", "Spotify.Playlist.Count",         # Select relevant columns
         "Spotify.Playlist.Reach", "YouTube.Views",
         "YouTube.Likes", "TikTok.Posts", "TikTok.Likes",
         "TikTok.Views", "YouTube.Playlist.Reach",
         "AirPlay.Spins", "Deezer.Playlist.Reach",
         "Pandora.Streams", "Shazam.Counts",
         "Explicit.Track") %>%
  select(sort(names(.))) %>%                                  # Sort columns
  mutate(across(where(is.character), ~na_if(.x, ""))) %>%     # Replace empty cells with NA
  mutate(across(where(is.character),                          # Modify commas & class
                ~as.numeric(gsub(",", "", .x)))) %>%
  mutate(across(-Explicit.Track, ~ifelse(. < 10, NA, .))) %>% # Replace below 10 with NA
  na.omit()                                                   # Remove rows with NA

ggplot(my_spotify,                                            # Draw density plot of Streams
       aes(x = Spotify.Streams)) +
  geom_density()

ggplot(my_spotify,                                            # Grouped densities
       aes(x = Spotify.Streams,
           col = factor(Explicit.Track))) +
  geom_density()

my_spotify %>%                                                # Improve layout of plot
  mutate(Explicit.Track = ifelse(Explicit.Track == 1,         # Change legend items
                                 "Yes", "No")) %>% 
  ggplot(aes(x = Spotify.Streams,                             # Specify plot variables
             color = Explicit.Track)) +
  geom_density(linewidth = 1) +                               # Draw density
  labs(title = "Density Plot of Spotify Streams by Explicit Track", # Plot labels
       x = "Spotify Streams",
       y = "Density",
       color = "") +
  theme_minimal(base_size = 14) +                             # Specify plot theme
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "top")

my_mod <- lm(Spotify.Streams ~ ., my_spotify)                 # Estimate linear regression
summary(my_mod)                                               # Print summary statistics

my_spotify_x <- my_spotify %>%                                # Create subset of predictors
  select(-Spotify.Streams)

pca_result <- prcomp(my_spotify_x, scale. = TRUE)             # Apply PCA
summary(pca_result)                                           # Summary of PCA results

my_spotify_pca <- data.frame(Spotify.Streams = my_spotify$Spotify.Streams, # Combine data
                             pca_result$x[ , 1:5])

my_mod_pca <- lm(Spotify.Streams ~ ., data = my_spotify_pca)  # Apply PCR
summary(my_mod_pca)                                           # Summary of PCR results