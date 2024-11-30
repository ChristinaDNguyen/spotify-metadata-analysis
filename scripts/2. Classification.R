#################### Classification ###################################
# Since in our first script, we already identified what the top
# ten "danceable" genres are, let's focus our classification
# on just those 10 genres to keep it manageable.

## 1. Keep only the data from those 10 genres

top_10_genres <- c("chicago-house", "latino", "reggaeton", "kids", "reggae",
                   "hip-hop", "latin", "detroit-techno", "dancehall", "minimal-techno")
spotify_kaggle_data_filtered <- spotify_kaggle_data %>%
  filter(track_genre %in% top_10_genres)

## 2. Split data into training and test sets

# Remove any rows with missing values (just in case).
spotify_kaggle_data_filtered <- spotify_kaggle_data_filtered %>%
  drop_na()

# Encode the target variable (track_genre) as a factor
spotify_kaggle_data_filtered$track_genre <- as.factor(spotify_kaggle_data_filtered$track_genre)

# Split the data into training and test sets (80% train, 20% test)
set.seed(47)  # Set a seed for reproducibility
train_index <- sample(seq_len(nrow(spotify_kaggle_data_filtered)), size = 0.8 * nrow(spotify_kaggle_data_filtered))
train_data <- spotify_kaggle_data_filtered[train_index, ]
test_data <- spotify_kaggle_data_filtered[-train_index, ]

## 3. Build classification model using Random Forest; then train the data using  training set

install.packages("randomForest")
library(randomForest)

rf_model <- randomForest(track_genre ~ danceability + energy + loudness + speechiness + acousticness + 
                           instrumentalness + liveness + valence + tempo, 
                         data = train_data, 
                         importance = TRUE, 
                         ntree = 100)

## 4. Evaluate ("run") the trained Random Forest model on test data to see if it can accurately predict the genre given those track metadatas

# Predict the genres on the test data
predictions <- predict(rf_model, test_data)

# Evaluate the accuracy of the model
confusion_matrix <- table(predictions, test_data$track_genre)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", round(accuracy * 100, 2), "%"))
# yikes, the model correctly predicted the genre of the track about 40.3% of the time in test data
# some quick research tells me that multi-class classification actually makes it hard
# to achieve super high accuracy, so 40% is about right. this might be because
# for example, "chicago-house" and "minimal-techno" could have some similarities in terms 
# of tempo and rhythm, making it harder for the model to distinguish between them.
# Plus some genres probably have overlap between the features of different metadata fields ("classes").
# I imagine for latin and latino, the danceability, energy, tempo of the tracks might be very similar, 
# so it becomes difficult for the model to separate them accurately

## 5. Accuracy of first trained RF model was low, so let's change <ntree> parameter.
## This controls how many decision trees the model builds when its training/
## Generally more trees leads to more stable predicition. Learned about
## number of trees here: https://www.youtube.com/watch?v=v6VJ2RO66Ag


# Test different numbers of trees (e.g., 100, 500, 1000) 
rf_model_100 <- randomForest(track_genre ~ danceability + energy + loudness + 
                               speechiness + acousticness + instrumentalness + 
                               liveness + valence + tempo, 
                             data = train_data, 
                             ntree = 100, 
                             importance = TRUE)

rf_model_500 <- randomForest(track_genre ~ danceability + energy + loudness + 
                               speechiness + acousticness + instrumentalness + 
                               liveness + valence + tempo, 
                             data = train_data, 
                             ntree = 500, 
                             importance = TRUE)

rf_model_1000 <- randomForest(track_genre ~ danceability + energy + loudness + 
                                speechiness + acousticness + instrumentalness + 
                                liveness + valence + tempo, 
                              data = train_data, 
                              ntree = 1000, 
                              importance = TRUE)

# "Evaluate"/run the models
print(rf_model_100) #100 trees were used, tells us 3 variables were tried at each split. OOB error rate is 61.66% so this model is about the same (ish), overall. But I do see that it's really bad especially at classifying reggaeton, latino, and latin. That makes sense since latino and latin are probably quite similar! 
print(rf_model_500) #500 trees were used, tells us 3 variables were tried at each split. OOB error rate is 60.2%, so better than the ntree = 100... chicago-house: The model correctly predicted 473 tracks as "chicago-house", but misclassified 158 as "detroit-techno", 10 as "dancehall", etc. Compared to the 100-tree model, the number of misclassifications decreased slightly.
print(rf_model_1000) #1000 trees were used, tells us 3 variables were used at each split as usual. OOB error rate is 60.34 %, so really nothing is too different (aww). Beyond 500 trees, increasing the number of trees to 1000 does not lead to significant improvements in OOB error or class error, suggesting that the model has converged or proably reached optimal point at 500 trees.

#OK, so 500 trees is the best we'll get. But further improvements may require exploring model tuning or addressing genre-specific challenges in the data.

## 6. What all this difficult-to-classify-accurately-no-matter-what-number-of-branches-we-use tells us about 
## the genres that keep getting misclassified or mixed up with each other: all of this is focused on the 500 tree model

# 6.1.: chicago-house and detroit-techno get confused for each other alot:
  
# chicago-house is often misclassified as detroit-techno (e.g., 158 times in the 500-tree model).
# detroit-techno is often misclassified as chicago-house (e.g., 211 times).
# so these two genres share many similar musical features, such as tempo, energy, and maybe even rhythmic patterns. 
# From context we know that both genres have deep roots in electronic dance music, and they may exhibit overlapping characteristics in terms of sound.

# 6.2.: dancehall and hip-hop 

# dancehall is misclassified as hip-hop 173 times
# From context we know that both dancehall and hip-hop share rhythmic characteristics, 
# especially in terms of the beat and tempo. While dancehall has its distinctive 
# reggae-influenced rhythms (e.g., the one-drop and riddim), hip-hop also tends 
# to emphasize heavy beats with a similar groove. 

# 6.3. : latino and latin

# latin is sometimes misclassified as latino (e.g., 271 times).
# and the inverse is true, latino is often misclassified as latin (e.g. 68 times)
# likely share some characteristics, such as rhythm, tempo, and musical structures 
# that stem from Latin American musical traditions. This overlap makes them difficult 
# to distinguish based on features like tempo or energy. 

# 6.4 : latino and reggae often misclassified for each other
# latino is misclassified as reggae 252 times.
# reggae is misclassified as latino 216 times.
#  While latino music (such as salsa, reggaeton, or Latin pop) and reggae 
# have distinct cultural roots, they share certain rhythmic and instrumental characteristics.
# Both genres often use syncopated rhythms that emphasize the offbeat 
# plus both genres are highly dance-oriented, and this feature might cause them to be grouped together.


## Export the confusion matrices and OOBs to put into my paper so I can refer to them

# Extract confusion matrices and OOB errors from models
conf_matrix_100 <- rf_model_100$confusion
conf_matrix_500 <- rf_model_500$confusion
conf_matrix_1000 <- rf_model_1000$confusion

oob_error_100 <- rf_model_100$err.rate
oob_error_500 <- rf_model_500$err.rate
oob_error_1000 <- rf_model_1000$err.rate

# Export confusion matrices to CSV
write.csv(conf_matrix_100, "conf_matrix_100.csv")
write.csv(conf_matrix_500, "conf_matrix_500.csv")
write.csv(conf_matrix_1000, "conf_matrix_1000.csv")

# Export OOB errors to CSV
write.csv(oob_error_100, "oob_error_100.csv")
write.csv(oob_error_500, "oob_error_500.csv")
write.csv(oob_error_1000, "oob_error_1000.csv")
