# Set the working directory to the folder containing the dataset
setwd("C:/Users/USER/Desktop/DWDM/Mid/Project")

# Load the required packages
library(class)
library(ggplot2)

# Read the dataset
wine_data <- read.csv("winequality-white.csv", sep = ";")

# Remove any missing values
wine_data <- na.omit(wine_data)

# Set the predictor variables and the target variable
predictor_cols <- names(wine_data)[-ncol(wine_data)]
target_col <- names(wine_data)[ncol(wine_data)]

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(wine_data), round(0.7 * nrow(wine_data)))
train_data <- wine_data[train_indices, predictor_cols]
train_labels <- wine_data[train_indices, target_col]
test_data <- wine_data[-train_indices, predictor_cols]
test_labels <- wine_data[-train_indices, target_col]

# Function to perform k-NN with a specified distance measure
knn_with_distance_measure <- function(train_data, test_data, train_labels, k, distance_measure) {
  predicted_labels <- knn(train = train_data, test = test_data, cl = train_labels, k = k, prob = TRUE, use.all = TRUE)
  return(predicted_labels)
}

# Set the values of k
k_values <- c(3, 5, 7)

# Initialize vectors to store accuracies
accuracies <- vector()

# Apply k-NN for each k value and distance measure
for (k in k_values) {
  # Apply k-NN with Euclidean distance
  euclidean_predictions <- knn_with_distance_measure(train_data, test_data, train_labels, k, "euclidean")
  
  # Apply k-NN with Manhattan distance
  manhattan_predictions <- knn_with_distance_measure(train_data, test_data, train_labels, k, "manhattan")
  
  # Apply k-NN with Maximum distance
  maximum_predictions <- knn_with_distance_measure(train_data, test_data, train_labels, k, "maximum")
  
  # Evaluate the accuracy of the predictions
  accuracy_euclidean <- sum(euclidean_predictions == test_labels) / length(test_labels)
  accuracy_manhattan <- sum(manhattan_predictions == test_labels) / length(test_labels)
  accuracy_maximum <- sum(maximum_predictions == test_labels) / length(test_labels)
  
  # Store the accuracy
  accuracies <- c(accuracies, accuracy_euclidean, accuracy_manhattan, accuracy_maximum)
  
  # Print the accuracy for the current k value
  cat("Accuracy for k =", k, "\n")
  cat("Euclidean Distance:", accuracy_euclidean, "\n")
  cat("Manhattan Distance:", accuracy_manhattan, "\n")
  cat("Maximum Distance:", accuracy_maximum, "\n")
  cat("\n")
}

# Create a data frame for accuracies
accuracy_df <- data.frame(Distance = rep(c("Euclidean", "Manhattan", "Maximum"), length(k_values)),
                          K = rep(k_values, each = 3),
                          Accuracy = accuracies)

# Plot the accuracies
ggplot(accuracy_df, aes(x = K, y = Accuracy, color = Distance, group = Distance)) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy of k-NN with Different Distance Measures",
       x = "k",
       y = "Accuracy",
       color = "Distance Measure") +
  theme_minimal()
