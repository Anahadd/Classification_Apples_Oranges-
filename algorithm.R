library(ggplot2)
library(class)
library(caret)

# Inputting the Data 
data <- read.csv("C:/Users/anaha/OneDrive/research/apples_and_oranges_updated.csv", header = T)

# Plotting the Data To Notice Trends (using kNN as a result)
ggplot(data, aes(x = Weight, y = Size, color = Class)) +
  geom_point() +
  labs(title = "Apples v Oranges", x = "Weight of Fruit", y = "Size of Fruit") +
  theme_minimal()

# Normalizing the Data (for non-bias)
normalized <- function(x) {
  return ((x-min(x)/max(x) - min(x)))
}

normalized_data = normalized(data[,1:2]) # Normalizes the Numerical Data not Labels

# Selecting Random Data Points
set.seed(123)
data_R <- sample(1:nrow(data),size=nrow(data)*0.7, replace = FALSE) #random selection of 70% data.

# Data Splicing 
train_data <- data[data_R, 1:2] # 70% of Data
test_data <- data[-data_R, 1:2] # remaining 30%

train_data_labels <- data[data_R, 3] # for labels
test_data_labels <- data[-data_R, 3] # for labels 

# Implementing the KNN Algorithm, sqrt(28) = 5, therefore k = 5 
knn.5 <- knn(train = train_data, test = test_data, cl = train_data_labels, k = 5)
ACC.5 <- 100 * (test_data_labels == knn.5) / NROW(test_data_labels)

# Self-Made Accuracy Check
count = 0
for (i in length(test_data_labels)){
  if (test_data_labels[i] == knn.5[i]){
    count <- count + 1
  }
}

# Outputting Our Accuracy
accuracy = count * 100
accuracy_str = as.character(accuracy)
print(paste("ACCURACY OF MODEL:", accuracy_str))

# More Detailed Accuracy
confusionMatrix(table(knn.5,test_data_labels))
