df <- read.csv("credit_score.csv")
df1 <- df[, -c(8:79)]
View(df1)
str(df1)
df1<- df1[,-c(1)]
str(df1)
variables_to_convert <- c("DEFAULT", "CAT_DEPENDENTS", 
                          "CAT_SAVINGS_ACCOUNT", "CAT_MORTGAGE",
                          "CAT_CREDIT_CARD","CAT_DEBT",
                          "CAT_GAMBLING")

df1[variables_to_convert] <- lapply(df1[variables_to_convert], as.factor)
str(df1)
summary(df1)
View(df1)

# Custom Min-Max scaling function
min_max_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#getting the names of numric columns 
col_n <- names(df1[sapply(df1,is.numeric)])


#replacing orignal columns with scaler columns 

scaled_cols <- apply(df1[, col_n], 2, min_max_scale)
df1[, col_n] <- scaled_cols

# Scatter Plot
library(psych)
pairs.panels(df1[col_n], 
             gap = 0,
             pch = 21)

#Splitting data set into training and testing 
set.seed(1234)
ind<-sample(2,nrow(df1),
            replace=TRUE,
            prob = c(0.75,0.25))
df1_train <- df1[ind==1,]
df1_test <- df1[ind==2,]

# Linear Discriminant Analysis Model Building
library(MASS)
linear <- lda(DEFAULT~., df1_train)
linear

# Prediction on Training Dataset
p <- predict(linear, df1_train)
p

#Confusion Matrix and Accuracy on Training Data
train_predict <- predict(linear, df1_train)$class
train_matrix <- table(Predicted = train_predict, Actual = df1_train$DEFAULT)
train_matrix
sum(diag(train_matrix))/sum(train_matrix)

#Confusion Matrix and Accuracy on Test Data
test_predict <- predict(linear, df1_test)$class
test_matrix <- table(Predicted = test_predict, Actual = df1_test$DEFAULT)
test_matrix
sum(diag(test_matrix))/sum(test_matrix)

#--------------------------------------------------------------

df2<- data.frame(df1)
head(df2)

library(ROSE)
df2 <- ovun.sample(DEFAULT ~ ., 
                    data=df2, method = "both", p=0.5, 
                    seed = 222, N=2000)$data
table(df2$DEFAULT)
summary(df2)

#Splitting data set into training and testing 
set.seed(1235)
ind<-sample(2,nrow(df2),
            replace=TRUE,
            prob = c(0.75,0.25))
df2_train <- df1[ind==1,]
df2_test <- df1[ind==2,]

linear <- lda(DEFAULT~., df2_train)

#Confusion Matrix and Accuracy on Training Data
train_predict <- predict(linear, df2_train)$class
train_matrix <- table(Predicted = train_predict, Actual = df2_train$DEFAULT)
train_matrix
sum(diag(train_matrix))/sum(train_matrix)

#Confusion Matrix and Accuracy on Test Data
test_predict <- predict(linear, df2_test)$class
test_matrix <- table(Predicted = test_predict, Actual = df2_test$DEFAULT)
test_matrix
sum(diag(test_matrix))/sum(test_matrix)

# even after reducing data imbalance the accurracy of model doesn't
# improve significantly so we are going to try some other methods of
# classification

# ----------------------------ANN--------------------------------
#--------------------Data Preprocessing--------------------------
library(neuralnet)
library(tidyverse)

df3<- data.frame(df)
df3 <- df3[, -c(8:79)]
df3<- df3[,-c(1)]
str(df3)

library(dplyr)

df3 <- df3 %>%
  mutate(CAT_DEPENDENTS = ifelse(CAT_DEPENDENTS == 0, "NO", "YES"))

df3 <- df3 %>%
  mutate(CAT_SAVINGS_ACCOUNT = ifelse(CAT_SAVINGS_ACCOUNT == 0, "NO", "YES"))

df3 <- df3 %>%
  mutate(CAT_MORTGAGE = ifelse(CAT_MORTGAGE == 0, "NO", "YES"))

df3 <- df3 %>%
  mutate(CAT_CREDIT_CARD = ifelse(CAT_CREDIT_CARD == 0, "NO", "YES"))

df3 <- df3 %>%
  mutate(CAT_DEBT = ifelse(CAT_DEBT == 0, "NO", "YES"))

df3 <- df3 %>%
  mutate(DEFAULT = ifelse(DEFAULT == 0, "NO", "YES"))

View(df3)

df3[variables_to_convert] <- lapply(df3[variables_to_convert], as.factor)

str(df3)

#getting the names of numric columns 
col_n_1 <- names(df3[sapply(df3,is.numeric)])
col_n_1
#replacing orignal columns with scaler columns 

scaled_cols <- apply(df3[, col_n], 2, min_max_scale)
df3[, col_n] <- scaled_cols



library(caret)

# Identify numeric variables
numeric_variables <- sapply(df3, is.numeric)

# Identify categorical variables (excluding 'DEFAULT')
categorical_variables <- sapply(df3, is.factor) & names(df3) != 'DEFAULT'

# Create dummy variables for all categorical variables (excluding 'DEFAULT')
dummy_data <- dummyVars("~ .", data = df3[, categorical_variables])
dummy_data <- data.frame(predict(dummy_data, newdata = df3))

# Combine numeric variables, dummy variables, and 'Credit_Score'
result_data <- cbind(df3[, numeric_variables, drop = FALSE], dummy_data, df3$DEFAULT)

# renaming the 'Credit$Credit_Score' to 'Credit_Score'
colnames(result_data)[colnames(result_data) == "df3$DEFAULT"] <- 'DEFAULT'

result_data <- ovun.sample(DEFAULT ~ ., 
                   data=result_data, method = "both", p=0.5, 
                   seed = 242, N=2000)$data

str(result_data)
View(result_data)

# Splitting the dataset into training & testing
set.seed(222)
inp <- sample(2, nrow(result_data), replace = TRUE, prob = c(0.75, 0.25))
df3_train <- result_data[inp==1, ]
df3_test<- result_data[inp==2, ]
#printing the no of records in each category for both dataset
table(df3_train$DEFAULT)
table(df3_test$DEFAULT)
str(df3_train)
str(df3_test)


set.seed(333)

ann_model_1 <- neuralnet(DEFAULT ~ .,
                       data = df3_train,
                       hidden = 5,
                       err.fct = "sse",
                       linear.output = FALSE,
                       lifesign = 'full',
                       rep = 1,
                       act.fct = "logistic",
                       algorithm = "rprop+",
                       stepmax = 100000000,
                       threshold=0.04)

# plot neural network 
plot(ann_model_1, rep=1)

# error
ann_model_1$result.matrix
View(df3_train)

# First attempt to Predict on the training data

training_predict_probs <- predict(ann_model_1,df3_train)
head(training_predict_probs)
training_predict_class <- levels(df3_train$DEFAULT)[max.col(training_predict_probs, "first")]
head(training_predict_class)
view(df3_train)

######## Fixing the level mismatch issue ######
# Identifying the issue
#checking for issue
length(training_predict_class)
length(df3_train$DEFAULT)
class(training_predict_class)
class(df3_train$DEFAULT)
levels(df3_train$DEFAULT)
training_predict_class <- levels(df3_train$DEFAULT)[max.col(training_predict_probs, "first")]
head(training_predict_class)
identical(levels(df3_train$DEFAULT), colnames(training_predict_probs))

# Print information to understand the issue
print(levels(df3_train$DEFAULT))
print(colnames(training_predict_probs)) #issue
# Print the structure of ann_model$net.result & target
print(str(ann_model_1$net.result))
print(levels(df3_train$DEFAULT))

# Fixing the issue

# Extract the matrix from ann_model$net.result
predicted_probs_matrix <- ann_model_1$net.result[[1]]
# Create appropriate column names to match
colnames(predicted_probs_matrix) <- levels(df3_train$DEFAULT)
# Print the structure of predicted_probs_matrix
print(str(predicted_probs_matrix))
# Fixing the mismatch issue--Reorder columns of predicted_probs_matrix
column_order <- match(levels(df3_train$DEFAULT), colnames(predicted_probs_matrix))
################ end of Fixing the level mismatch issue ######

# Predicting probabilities on the training data
predicted_probs_reordered <- predicted_probs_matrix[, column_order]
predicted_probs_reordered
# Extract predicted class labels
training_predict_class <- levels(df3_train$DEFAULT)[max.col(predicted_probs_reordered, "first")]
# Check the head of training_predict_class
head(training_predict_class)

# Evaluate accuracy

accuracy <- sum(training_predict_class == df3_train$DEFAULT) / length(df3_train$DEFAULT)
print(paste("Accuracy on Training Data:", round(accuracy * 100, 2), "%"))
# Draw a confusion matrix
conf_matrix <- table(Actual = df3_train$DEFAULT, Predicted = training_predict_class)
print(conf_matrix)

# First attempt to Predict on the testing data

testing_predict_probs <- predict(ann_model_1,df3_test,type = "response")
head(testing_predict_probs)
testing_predict_class <- levels(df3_test$DEFAULT)[max.col(testing_predict_probs, "first")]
head(testing_predict_class)


######## Fixing the level mismatch issue ######
# Identifying the issue
#checking for issue
length(testing_predict_class)
length(df3_test$DEFAULT)
class(testing_predict_class)
class(df3_test$DEFAULT)
levels(df3_test$DEFAULT)
identical(levels(df3_test$DEFAULT), colnames(testing_predict_probs))

# Print level & structure of ann_model$net.result and target to understand the issue
print(levels(df3_test$DEFAULT))
print(colnames(testing_predict_probs)) #issue
print(str(ann_model_1$net.result))
print(levels(df3_test$DEFAULT))

# Fixing the issue

# Extract the matrix from ann_model$net.result
predicted_probs_test <- predict(ann_model_1, newdata = df3_test, type = "response")
# Ensure that the levels match
levels(predicted_probs_test) <- levels(df3_test$DEFAULT)
# Convert the predicted probabilities to a matrix
predicted_probs_matrix_test <- as.matrix(predicted_probs_test)
# Print the matrix
print(predicted_probs_matrix_test)


# Create appropriate column names to match
colnames(predicted_probs_matrix_test) <- levels(df3_test$DEFAULT)
# Print the structure of predicted_probs_matrix_test
print(str(predicted_probs_matrix_test))
# Fixing the mismatch issue--Reorder columns of predicted_probs_matrix_test
column_order_test <- match(levels(df3_test$DEFAULT), colnames(predicted_probs_matrix_test))

################ end of Fixing the level mismatch issue ######




# Predicting probabilities on the testing data
predicted_probs_reordered_test <- predicted_probs_matrix_test[, column_order_test]
predicted_probs_reordered_test
# Extract predicted class labels
testing_predict_class <- levels(df3_test$DEFAULT)[max.col(predicted_probs_reordered_test, "first")]
# Check the head of testing_predict_class
head(testing_predict_class)


# Evaluate accuracy
accuracy <- sum(testing_predict_class == df3_test$DEFAULT) / length(df3_test$DEFAULT)
print(paste("Accuracy on testing Data:", round(accuracy * 100, 2), "%"))
# Draw a confusion matrix
conf_matrix <- table(Actual = df3_test$DEFAULT, Predicted = testing_predict_class)
print(conf_matrix)





