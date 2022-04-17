# load data:
train <- read.csv("./titanic/train.csv")
test <- read.csv("./titanic/test.csv")
submission <- read.csv("./titanic/gender_submission.csv")

# storing dependent variable (Survived):
y_train <- train$Survived

# removing y from training data: 
train <- train[, -c(which(colnames(train) == "Survived"))]

# train and test set rows:
train$istrain <- 1
test$istrain <- 0

# concatenating test and train datasets:
all_data <- rbind(train, test)

# removing passenger ID:
all_data <- all_data[, -c(which(colnames(all_data) == "PassengerId"))]

# classes for each column:
lapply(all_data, class)

# changing the classes for factors and numeric:
all_data$Pclass <- as.character(all_data$Pclass)
char_vars <- sapply(all_data, is.character)
char_vars <- all_data[, char_vars]
factor_vars <- as.data.frame(sapply(char_vars, as.factor), stringsAsFactors = TRUE)

num_vars <- sapply(all_data, is.numeric)
num_vars <- all_data[, num_vars]
num_vars <- as.data.frame(sapply(num_vars, as.numeric))

# join variables
all_data_casted <- cbind(factor_vars, num_vars)

# inputing NA:
table(is.na(all_data_casted$Fare))
fare_median <- median(all_data_casted$Fare, na.rm = TRUE)
all_data_casted[is.na(all_data_casted$Fare), "Fare"] <- fare_median

# Predicting Age ----------------------------------------------------------


# filtering outliers:
boxplot(all_data_casted$Age)
upper_whisker <- boxplot.stats(all_data_casted$Age)$stats[5]
lower_whisker <- boxplot.stats(all_data_casted$Age)$stats[1]

outlier_filter_upper <- all_data_casted$Age < upper_whisker 
outlier_filter_lower <- all_data_casted$Age > lower_whisker 
outlier_filter <- all_data_casted[outlier_filter_lower & outlier_filter_upper,]

# fit the model
age_model <- lm(Age ~ Pclass + Sex + SibSp + Parch + Embarked, data = outlier_filter);summary(age_model)
age_model <- lm(Age ~ Pclass + Sex + SibSp, data = outlier_filter);summary(age_model)

# plot to check linear model assumptions:
plot(age_model)

# predicting NAs:
age_rows <- predict(age_model, 
                    newdata = all_data_casted[is.na(all_data_casted$Age),
                                              c("Pclass", "Sex", "SibSp")])

# replacing NAs:
all_data_casted[is.na(all_data_casted$Age), "Age"] <- age_rows

# checking correlation:
cor(all_data_casted[, c("Age", "Fare", "SibSp", "Parch")])


# K-means -----------------------------------------------------------------


# variables: Sex + Age + SibSp + Fare + Embarked + Pclass
k_data <- all_data_casted[,c(which(colnames(all_data_casted) %in% c("Sex", "Age", "SibSp", "Fare", "Parch", "Pclass")))]
names(k_data)

# converting factors to dummies
sex_dummy <- model.matrix( ~ Sex - 1, k_data)
pclass_dummy <- model.matrix( ~ Pclass - 1, k_data)

# adding to data
k_data <- data.frame(k_data[, !colnames(k_data) %in% c("Sex","Pclass")], sex_dummy, pclass_dummy)

# Normalizing variables for k-means clustering
k_data_normalized <- scale(k_data)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(k_data_normalized, k)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k <- 1:15

# extract wss for 1-15 clusters
wss_values <- Reduce("c", purrr::map(k, wss))

plot(k, wss_values, type = "b", pch = 20, 
     xlab = "Number of clusters k",
     ylab = "Total within-clusters sum of squares")

# the best k was 2:
k_data_cluster <- kmeans(k_data_normalized, centers = 2) 

# adding new feature to original data: 
all_data_casted$Cluster <- as.factor(k_data_cluster$cluster)


#  train, test, validation ------------------------------------------------

# train and test:
nrow(all_data_casted[all_data_casted$istrain == 1,])
nrow(all_data_casted[all_data_casted$istrain == 0,])

train <- all_data_casted[all_data_casted$istrain == 1,]
test <- all_data_casted[all_data_casted$istrain == 0,]

train$Survived <- as.factor(y_train)

# splitting train and validation 80%:
p <- 0.8
train_split <- sample.int(nrow(train), p*nrow(train))
train_data <- train[train_split,]
val_data <- train[-train_split,]


# knn --------------------------------------------------------------------

test_data <- as.data.frame(sapply(test, as.numeric))
train_data <- as.data.frame(sapply(train_data, as.numeric))
val_data <- as.data.frame(sapply(val_data, as.numeric))

col_pos <- c(which(colnames(train_data) %in% c("Sex", "Age", "SibSp", "Pclass", "Cluster")))

knn_pred <- class::knn(train = train_data[, col_pos], 
                       test = val_data[, col_pos], 
                       cl = train_data$Survived, 
                       k = 1)

y_valid <- ifelse(knn_pred == 2, 1, 0)

# confusion matrix:
cm <- table(val_data$Survived, y_valid)

# accuracy:
sum(diag(cm)) / sum(colSums(cm)) 

# Final test set predictions ----------------------------------------------

knn_pred_test <- class::knn(train = train_data[, col_pos], 
                            test = test_data[, col_pos], 
                            cl = train_data$Survived, 
                            k = 1)
  
y_pred <- ifelse(knn_pred_test == 2, 1, 0)

submission$Survived <- y_pred
write.csv(submission, file = "titanic/submission_r.csv", row.names = FALSE)
