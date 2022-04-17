# load data:
train <- read.csv("./titanic/train.csv")
test <- read.csv("./titanic/test.csv")
submission <- read.csv("./titanic/gender_submission.csv")

y_train <- train$Survived

# col position for y: 
c(which(colnames(train) == "Survived"))
train <- train[, -2]

# train and test set rows:
train$istrain <- 1
test$istrain <- 0

# concatenating test and train datasets:
all_data <- rbind(train, test)

c(which(colnames(all_data) == "PassengerId"))
all_data <- all_data[, -1]

# classes for each column:
lapply(all_data, class)

# we need to change the classes for factors and numeric:
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


# variables: Sex + Age + SibSp + Fare + Parch + Pclass
head(all_data_casted)
k_data <- all_data_casted[,c(which(colnames(all_data_casted) %in% c("Sex", "Age", "SibSp", "Fare", "Parch", "Pclass")))]
names(k_data)

# converting factors to dummies
sex_dummy <- model.matrix( ~ Sex - 1, k_data)
pclass_dummy <- model.matrix( ~ Pclass - 1, k_data)

# adding to data
k_data <- data.frame(k_data[, !colnames(k_data) %in% c("Sex","Pclass")], sex_dummy, pclass_dummy)

# Normalizing variables for k-means clustering
k_data_normalized <- scale(k_data)

set.seed(1)
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
k_data_cluster <- kmeans(k_data_normalized, centers = 6) 

# adding new feature to original data: 
all_data_casted$Cluster <- as.factor(k_data_cluster$cluster)


# train, val and test: ----------------------------------------------------


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

# logistic regression  ----------------------------------------------------

glm_fit <- glm(Survived ~ Sex + Age + SibSp + Fare + Parch + Embarked + Pclass + Cluster, 
               data = train_data, 
               family = binomial); summary(glm_fit)

# updating:
glm_fit <- glm(Survived ~ Sex + Age + SibSp + Pclass + Cluster, 
               data = train_data, 
               family = binomial); summary(glm_fit)

# val_data predictions:
y_probs <- predict(glm_fit, newdata = val_data, type = "response")
y_valid <- ifelse(y_probs > 0.5, 1, 0)

# confusion matrix:
cm <- table(val_data$Survived, y_valid)

# accuracy:
sum(diag(cm)) / sum(colSums(cm)) 

# Final test set predictions ----------------------------------------------

y_test_probs <- predict(glm_fit, newdata = test, type = "response")
y_pred <- ifelse(y_test_probs > 0.5, 1, 0)

submission$Survived <- y_pred
write.csv(submission, file = "titanic/submission_r.csv", row.names = FALSE)