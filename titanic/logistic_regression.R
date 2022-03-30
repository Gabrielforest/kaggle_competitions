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
  
# model and 10 fold cross validation --------------------------------------

glm_fit <- glm(Survived ~ Sex + Age + SibSp + Fare + Parch + Embarked + Pclass, 
               data = train_data, 
               family = binomial); summary(glm_fit)

# updating:
glm_fit <- glm(Survived ~ Sex + Age + SibSp + Pclass, 
               data = train_data, 
               family = binomial); summary(glm_fit)

cv_error_10 <- boot::cv.glm(data = train_data, glmfit = glm_fit, K = 10)$delta[1]

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