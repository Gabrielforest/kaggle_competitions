library(randomForest)

train <- read.csv("./train.csv")
test <- read.csv("./test.csv")
submission <- read.csv("./sample_submission.csv")

# removendo ID:
train <- train[, -1]
test <- test[, -1]

# caractere para fator e inteiro para numérico
train_character_logical <- sapply(train, is.character)
train_character <- train[, train_character_logical]

# criando categoria missing:
train_character <- replace(train_character, is.na(train_character), "missing")
train_factor <- as.data.frame(sapply(train_character, as.factor), stringsAsFactors = TRUE)

train_integer <- train[, !train_character_logical]
train_numeric <- as.data.frame(sapply(train_integer, as.numeric))

train <- cbind.data.frame(train_numeric, train_factor)


# caractere para fator e inteiro para numérico
test_character_logical <- sapply(test, is.character)
test_character <- test[, test_character_logical]

# criando categoria missing:
test_character <- replace(test_character, is.na(test_character), "missing")
test_factor <- as.data.frame(sapply(test_character, as.factor), stringsAsFactors = TRUE)

test_integer <- test[, !test_character_logical]
test_numeric <- as.data.frame(sapply(test_integer, as.numeric))

test <- cbind.data.frame(test_numeric, test_factor)


SalePrice <- train$SalePrice
c(which(colnames(train) == "SalePrice"))
train <- train[, -37]

train <- train[, order(names(train))]
test <- test[, order(names(test))]
(colnames(test) == colnames(train))

full <- rbind.data.frame(train, test)

# substituindo NAs dos valores numéricos por valores medianos:
full_na <- na.roughfix(full)
rf <- randomForest(SalePrice ~ ., full_na[1:1460,])
p <- predict(rf, full_na[1461:nrow(full_na),])

submission$SalePrice <- p
write.csv(submission, file = "submission.csv", row.names = FALSE)