library(ggplot2)
library(readr)
library(maps)
library(ggmap)
library(dplyr)
library(tidygeocoder)
library(tidyr)
library(viridis)
library(sf)
library(purrr)
library(corrplot)
library(leaps)
library(boot)
library(caret)
library(tree)
library(randomForest)
library(keras)
library(tensorflow)

set.seed(1)

setwd("C:/Users/llaur/Downloads")
df = read_csv("data.csv")
names(df)
dim(df)
df <- na.omit(df)
dim(df)

head(df)
corr_matrix = cor(df[, 7:ncol(df)])
corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

tjs = read_csv("store_info.csv")
names(tjs)
tjzips = unique(tjs$zip)
length(tjzips)

# add 0 or 1 for trader joes
df$tj = as.integer(df$zip %in% tjzips)
indices_tj_1 <- which(df$tj == 1)
indices_tj_0 <- which(df$tj == 0)
# 1/4 no tjs
y_test_indices_tj_0 <- sample(indices_tj_0, size = floor(length(indices_tj_0) / 4))
y_test_indices_tj_1 <- sample(indices_tj_1, size = floor(length(indices_tj_1) / 4))
# 1/4 tjs
test_indices <- c(y_test_indices_tj_1, y_test_indices_tj_0)

# best subset
predictors = colnames(df)[!colnames(df) %in% c("zip", "lat", "lng", "city", "state_name", "closest_city", "tj")]
formula <- as.formula(paste("tj ~", paste(predictors, collapse = " + ")))
fullsubs = regsubsets(formula, data=df[-test_indices,], nvmax = length(predictors), method = "exhaustive")
summary(fullsubs)

plot(fullsubs, scale = "adjr2")
plot(summary(fullsubs)$rss, xlab = "Number of Variables",
     ylab = "RSS", type = "l")
plot(summary(fullsubs)$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")

#fit glm with 5 predictors
best5 = coef(fullsubs, 5)
best5preds <- names(best5)[-1]
formula_best5 <- paste("tj ~", paste(best5preds, collapse = " + "))
glm.fits <- glm(
  formula_best5,  data = df[-test_indices,], family = binomial
)
glm.probs <- predict(glm.fits, newdata = df[test_indices,] , type = "response")
glm.probs
predictions = round(glm.probs)
truth = df$tj[test_indices] 
confusion_matrix <- table(Predicted = predictions, Actual = truth)
print(confusion_matrix)
confusionMatrix(factor(predictions), factor(truth))

# compare to absolute best (all)
glm.fits <- glm(
  formula,  data = df[-test_indices,], family = binomial
)
glm.probs <- predict(glm.fits, newdata = df[test_indices,] , type = "response")
glm.probs
predictions = round(glm.probs)
confusion_matrix <- table(Predicted = predictions, Actual = truth)
print(confusion_matrix)
confusionMatrix(factor(predictions), factor(truth))

# plot false positives. potential new trader joe's??
false_positives_indices <- which(predictions == 1 & truth == 0)
glmfp = df[test_indices,][false_positives_indices, c("zip", "city", best5preds)]
glmfp

# decision tree
df$tj <- as.factor(df$tj)
tree = tree(formula, data=df[-test_indices,])
summary(tree)
plot(tree)
text(tree)
predictions = predict(tree, df[test_indices,], type = "class")

# bagging
bag.tj <- randomForest(formula, df[-test_indices,], mtry = 12, importance = TRUE)
bag.tj

yhat.bag <- predict(bag.tj, newdata = df[test_indices, ])
predictions <- as.factor(yhat.bag)
truth <- as.factor(df$tj[test_indices])
table(Predicted = predictions, Actual = truth)

importance(bag.tj)
varImpPlot(bag.tj)



# neural network - no class imbalance

input <- layer_input(shape = c(21))  
output <- input %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = "softmax")


modelnn <- keras_model(inputs = input, outputs = output)
modelnn %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

x_train <- df[-test_indices, predictors]
y_train <- df[-test_indices, "tj"]

x_test <- df[test_indices, predictors]
y_test <- df[test_indices, "tj"]

x_train <- data.matrix(x_train)
x_test <- data.matrix(x_test)

x_train <- scale(x_train)
x_test <- scale(x_test)


y_train <- to_categorical(as.integer(unlist(y_train)) - 1)
y_test <- to_categorical(as.integer(unlist(y_test)) - 1)

class(x_train)
class(x_test)
class(y_train)
class(y_test)

dim(y_train)

system.time(
  history <- modelnn %>%
    fit(x_train, y_train, epochs = 10, batch_size = 128,
          validation_split = 0.2)
  )
plot(history, smooth = FALSE)

modelnn %>% evaluate(x_test, y_test)

nnpreds <- modelnn %>% predict(x_test)
predicted_classes <- apply(nnpreds, 1, which.max) - 1  # Subtract 1 if your labels are 0-indexed
true_classes <- apply(y_test, 1, which.max) - 1  # Subtract 1 for zero-based class labels
confusionMatrix(as.factor(predicted_classes), as.factor(true_classes))

# fix class imbalance
