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
df %>% 
  rename(
    diff = overall_diff
  )
dim(df)
df <- na.omit(df)
dim(df)
df$zip <- sprintf("%05s", df$zip)

tjs = read_csv("store_info.csv")
names(tjs)
dim(tjs)
tjzips = unique(tjs$zip)
tjzips = tjzips[which(nchar(tjzips) < 11)]
tjzips = substr(tjzips,1,5)
tjzips <- sprintf("%05s", tjzips)
length(tjzips)

class(df$zip)
class(tjzips)

length(which(df$zip %in% tjzips))
# add 0 or 1 for trader joes
df$tj = as.integer(df$zip %in% tjzips)
length(which(df$tj==1))
length(which(df$tj==0))

indices_tj_1 <- which(df$tj == 1)
indices_tj_0 <- which(df$tj == 0)

# uncomment if balance dataset:
indices_tj_0 = sample(indices_tj_0, size = 1000)
keep_index = c(indices_tj_1, indices_tj_0)
df = df[keep_index, ]
row.names(df) <- NULL # reset index
indices_tj_1 <- which(df$tj == 1)
indices_tj_0 <- which(df$tj == 0)
keep_index = c(indices_tj_1, indices_tj_0)
######################
# re split

# 75/25 train/test
y_test_indices_tj_0 <- sample(indices_tj_0, size = floor(length(indices_tj_0) / 4))
y_test_indices_tj_1 <- sample(indices_tj_1, size = floor(length(indices_tj_1) / 4))
test_indices <- c(y_test_indices_tj_1, y_test_indices_tj_0)
dim(df[test_indices,])
train_indices = setdiff(keep_index, c(y_test_indices_tj_0, y_test_indices_tj_1))
dim(df[train_indices,])

predictors = colnames(df)[!colnames(df) %in% c("zip", "lat", "lng", "city", "state_name", "closest_city", "tj", "avg_diff")]
length(predictors)
corr_matrix = cor(df[, predictors])
corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# best subset
formula <- as.formula(paste("tj ~", paste(predictors, collapse = " + ")))
fullsubs = regsubsets(formula, data=df[train_indices,], nvmax = length(predictors), method = "exhaustive")
summary(fullsubs)

plot(fullsubs, scale = "adjr2")
plot(summary(fullsubs)$rss, xlab = "Number of Variables",
     ylab = "RSS", type = "l")
plot(summary(fullsubs)$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")

#fit glm with 5 predictors
unique(df$tj)
best5 = coef(fullsubs, 5)
best5preds <- names(best5)[-1]
best5preds
formula_best5 <- paste("tj ~", paste(best5preds, collapse = " + "))
glm.fits <- glm(
  formula_best5,  data = df[train_indices,], family = binomial
)
glm.fits
dim(df[test_indices,])
glm.probs <- predict(glm.fits, newdata = df[test_indices,] , type = "response")
glm.preds = round(glm.probs)
truth = df$tj[test_indices]
confusionMatrix(factor(glm.preds), factor(truth))

false_positives_indices = which(glm.preds == 1 & truth == 0)
false_positives_indices

# compare to absolute best (all)
glm.fits <- glm(
  formula,  data = df[train_indices,], family = binomial
)
glm.probs <- predict(glm.fits, newdata = df[test_indices,] , type = "response")
predictions = round(glm.probs)
confusionMatrix(factor(predictions), factor(truth))
false_positives_indices = intersect(false_positives_indices, which(predictions == 1 & truth == 0))

# decision tree
df$tj <- as.factor(df$tj)
tree = tree(formula, data=df[train_indices,])
summary(tree)
plot(tree, pretty=TRUE)
text(tree)
predictions = predict(tree, df[test_indices,], type = "class")
confusionMatrix(factor(predictions), factor(truth))
false_positives_indices = intersect(false_positives_indices, which(predictions == 1 & truth == 0))

# bagging
bag.tj <- randomForest(formula, df[train_indices,], mtry = 12, importance = TRUE)
bag.tj

yhat.bag <- predict(bag.tj, newdata = df[test_indices, ])
predictions <- as.factor(yhat.bag)
truth <- as.factor(df$tj[test_indices])
confusionMatrix(factor(predictions), factor(truth))
false_positives_indices = intersect(false_positives_indices, which(predictions == 1 & truth == 0))
false_positives_indices

importance(bag.tj)
varImpPlot(bag.tj)



# neural network

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

x_train <- df[train_indices, predictors]
y_train <- df[train_indices, "tj"]

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
    fit(x_train, y_train, epochs =50, batch_size = 16,
          validation_split = 0.2)
  )
plot(history, smooth = FALSE)

modelnn %>% evaluate(x_test, y_test)

nnpreds <- modelnn %>% predict(x_test)
predicted_classes <- apply(nnpreds, 1, which.max) - 1  # Subtract 1 if your labels are 0-indexed
true_classes <- apply(y_test, 1, which.max) - 1  # Subtract 1 for zero-based class labels
confusionMatrix(as.factor(predicted_classes), as.factor(true_classes))
false_positives_indices = intersect(false_positives_indices, which(predicted_classes == 1 & true_classes == 0))

names(df)
false_positives_indices
df[test_indices,][false_positives_indices, c("zip", "city", "overall_diff", "medage", "medinc", "asian", "white", "black", "tj")]

