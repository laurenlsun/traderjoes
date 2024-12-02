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


