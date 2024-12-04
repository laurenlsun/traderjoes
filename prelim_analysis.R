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


names(df)


ggplot(df, aes(x = factor(tj), y = medinc, fill = factor(tj))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(
    #title = "Median Income",
    x = "ZIP code has TJ's?",
    y = "Median Income"
  ) +
  theme_minimal()


ggplot(df, aes(x = factor(tj), y = overall_diff, fill = factor(tj))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(
    title = "Home Price Increase",
    x = "ZIP code has TJ's?",
    y = "Home Price Increase"
  ) +
  theme_minimal()

ggplot(df, aes(x = factor(tj), y = medage, fill = factor(tj))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(
    title = "Median Age",
    x = "ZIP code has TJ's?",
    y = "Median Age"
  ) +
  theme_minimal()


df_long <- df %>%
  pivot_longer(
    cols = c(sales, serv, labor),
    names_to = "Feature",
    values_to = "Value"
  )

df_long$tj <- factor(df_long$tj, levels = c(0, 1), labels = c("No", "Yes"))
ggplot(df_long, aes(x = Feature, y = Value, fill = factor(tj))) +
  geom_boxplot() +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "orange"), 
                    name = "ZIP code has TJ's?") +
  labs(
    title = "TJ's vs. No TJ's",
    x = "Industry of Employment",
    y = "Percent of Population"
  ) +
  theme_minimal()



df_long <- df %>%
  pivot_longer(
    cols = c(hs, bach),
    names_to = "Feature",
    values_to = "Value"
  )
df_long$tj <- factor(df_long$tj, levels = c(0, 1), labels = c("No", "Yes"))

ggplot(df_long, aes(x = Feature, y = Value, fill = factor(tj))) +
  geom_boxplot() +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "orange"), 
                    name = "ZIP code has TJ's?") +
  labs(
    title = "TJ's vs. No TJ's",
    x = "Obtained",
    y = "Percent of Population"
  ) +
  theme_minimal()

df_long <- df %>%
  pivot_longer(
    cols = c(hs, bach),
    names_to = "Feature",
    values_to = "Value"
  )
df_long$tj <- factor(df_long$tj, levels = c(0, 1), labels = c("No", "Yes"))

ggplot(df_long, aes(x = Feature, y = Value, fill = factor(tj))) +
  geom_boxplot() +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "orange"), 
                    name = "ZIP code has TJ's?") +
  labs(
    title = "TJ's vs. No TJ's",
    x = "Obtained",
    y = "Percent of Population"
  ) +
  theme_minimal()



df_long_breakdown <- df %>%
  pivot_longer(
    cols = c("au18", "a18to24", "a25to29", "a30to34", "midage", "ao65"),
    names_to = "Age_Group",
    values_to = "Percentage"
  )

ggplot(df_long_breakdown, aes(x = factor(tj, labels = c("No", "Yes")), 
                              y = Percentage, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set3", name = "Age Group") +
  labs(
    title = "Age Breakdown",
    x = "TJ's?",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

df_long_breakdown <- df %>%
  pivot_longer(
    cols = c("white", "black", "indg", "asian", "hisp"),
    names_to = "Race",
    values_to = "Percentage"
  )

ggplot(df_long_breakdown, aes(x = factor(tj, labels = c("No", "Yes")), 
                              y = Percentage, fill = Race)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "Set3", name = "Race") +
  labs(
    title = "Race Breakdown",
    x = "TJ's?",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
