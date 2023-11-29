# imports
library(rpart)
library(dplyr)
library(randomForest)

# References:
# OM420 slides
# https://www.gormanalysis.com/blog/decision-trees-in-r-using-rpart/
# https://stats.stackexchange.com/questions/103018/difference-between-rel-error-and-xerror-in-rpart-regression-trees
# https://stackoverflow.com/questions/68945954/how-to-use-mutate-if-to-change-values
# https://stackoverflow.com/questions/48248037/how-to-use-which-min-function-for-a-list
# https://www.statology.org/random-forest-in-r/
# https://www.statology.org/bagging-in-r/
# https://www.geeksforgeeks.org/the-validation-set-approach-in-r-programming/



#1
heart <- read.csv("heart.csv")
heart <- mutate_if(heart, is.character, as.factor)

#2
set.seed(2) # a
nrow_heart <- nrow(heart) # b
nrow_to_sample <- (2/3) * nrow_heart
sampeled_indicies <- sample(1:nrow_heart, size = nrow_to_sample, replace = FALSE)
heart_train <- heart[sampeled_indicies, ] # c

val_indices <- setdiff(1:nrow_heart, sampeled_indicies) # d
heart_val <- heart[val_indices, ]



#3
classification_tree <- rpart(formula = AHD ~ ., data = heart_train, method = "class", na.action = na.omit)

#4
printcp(classification_tree)
plotcp(classification_tree)

# a: as the size of the sub-tree increases, the rel_error decreases slowly, then increases quite fast
# b: as the size of the sub-tree increase, initially the x-error decreases as the model is fit properly (fit perfectly  = xerror minimized).
     # However at a point, the sub-tree becomes too complex and starts to overfit, leading to increased xerror.

# note; couldn't find where he mentioned this in the slides, so I'm getting from the internet
# c: Cross-validation: chooses cp that minimizes x-error  
# 1-SE: This method chooses a simpler tree that is within one standard error of the minimum x-error
# I don't know which was method 2 so i used both

#5
cptable <- classification_tree$cptable
n_tree <- nrow(cptable)  # the number of CP values
# change the array type
validation.error.rates <- numeric(n_tree)

for (i in 1:n_tree) {
  # prune the tree
  pruned_tree <- prune(classification_tree, cp = cptable[i, "CP"])
  
  # predict AHD
  prediction <- predict(pruned_tree, newdata = heart_val, type = "class")

  
  # compute the validation error rate
  # for some reason I don't think this is right, it shouldn't be 0...
  cross_table <- table(na.omit(heart_val$AHD), prediction)
  validation.error.rate <- 1 - sum(diag(cross_table)) / sum(cross_table)
  validation.error.rates[i] <- validation.error.rate
}

# print the computed error rates
print(validation.error.rates)

#6
# a: goes down as complexity increases (model fits better), increases after optimal fit (overfits).
#   --> We observe this trend because data is often in wide variety, 
#   and we don't want to generalize all predictions off of the training set.

#b. You can do this by mapping indicies with the error_rate array and the cptable (see below)
index <- which.min(validation.error.rates)
cp_val <- cptable[index, "CP"]

#7 
# no specific model type specified, I am going to do a random forest with mtry=10
# create random forest with bagging
rf2 <- randomForest(AHD ~ ., data = heart_train, mtry=10, importance = TRUE, na.action = na.omit)
prediction_rf2 <- predict(rf2, newdata = heart_val)

# get error rate
cross_rf2 <- table(na.omit(heart_val$AHD), prediction_rf2)
rf_error2 <- 1 - sum(diag(cross_rf2)) / sum(cross_rf2)



#8
# create random forest with default mtry
rf <- randomForest(AHD ~ ., data = heart_train, importance = TRUE, na.action = na.omit)
prediction_rf <- predict(rf, newdata = heart_val)

# get error rate
cross_rf <- table(na.omit(heart_val$AHD), prediction_rf)
rf_error <- 1 - sum(diag(cross_rf)) / sum(cross_rf)



#9
# Based on the error rate, the best model appears to be the random forest w/o bagging 
# (rf_error is minimized).


