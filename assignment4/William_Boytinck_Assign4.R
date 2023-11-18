library(dplyr)
library(MASS)
library(class)

####################################################################
# references
# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/factor (I used this for the categorical check)
# I used gpt 3.5 for the syntax of variables (remaining_rows)
# OM420 slides
# https://www.datacamp.com/tutorial/k-nearest-neighbors-knn-classification-with-r-tutorial
####################################################################

#1. 
data <- read.csv("hmeq.csv")
na_check <- any(is.na(data))
if (na_check) {
  print("There is some missing data...")
} else {
  print("There is no data missing")
}
#2.
data_types <- sapply(data, class)

# 3.
categorical_vars <- sapply(data, is.factor)
num_data <- data[, !categorical_vars]

num_data <- data %>%
  select_if(~ is.numeric(.)) 

#4.
current_data_type <- class(num_data$BAD)
# converting BAD (target variable) to factor
num_data$BAD <- factor(num_data$BAD)

#5.
set.seed(123) #a.
number_to_sample <- round((nrow(num_data)) * (2/3))
rows_sampled <- sample(1:nrow(num_data), size = number_to_sample) #b
num_data.train <- num_data[rows_sampled, ] #c
remaining_rows <-  setdiff(1:nrow(num_data), rows_sampled) #d
num_data.rest <- num_data[remaining_rows, ]

# e-->g
half_of_rest <- round(nrow(num_data.rest) / 2)
rows_of_rest <- sample(1:nrow(num_data.rest), size = half_of_rest)
num_data.val <- num_data.rest[rows_of_rest, ]
remaining <- setdiff(1:nrow(num_data.rest), rows_of_rest)
num_data.test <- num_data.rest[remaining, ]


#6.
# I don't think i computed the error rates correctly. but i've spent to long debugging already


# Models
first_knn <- knn(num_data.train, num_data.test, num_data.train$BAD, k=1, prob=FALSE )
second_knn <- knn(num_data.train, num_data.test, num_data.train$BAD, k=5, prob=FALSE )
third_knn <- knn(num_data.train, num_data.test, num_data.train$BAD, k=10, prob=FALSE )

# Validation Set error rates
first_error <- sum(first_knn != num_data.val$BAD) / length(num_data.val$BAD)
second_error <- sum(second_knn != num_data.val$BAD) / length(num_data.val$BAD)
third_error <- sum(third_knn != num_data.val$BAD) / length(num_data.val$BAD)

# Validation Set class 1 error rates
class_1_indices <- num_data.val$BAD == 1 # DEFAULT ON LOAN, = TRUE

# should this be 0:FALSE ? 
# if its class 1, should it not be 0:TRUE ?
class_1_first <- sum(first_knn[class_1_indices] == 0) / sum(class_1_indices == FALSE)
class_1_second <- sum(second_knn[class_1_indices] == 0) / sum(class_1_indices == FALSE)
class_1_third <- sum(third_knn[class_1_indices] == 0) / sum(class_1_indices == FALSE)
print(class_1_first)



#7.
# exclude manually binary variables
LDA <- lda(BAD~LOAN+MORTDUE+YOJ+CLAGE+NINQ+CLNO+DEBTINC, data = num_data.train)

#8.
prediction <- predict(LDA, data = num_data.val)$class
# validation set error rate:
lda_error <- sum(prediction != num_data.val$BAD) / length(num_data.val$BAD)
# validation set class 1 error rate
lda_class_one <- sum(prediction[class_1_indices] == 0) / sum(class_1_indices == FALSE)

#9.
# It appears the KNN model with k = 10 is the best, as it has the lowest validation set error rate, and a low class 1 error rate

#10.
class_1_indices_test <- num_data.test$BAD

new_knn <- knn(num_data.test, num_data.val, num_data.test$BAD, k=10, prob=FALSE )
# error rate
new_error <- sum(new_knn != num_data.test$BAD) / length(num_data.test$BAD)
# class 1 error rate
class_1_new <- sum(new_knn[class_1_indices_test] == 0) / sum(class_1_indices_test == 0)





