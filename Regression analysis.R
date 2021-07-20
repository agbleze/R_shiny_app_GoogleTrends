library(tidyverse)
library(caret)
library(h2o)
library(rsample)
library(ggplot2)


########### spliting dataset into training and test set  ################
set.seed(123)
dataset_num <- data.frame(data_select[, c(1, 3:8)])
data_split <- initial_split(dataset_num, prop = 0.7)
training_dataset <- training(data_split)
test_dataset <- testing(data_split)

ggplot(test_dataset, mapping = aes(Revenue)) + geom_density(trim = T) + geom_density(training_dataset, trim = T, col = "red",
                                                                                     mapping = aes(Revenue))

###### Response transformation  ##########
train_resp_log <- log(training_dataset$Revenue) 
test_resp_log <- data.frame(log(test_dataset$Revenue))

ggplot(test_resp_log, mapping = aes(log.test_dataset.Revenue.)) + geom_density(trim = T) +
  geom_density(train_resp_log, mapping = aes(log.training_dataset.Revenue.), trim = T, col = "green")


### Predictor transformation
## standardize predictor 
# get predictors
featurespre <- setdiff(names(training_dataset), "Revenue")
## preprocess based on center, scale amg from training set
preprocess_x <- preProcess(training_dataset[, featurespre],
                                   method = c("center", "scale"))

training_predictor_var <- predict(preprocess_x, training_dataset[, featurespre])
testing_predictor_var <- predict(preprocess_x, test_dataset[, featurespre])

## lm model formulation with matrix formulation (glmnet())
## no standardization required for glmnet(). It is undertaken internally
training_response <- training_dataset$Revenue
training_predictor <- training_dataset[,featurespre]

(glmnet(x = training_predictor, y = training_response))

##lm
(lm_model <- lm(Revenue ~ ., data = training_dataset))
(glm_model <- glm(Revenue ~ ., data = training_dataset, family = gaussian))
(lm_caret <- train(Revenue ~ ., data = training_dataset, method = "lm"))


## using h2o with variable name specication
h2o_train_data <- as.h2o(training_dataset)
predictor <- setdiff(names(training_dataset), "Revenue")
response <- "Revenue"
(h2o_lm <- h2o.glm(x = predictor, y = response, training_frame = h2o_train_data))
