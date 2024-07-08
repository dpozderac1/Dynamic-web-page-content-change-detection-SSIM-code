# require packages needed for model training
if(!require(Cubist)) install.packages("Cubist")
if(!require(caret)) install.packages("caret")
if(!require(RWeka)) install.packages("RWeka")
if(!require(Metrics)) install.packages("Metrics")

library(Cubist)
library(caret)
library(RWeka)
library(Metrics)

# load datasets from CSV files
X_train_le <- read.csv("Data for R/X_train_le.csv")
y_train_le <- read.csv("Data for R/y_train_le.csv")
X_test_le <- read.csv("Data for R/X_test_le.csv")
y_test_le <- read.csv("Data for R/y_test_le.csv")

# change target into vector
y_train_le_vector <- as.numeric(y_train_le[,1])
y_test_le_vector <- as.numeric(y_test_le[,1])


############################## Cubist model ##############################
# define different values for committees and rules to iterate over
committee_values <- c(1, 5, 10)
rules_values <- c(10, 50, 100)

# Cubist model
# hyperparameter tuning
# loop over the values
for(committees in committee_values) {
  for(rules in rules_values) {
    cat("Committees:", committees, "Rules:", rules, "\n")
    
    # variables to accumulate total time needed for training and inference
    total_train_time <- 0
    total_inference_time <- 0
    
    # perform 5 calculations
    for(i in 1:5) {
      # start measuring training time
      train_start_time <- Sys.time()
      # train cubist model
      model <- cubist(x = X_train_le, y = y_train_le_vector, committees = committees, rules = rules)
      # stop measuring training time
      train_end_time <- Sys.time()
      
      # start measuring inference time
      inference_start_time <- Sys.time()
      # create predictions for cubist model
      predictions <- predict(model, X_test_le)
      # stop measuring inference time
      inference_end_time <- Sys.time()
      
      # calculate training and inference time and their cumulative values
      train_time <- as.numeric(train_end_time - train_start_time)
      inference_time <- as.numeric(inference_end_time - inference_start_time)
      total_train_time <- total_train_time + train_time
      total_inference_time <- total_inference_time + inference_time
      
      # calculate metrics
      mse <- mean((predictions - y_test_le_vector)^2)
      rmse <- sqrt(mse)
      ss_res <- sum((y_test_le_vector - predictions)^2)
      mean_y <- mean(y_test_le_vector)
      ss_tot <- sum((y_test_le_vector - mean_y)^2)
      r_squared <- 1 - (ss_res / ss_tot)
      
      cat("\n\nCommittees:", committees, "Rules:", rules, "\n")
      print(paste("Mean Squared Error: ", mse))
      print(paste("Root Mean Squared Error: ", rmse))
      print(paste("R-squared: ", r_squared))
      
      # print training and inference time for the current calculation
      print(paste("Calculation", i, "Train time (seconds):", train_time))
      print(paste("Calculation", i, "Inference time (seconds):", inference_time))
    }
    
    # calculate and print average training and inference time
    avg_train_time <- total_train_time / 5
    avg_inference_time <- total_inference_time / 5
    print(paste("Average train time (seconds):", avg_train_time))
    print(paste("Average inference time (seconds):", avg_inference_time))
    cat("------------------------------------------------\n")
  }
}

# use cross-validation
train_control <- trainControl(method = "cv", 
                             number = 5, 
                             summaryFunction = defaultSummary,
                             savePredictions = TRUE)

# create the tuning grid
grid <- expand.grid(committees = c(1, 5, 10, 20, 25), neighbors = c(0, 2, 5, 7, 9))

# set seed for reproducibility
set.seed(42)

# train cubist using cross-validation
model <- train(x = X_train_le, y = y_train_le_vector, 
               method = "cubist", 
               trControl = trainControl, 
               tuneGrid = grid)

# output the best model's parameters
print(model$bestTune)

# train best cubist model based on parameter values
# start measuring training time
fit_start_time <- Sys.time()

# train the cubist model
cubist_model <- cubist(x = X_train_le, y = y_train_le_vector, committees = 25, neighbors=9)

# stop measuring training time
fit_end_time <- Sys.time()

# calculate training time
cubist_fit_time <- fit_end_time - fit_start_time

# output training time
print(paste("Model training time: ", cubist_fit_time))

# define the tuning grid for cubist model with best parameter values
tuneGrid <- expand.grid(.committees = 25,
                        .neighbors = 9)

cubist_fit <- train(x = X_train_le, y = y_train_le_vector,
                    method = "cubist",
                    trControl = train_control,
                    tuneGrid = tuneGrid,
)

# get cross-validation results
results <- cubist_fit$results
predictions <- cubist_fit$pred

# calculate metrics
mae <- mean(abs(predictions$pred - predictions$obs))
mse <- mean((predictions$pred - predictions$obs)^2)
r_squared <- cor(predictions$pred, predictions$obs)^2

# output validation metrics
print(paste("Cross-Validated Mean Absolute Error: ", mae))
print(paste("Cross-Validated Mean Squared Error: ", mse))
print(paste("Cross-Validated R-squared: ", r_squared))

# create predictions on the training data
train_predictions <- predict(cubist_fit, X_train_le)

# calculate metrics
mae_train <- mean(abs(train_predictions - y_train_le_vector))
mse_train <- mean((train_predictions - y_train_le_vector)^2)
ss_res <- sum((y_train_le_vector - train_predictions)^2)
ss_tot <- sum((y_train_le_vector - mean(y_train_le_vector))^2)
r_squared_train <- 1 - (ss_res / ss_tot)

# output training metrics
cat("Training MAE:", mae_train, "\n")
cat("Training MSE:", mse_train, "\n")
cat("Training R-squared:", r_squared_train, "\n")

# inference time calculation
# store cumulative inference time in variable
total_cubist_inference_time <- 0

# run inference 20 times
for (i in 1:20) {
  start_time <- Sys.time()
  
  # make predictions using test set
  cubist_predictions <- predict(cubist_model, X_test_le)
  
  end_time <- Sys.time()
  total_cubist_inference_time <- total_cubist_inference_time + (end_time - start_time)
}

# calculate average inference time
average_cubist_inference_time <- total_cubist_inference_time / 20

# ouput average inference time
print(paste("Average cubist inference time: ", average_cubist_inference_time))

# calculate MSE, RMSE and MAE metrics for test data
cubist_mse <- mean((cubist_predictions - y_test_le_vector)^2)
cubist_rmse <- sqrt(cubist_mse)
cubist_mae <- mean(abs(cubist_predictions - y_test_le_vector))

# output test metrics
print(paste("Cubist Mean Absolute Error: ", cubist_mae))
print(paste("Cubist Mean Squared Error: ", cubist_mse))
print(paste("Cubist Root Mean Squared Error: ", cubist_rmse))

# calculate R-squared for test data
# calculate Sum of Squared Residuals
cubist_ss_res <- sum((y_test_le_vector - cubist_predictions)^2)
# calculate Total Sum of Squares
cubist_mean_y <- mean(y_test_le_vector)
cubist_ss_tot <- sum((y_test_le_vector - cubist_mean_y)^2)
# calculate R-squared
cubist_r_squared <- 1 - (cubist_ss_res / cubist_ss_tot)

# output R-squared for test data
print(paste("Cubist R-squared: ", cubist_r_squared))
##########################################################################


################################ M5 model ################################
# use cross-validation
train_control <- trainControl(method = "cv", number = 5)

# create hyperparameter grid
param_grid <- expand.grid(M = c(2, 4, 6, 8, 10))

results <- data.frame(M = integer(), RMSE = numeric(), stringsAsFactors = FALSE)

for (i in 1:nrow(param_grid)) {
  # extract current parameters
  current_params <- param_grid[i, ]
  
  # use extracted parameters in the Weka_control function for M5 regression tree model
  model <- M5P(y_train_le_vector ~ ., data = X_train_le, control = Weka_control(M = current_params))
  
  # create predictions
  predictions <- predict(model, X_test_le)
  
  # calculate RMSE for the current model
  rmse <- sqrt(mean((predictions - y_test_le_vector)^2))
  
  # save the results in variable
  results <- rbind(results, data.frame(M = current_params, RMSE = rmse))
}

# find the best parameters for M5 model
best_model <- results[which.min(results$RMSE), ]
print(best_model)

# start measuring time for training
fit_start_time <- Sys.time()

# train the M5 model using the best parameters
M5_model <- M5P(y_train_le_vector ~ ., data = X_train_le, control = Weka_control(M = 10))

# stop measuring time for training
fit_end_time <- Sys.time()

# calculate training time
m5_fit_time <- fit_end_time - fit_start_time

# output training time
print(paste("M5 model training time: ", m5_fit_time))

# cross-validation metrics
data <- data.frame(X_train_le, target = y_train_le_vector)

# set up cross-validation folds
folds <- createFolds(data$target, k = 5, list = TRUE)

# initialize vectors to store metrics
maes <- numeric(length(folds))
mses <- numeric(length(folds))
r_squareds <- numeric(length(folds))

# iterate over folds
for(i in seq_along(folds)) {
  # split the data
  train_indices <- unlist(folds[-i])
  validation_indices <- folds[[i]]
  train_set <- data[train_indices, ]
  validation_set <- data[validation_indices, ]
  
  # train the M5 model tree
  m5_model <- M5P(target ~ ., data = train_set)
  
  # create prediction on the validation set
  predictions <- predict(m5_model, validation_set)
  
  # calculate validation metrics
  mae <- mean(abs(predictions - validation_set$target))
  mse <- mean((predictions - validation_set$target)^2)
  ss_tot <- sum((validation_set$target - mean(validation_set$target))^2)
  ss_res <- sum((validation_set$target - predictions)^2)
  r_squared <- 1 - (ss_res / ss_tot)
  
  # store metrics
  maes[i] <- mae
  mses[i] <- mse
  r_squareds[i] <- r_squared
}

# calculate and output average validation metrics
avg_mae <- mean(maes)
avg_mse <- mean(mses)
avg_r_squared <- mean(r_squareds)

cat("Average validation MAE:", avg_mae, "\n")
cat("Average validation MSE:", avg_mse, "\n")
cat("Average validation R-squared:", avg_r_squared, "\n")

# inference time calculation
# store cumulative inference time in variable
total_m5_inference_time <- 0

# run inference 20 times
for (i in 1:20) {
  start_time <- Sys.time()
  
  # create predictions using the test set
  predictions_M5 <- predict(M5_model, X_test_le)
  
  end_time <- Sys.time()
  total_m5_inference_time <- total_m5_inference_time + (end_time - start_time)
}

# calculate average inference time
average_m5_inference_time <- total_m5_inference_time / 20

# output average inference time
print(paste("Average M5 model inference time: ", average_m5_inference_time))

# create predictions on the training data
train_predictions <- predict(M5_model, X_train_le)

# calculate metrics
mae_train <- mean(abs(train_predictions - y_train_le_vector))
mse_train <- mean((train_predictions - y_train_le_vector)^2)
ss_res <- sum((y_train_le_vector - train_predictions)^2)
ss_tot <- sum((y_train_le_vector - mean(y_train_le_vector))^2)
r_squared_train <- 1 - (ss_res / ss_tot)

# output the training metrics
cat("Training MAE:", mae_train, "\n")
cat("Training MSE:", mse_train, "\n")
cat("Training R-squared:", r_squared_train, "\n")

# create predictions on the test set
predictions_M5 <- predict(M5_model, newdata = X_test_le)

# calculate metrics
mse_m5 <- mse(y_test_le_vector, predictions_M5)
rmse_m5 <- sqrt(mse_m5)
mae_m5 <- mae(y_test_le_vector, predictions_M5)

# calcule R-squared
sst <- sum((y_test_le_vector - mean(y_test_le_vector))^2)
ssr_m5 <- sum((y_test_le_vector - predictions_M5)^2)
r_squared_m5 <- 1 - (ssr_m5 / sst)

# output metrics
cat("M5 model metrics\n")
cat(sprintf("MSE: %f\n", mse_m5))
cat(sprintf("RMSE: %f\n", rmse_m5))
cat(sprintf("MAE: %f\n", mae_m5))
cat(sprintf("R-squared: %f\n", r_squared_m5))
##########################################################################