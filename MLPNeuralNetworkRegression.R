
# Install and load the necessary libraries or packages

install.packages("tensorflow")
install.packages("keras")
install.packages("tibble")
install.packages("Rtools")

library(keras)

library(tensorflow)

install_tensorflow(method = "conda")

install_keras()

install_keras(method = c("auto", "virtualenv", "conda"),
              conda = "auto", version = "default", tensorflow = "default",
              extra_packages = c("tensorflow-hub"))


library(tensorflow)
Sys.setenv(TENSORFLOW_PYTHON="/usr/local/bin/python")


sess = tf$Session()
hello <- tf$constant('Hello, TensorFlow!')
sess$run(hello)


#  REGRESSION TRIAL WITH TENSORFLOW


setwd("F:/Users/fnkeumoe/GapFraction_NNdata")

train_data <-read.csv("train_features.csv")
train_labels <-read.csv("train_labels.csv")
test_data <-read.csv("test_features.csv")
test_labels <-read.csv("test_labels.csv")

train_data <- as.data.frame(train_data)
train_labels <- as.data.frame(train_labels)
test_data <- as.data.frame(test_data)
test_labels <- as.data.frame(test_labels)

# Convert dataframes to matrices

train_data = data.matrix(train_data)
train_labels = data.matrix(train_labels)
test_data = data.matrix(test_data)
test_labels = data.matrix(test_labels)

#boston_housing <- dataset_boston_housing()

#c(train_data, train_labels) %<-% boston_housing$train
#c(test_data, test_labels) %<-% boston_housing$test


paste0("Training entries: ", length(train_data), ", labels: ", length(train_labels))



library(tibble)

column_names <- c('VH', 'VV', 'VHdb', 'VVdb', 'VVminVH', 'nVHminVV', 'VHminVV', 
                  'VVVHra', 'VHVVra', 'nVVminVH')

train_df <- as_tibble(train_data)

colnames(train_df) <- column_names

train_df




# LABELS
#The labels are the Canopy Gap Fraction in percentages. 

train_labels[1:10] # Display first 10 entries



# NORMALISING THE MODEL

# Test data is *not* used when calculating the mean and std.

# Normalize training data
train_data <- scale(train_data) 

# Use means and standard deviations from training set to normalize test set
col_means_train <- attr(train_data, "scaled:center") 
col_stddevs_train <- attr(train_data, "scaled:scale")
test_data <- scale(test_data, center = col_means_train, scale = col_stddevs_train)

train_data[1, ] # First training sample, normalized



# CREATING THE MODEL

build_model1 <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 128, activation = "relu",
                input_shape = dim(train_data)[2]) %>%
    #layer_dropout(rate=0.2)%>%
    layer_dense(units = 64, activation = "relu") %>%
    #layer_dropout(rate=0.2)%>%
    layer_dense(units = 10, activation = "relu") %>%
    #layer_dropout(rate = 0.1)%>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(lr=0.003),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model1 <- build_model1()
model1 %>% summary()



# TRAINING THE MODEL

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 100

# Fit the model and store training stats
history1 <- model1 %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  #verbose = 0,
  callbacks = list(print_dot_callback)
)


# PLOTTING THE RESULTS

library(ggplot2)

#plot(history1, metrics = "mean_absolute_error", smooth = FALSE) +
#  coord_cartesian(ylim = c(0, 5))

plot(history1, metrics = "mean_absolute_error", smooth = FALSE) 


#This graph shows little improvement in the model after about 150 epochs. 
#Let's update the fit method to automatically stop training when the validation 
#score doesn't improve. We'll use a callback that tests a training condition for every epoch. 
#If a set amount of epochs elapses without showing improvement, 
#it automatically stops the training.


# Estimate early stopping value
# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 50)

history2 <- model1 %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)

plot(history2, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 16)) #



# Let's see how did the model1 performs on the test set:
  
c(loss, mae) %<-% (model1 %>% evaluate(test_data, test_labels, verbose = 0))

paste0("Mean absolute error on test set:", sprintf("%.2f", mae),"%")

model1 %>% evaluate(test_data, test_labels)



# PREDICT 
# Predict some Gap Fraction using data in the testing set:

test_predictions <- model1 %>% predict(test_data)
test_predictions[ , 1]

mean((test_labels-test_predictions)^2)

plot(test_predictions,test_labels)


#---------------------------------------------------------
#------------------------------------------------------------
# MODEL IMPROVEMENT

# CREATING THE MODEL


build_model2 <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 128, activation = "tanh", bias_regularizer = regularizer_l2(0.01),
                input_shape = dim(train_data)[2]) %>%
    #layer_dropout(rate=0.4)%>%
    layer_dense(units = 64, activation = "tanh", bias_regularizer = regularizer_l2(0.01)) %>%
    #layer_dropout(rate=0.3)%>%
    
    layer_dense(units = 10, activation = "tanh", bias_regularizer = regularizer_l2(0.01)) %>%
    #layer_dropout(rate = 0.2)%>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(lr=0.01),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model2 <- build_model2()
model2 %>% summary()



# TRAINING THE MODEL

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 300

# Fit the model and store training stats
history3 <- model2 %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  #verbose = 0,
  callbacks = list(print_dot_callback)
)


# PLOTTING THE RESULTS

library(ggplot2)

#plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
#  coord_cartesian(ylim = c(0, 5))

plot(history3, metrics = "mean_absolute_error", smooth = FALSE) 


#This graph shows little improvement in the model after about 150 epochs. 
#Let's update the fit method to automatically stop training when the validation 
#score doesn't improve. We'll use a callback that tests a training condition for every epoch. 
#If a set amount of epochs elapses without showing improvement, 
#it automatically stops the training.


# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 50)

#model <- build_model()
history4 <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)

plot(history4, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(xlim = c(0, 125), ylim = c(4, 16)) #



# Let's see how did the model2 performs on the test set:

c(loss, mae) %<-% (model2 %>% evaluate(test_data, test_labels, verbose = 0))

paste0("Mean absolute error on test set:", sprintf("%.2f", mae),"%")

model2 %>% evaluate(test_data, test_labels)


#
# PREDICT 
# Predict some Gap Fraction using data in the testing set:

test_predictions <- model2 %>% predict(test_data)
test_predictions[ , 1]

mean((test_labels-test_predictions)^2)

plot(test_predictions,test_labels)


#---------------------------------------------------------
#------------------------------------------------------------
# MODEL IMPROVEMENT Two

# CREATING THE MODEL


build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 128, activation = "tanh", kernel_initializer='RandomNormal',
                bias_regularizer = regularizer_l2(0.25),
                input_shape = dim(train_data)[2]) %>%
    layer_dropout(rate=0.4)%>%
    layer_dense(units = 100, activation = "tanh", 
                bias_regularizer = regularizer_l2(0.25)) %>%
    layer_dropout(rate=0.4)%>%
    
    layer_dense(units = 64, activation = "tanh", 
                bias_regularizer = regularizer_l2(0.25)) %>%
    layer_dropout(rate=0.4)%>%
    
    layer_dense(units = 10, activation = "tanh", 
                bias_regularizer = regularizer_l2(0.25)) %>%
    layer_dropout(rate = 0.4)%>%
    layer_dense(units = 1, activation = "linear")
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(lr=0.01),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()
model %>% summary()



# TRAINING THE MODEL

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 300

# Fit the model and store training stats
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  #verbose = 0,
  callbacks = list(print_dot_callback)
)


# PLOTTING THE RESULTS

library(ggplot2)

#plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
#  coord_cartesian(ylim = c(0, 5))

plot(history, metrics = "mean_absolute_error", smooth = FALSE) 


#This graph shows little improvement in the model after about 150 epochs. 
#Let's update the fit method to automatically stop training when the validation 
#score doesn't improve. We'll use a callback that tests a training condition for every epoch. 
#If a set amount of epochs elapses without showing improvement, 
#it automatically stops the training.


# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 50)

model <- build_model()
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)

plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(xlim = c(0, 300), ylim = c(5, 18)) #



# Let's see how did the model performs on the test set:

c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 0))

paste0("Mean absolute error on test set:", sprintf("%.2f", mae),"%")

model %>% evaluate(test_data, test_labels)


#
# PREDICT 
# Predict some Gap Fraction using data in the testing set:

test_predictions <- model %>% predict(test_data)
test_predictions[ , 1]

mean((test_labels-test_predictions)^2)

plot(test_predictions,test_labels)



#---------------------------------------------------------
#------------------------------------------------------------
# MODEL IMPROVEMENT Three

# CREATING THE MODEL


build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 128, activation = "relu", kernel_initializer = "uniform",
                bias_regularizer = regularizer_l2(0.05),
                input_shape = dim(train_data)[2]) %>%
    layer_dropout(rate=0.8)%>%
    layer_dense(units = 64, activation = "relu", kernel_initializer = "uniform",
                bias_regularizer = regularizer_l2(0.05)) %>%
    layer_dropout(rate=0.4)%>%
    
    layer_dense(units = 10, activation = "relu", kernel_initializer = "uniform",
                bias_regularizer = regularizer_l2(0.05)) %>%
    layer_dropout(rate = 0.2)%>%
    layer_dense(units = 1, kernel_initializer = "uniform")
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(lr=0.001),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()
model %>% summary()



# TRAINING THE MODEL

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 300

# Fit the model and store training stats
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  #verbose = 0,
  callbacks = list(print_dot_callback)
)


# PLOTTING THE RESULTS

library(ggplot2)

#plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
#  coord_cartesian(ylim = c(0, 5))

plot(history, metrics = "mean_absolute_error", smooth = FALSE) 


#This graph shows little improvement in the model after about 150 epochs. 
#Let's update the fit method to automatically stop training when the validation 
#score doesn't improve. We'll use a callback that tests a training condition for every epoch. 
#If a set amount of epochs elapses without showing improvement, 
#it automatically stops the training.


# The patience parameter is the amount of epochs to check for improvement.
#early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

#model <- build_model()
#history <- model %>% fit(
#  train_data,
#  train_labels,
#  epochs = epochs,
#  validation_split = 0.2,
#  verbose = 0,
#  callbacks = list(early_stop, print_dot_callback)
#)

#plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
#  coord_cartesian(xlim = c(0, 300), ylim = c(5, 18)) #



# Let's see how did the model performs on the test set:

c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 0))

paste0("Mean absolute error on test set:", sprintf("%.2f", mae),"%")

model %>% evaluate(test_data, test_labels)


#
# PREDICT 
# Predict some Gap Fraction using data in the testing set:

test_predictions <- model %>% predict(test_data)
test_predictions[ , 1]

mean((test_labels-test_predictions)^2)

plot(test_predictions,test_labels)




#------------------------------------------------------
#SIMPLE MODEL

# CREATING THE MODEL


build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 100, activation = "relu",
                input_shape = dim(train_data)[2]) %>%
    #layer_dropout(rate=0.2)%>%
    layer_dense(units = 5, activation = "relu") %>%
    #layer_dropout(rate=0.2)%>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(lr=0.001),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()
model %>% summary()



# TRAINING THE MODEL

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 100

# Fit the model and store training stats
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  batch_size = 32,
  validation_split = 0.2,
  #verbose = 0,
  callbacks = list(print_dot_callback)
)


# PLOTTING THE RESULTS

library(ggplot2)

#plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
#  coord_cartesian(ylim = c(0, 5))

plot(history, metrics = "mean_absolute_error", smooth = FALSE) 


#This graph shows little improvement in the model after about 150 epochs. 
#Let's update the fit method to automatically stop training when the validation 
#score doesn't improve. We'll use a callback that tests a training condition for every epoch. 
#If a set amount of epochs elapses without showing improvement, 
#it automatically stops the training.


# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 50)

model <- build_model()
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)

plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 16)) #



# Let's see how the model performed on the test set:

c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 0))

paste0("Mean absolute error on test set:", sprintf("%.2f", mae),"%")

model %>% evaluate(test_data, test_labels)










