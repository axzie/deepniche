library(keras)
library(tidyverse)

# install_keras()

mnist <- dataset_mnist()

############### INPUT ################
# 3d array (images, width, height)
x_train <- mnist$train$x # 60000 images
x_test <- mnist$test$x # 10000 images

# transform to arrays of size 784=28*28 and rescale 0-255 grey values to 0-1
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
x_train <- x_train / 255
x_test <- x_test / 255


############### EXPECTED OUTPUT ################
# 1d array (0-9)
y_train <- mnist$train$y
y_test <- mnist$test$y

# encode vectors into binary class matrices
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)


############### DEFINING THE MODEL ################
# layer_dense adds a hidden layer with certain number of nodes, a given activation function and the expected size of the input
# the last layer_dense represents the output layer (in this case 10 nodes)
# layer_dropout prevents overfitting
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

############### COMPILING THE NN ################
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

############### OPTIMIZING THE NN ################
history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)


model %>% evaluate(x_test, y_test)


