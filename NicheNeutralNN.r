
library(keras)
library(EBImage)
library(tidyverse)
library(sp)

setwd("C:/Users/tino_/Google Drive/Studium/Bioinformatik/9. Semester (Master)/Modelling Species Distribution and Biodiversity Patterns/Week 3-5/NicheVNeutral")
if(!exists("sim.neutral") && !exists("sim.niche")) load("data/sim.results.Rdata")

######################## HELPERS
mapRange <- function(x, a1, a2, b1, b2)
{
  return(round(b1 + (x-a1)*(b2-b1) / (a2-a1)))
}

######################## DATA PREP
training3D <- array(NA, c(1600, 110, 110)) # 800 neutral + 800 niche
test3D <- array(NA, c(400, 110, 110)) # 200 neutral + 200 niche
for(i in 1:length(sim.neutral))
{
  temp <- sim.neutral[[i]]
  temp$spr1[is.na(temp$spr1)] <- 0
  
  data <- mapRange(temp$spr1, min(temp$spr1), max(temp$spr1), 0, 255)

  if(i < 801) training3D[i, , ] <- matrix(data, 110, 110)
  else test3D[i-800, , ] <- matrix(data, 110, 110)
}

for(i in 1:length(sim.niche))
{
  temp <- sim.niche[[i]]
  temp$spr1[is.na(temp$spr1)] <- 0
  
  data <- mapRange(temp$spr1, min(temp$spr1), max(temp$spr1), 0, 255)

  if(i < 801) training3D[800+i, , ] <- matrix(data, 110, 110)
  else test3D[200 + i-800, , ] <- matrix(data, 110, 110)
}

# shuffle data
shuffleIndices <- sample(1:1600)
training3D <- training3D[shuffleIndices,,]

# reshape
training3D <- array_reshape(training3D, c(nrow(training3D), 110*110))
test3D <- array_reshape(test3D, c(nrow(test3D), 110*110))
training3D <- training3D / 255
test3D <- test3D / 255


trainingLabels <- c(rep(1, 800), rep(0, 800))
trainingLabels <- trainingLabels[shuffleIndices]

testLabels <- c(rep(1, 200), rep(0, 200))

# encode vectors into binary class matrices
trainingLabels <- to_categorical(trainingLabels, 2)
testLabels <- to_categorical(testLabels, 2)


model <- keras_model_sequential()
model %>% 
  layer_dense(units = 2048, activation = 'relu', input_shape = c(110*110)) %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 1024, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 512, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  training3D, trainingLabels, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

# meh
model %>% evaluate(test3D, testLabels)
model %>% predict_classes(test3D)


################################## CONVOlUTIONAL APPROACH

modelConv <- keras_model_sequential()
modelConv %>%
  layer_conv_2d(filter=32, kernel_size=c(5, 5), padding="same", input_shape=c(110, 110, 1) ) %>%  
  layer_activation("relu") %>%  
  layer_conv_2d(filter=32, kernel_size=c(5, 5), padding="same") %>%  
  layer_activation("relu") %>% 
  layer_max_pooling_2d(pool_size=c(4, 4)) %>%
  layer_conv_2d(filter=32 , kernel_size=c(5, 5),padding="same") %>% 
  layer_activation("relu") %>%
  layer_conv_2d(filter=32,kernel_size=c(5, 5) ) %>%  
  layer_activation("relu") %>%  
  layer_max_pooling_2d(pool_size=c(4, 4)) %>%  
  layer_dropout(0.25) %>%
  layer_flatten() %>%  
  layer_dense(512) %>%  
  layer_activation("relu") %>%  
  layer_dropout(0.5) %>%  
  layer_dense(2) %>%  
  layer_activation("softmax") 

modelConv %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- modelConv %>% fit(
  array_reshape(training3D, c(1600, 110, 110, 1)), trainingLabels, 
  epochs = 2, batch_size = 64, verbose=1
)

modelConv %>% evaluate(array_reshape(test3D, c(400, 110, 110, 1)), testLabels)
modelConv %>% predict_classes(array_reshape(test3D, c(400, 110, 110, 1)))

############################################## ZEUGS ####################################################
test <- sim.niche[[4]]
test$spr1[is.na(test$spr1)] <- 0

image <- Image(mapRange(test$spr1, min(test$spr1), max(test$spr1), 0, 255), c(110, 110), "Grayscale")
image(image)

spec <- as.factor(test$spr1)
sp.distr <- SpatialPixelsDataFrame(coordinates(test[, 1:2]), as.data.frame(spec))
spplot(sp.distr, col.regions=rainbow(length(unique(spec))))

m1 <- matrix(c(1,2,3,4,5,6), 2, 3)
m1v <- as.vector(t(m1))
m2 <- matrix(c(11, 22, 33, 44, 55, 66), 2, 3)
m2v <- as.vector(t(m2))

m3 <- array(c(m1v, m2v), c(2, 3, 2))

