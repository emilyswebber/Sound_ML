### Install Packages and set directory
library(readr) 
library(tuneR)
library(soundgen)
library(seewave)
library(stringr)
library(caret)

directory <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/"
setwd(directory)

### Read CSV files
traintest <- read.csv("train_test_split.csv")


### Train

### Read wav files
mydir_cat_train <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/train/cat"
mydir_dog_train <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/train/dog"

#### Cat Files
fnam=file.path(mydir_cat_train)
filist=list.files(fnam, recursive=TRUE, pattern="wav")
filist1=paste(fnam, "/", filist, sep="")
nfiles=length(filist1)

wav_analyze_cat_train <- analyzeFolder(mydir_cat_train)

#### Dog Files
fnam=file.path(mydir_dog_train)
filist=list.files(fnam, recursive=TRUE, pattern="wav")
filist1=paste(fnam, "/", filist, sep="")
nfiles=length(filist1)

wav_analyze_dog_train <- analyzeFolder(mydir_dog_train)

### Bind training files together
wav_analyze_cat_train$group <- "cat"
wav_analyze_dog_train$group <- "dog"
train_cat_dog <- rbind(wav_analyze_cat_train, wav_analyze_dog_train)

emptycols <- sapply(train_cat_dog, function (k) all(is.na(k)))
train_cat_dog <- train_cat_dog[!emptycols]
train_cat_dog$sound <- as.character(train_cat_dog$sound)
train_groups <- train_cat_dog$sound
train_cat_dog$sound <- NULL


#**********************
### Modeling

library(caret)

set.seed(825)
gbmFit1 <- train(group ~ ., data = train_cat_dog, 
                 method = "gbm", 
                 verbose = FALSE,
                 na.action = na.pass)
gbmFit1


### Get Test Data
          

mydir_cat_test <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/test/cats"
mydir_dog_test <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/test/dogs"

#### Cat Files
fnam=file.path(mydir_cat_train)
filist=list.files(fnam, recursive=TRUE, pattern="wav")
filist1=paste(fnam, "/", filist, sep="")
nfiles=length(filist1)

wav_analyze_cat_test<- analyzeFolder(mydir_cat_test)

#### Dog Files
fnam=file.path(mydir_dog_test)
filist=list.files(fnam, recursive=TRUE, pattern="wav")
filist1=paste(fnam, "/", filist, sep="")
nfiles=length(filist1)

wav_analyze_dog_test <- analyzeFolder(mydir_dog_test)

### Bind training files together

test_cat_dog <- rbind(wav_analyze_cat_test, wav_analyze_dog_test)

emptycols <- sapply(test_cat_dog, function (k) all(is.na(k)))
test_cat_dog <- test_cat_dog[!emptycols]
test_cat_dog$sound <- as.character(test_cat_dog$sound)
testing_groups <- test_cat_dog$sound 
test_cat_dog$sound <- NULL


### Preditions
pred <- predict(gbmFit1, test_cat_dog)

testing_groups <- as.data.frame(testing_groups)
testing_groups$pred <- pred

testing_groups$actual <- substr(testing_groups$testing_groups, start = 1, stop = 3)
testing_groups$actual <- as.factor(testing_groups$actual)

confusionMatrix(testing_groups$actual, testing_groups$pred)


cat <- readWave("/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/train/cat/cat_10.wav")
spectro(cat)

### Dog Voclalization Spectrogram
dog <- readWave("/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/train/dog/dog_barking_10.wav")
spectro(dog)


### Spectro folder

spectrogramFolder(mydir_cat_train, htmlPlots = TRUE, verbose = TRUE, step = NULL, overlap = 50, wn = "gaussian",
                  zp = 0, ylim = NULL, osc = TRUE, xlab = "Time, ms",
                  ylab = "kHz", width = 900, height = 500, units = "px",
                  res = NA)

spectrogramFolder(mydir_dog_train, htmlPlots = TRUE, verbose = TRUE, step = NULL, overlap = 50, wn = "gaussian",
                  zp = 0, ylim = NULL, osc = TRUE, xlab = "Time, ms",
                  ylab = "kHz", width = 900, height = 500, units = "px",
                  res = NA)


spectrogramFolder(mydir_cat_test, htmlPlots = TRUE, verbose = TRUE, step = NULL, overlap = 50, wn = "gaussian",
                  zp = 0, ylim = NULL, osc = TRUE, xlab = "Time, ms",
                  ylab = "kHz", width = 900, height = 500, units = "px",
                  res = NA)

spectrogramFolder(mydir_dog_test, htmlPlots = TRUE, verbose = TRUE, step = NULL, overlap = 50, wn = "gaussian",
                  zp = 0, ylim = NULL, osc = TRUE, xlab = "Time, ms",
                  ylab = "kHz", width = 900, height = 500, units = "px",
                  res = NA)

### Image Modeling

### Image folders
mydir_train_spec <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/train/train_spec"
mydir_test_spec <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/train/test_spec"

library(keras)
library(tensorflow)

# optional data augmentation
train_data_gen = image_data_generator(
  rescale = 1/255 #,
  #rotation_range = 40,
  #width_shift_range = 0.2,
  #height_shift_range = 0.2,
  #shear_range = 0.2,
  #zoom_range = 0.2,
  #horizontal_flip = TRUE,
  #fill_mode = "nearest"
)

# Validation data shouldn't be augmented! But it should also be scaled.
valid_data_gen <- image_data_generator(
  rescale = 1/255
)  

pets <- c("cat", "dog")

# number of output classes (i.e. fruits)
output_n <- length(pets)

# image size to scale down to (original images are 100 x 100 px)
img_width <- 20
img_height <- 20
target_size <- c(img_width, img_height)

# RGB = 3 channels
channels <- 3

# training images
train_image_array_gen <- flow_images_from_directory(mydir_train_spec, 
                                                    target_size = target_size,
                                                    class_mode = "categorical",
                                                    classes = pets,
                                                    seed = 42)

# validation images
valid_image_array_gen <- flow_images_from_directory(mydir_test_spec, 
                                                    target_size = target_size,
                                                    class_mode = "categorical",
                                                    classes = pets,
                                                    seed = 42)


cat("Number of images per class:")
## Number of images per class:
table(factor(train_image_array_gen$classes))

cat("\nClass label vs index mapping:\n")
## Class label vs index mapping:
train_image_array_gen$class_indices

pet_classes_indices <- train_image_array_gen$class_indices
save(pet_classes_indices, file = "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/pet_indices.RData")

# number of training samples
train_samples <- train_image_array_gen$n
# number of validation samples
valid_samples <- valid_image_array_gen$n

# define batch size and number of epochs
batch_size <- 32
epochs <- 10


# initialise model
model <- keras_model_sequential()

# add layers
model %>%
  layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same", input_shape = c(img_width, img_height, channels)) %>%
  layer_activation("relu") %>%
  
  # Second hidden layer
  layer_conv_2d(filter = 16, kernel_size = c(3,3), padding = "same") %>%
  layer_activation_leaky_relu(0.5) %>%
  layer_batch_normalization() %>%
  
  # Use max pooling
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%
  
  # Flatten max filtered output into feature vector 
  # and feed into dense layer
  layer_flatten() %>%
  layer_dense(100) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  
  # Outputs from dense layer are projected onto output layer
  layer_dense(output_n) %>% 
  layer_activation("softmax")

# compile
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.0001, decay = 1e-6),
  metrics = "accuracy"
)

# fit
hist <- model %>% fit_generator(
  # training data
  train_image_array_gen,
  
  # epochs
  steps_per_epoch = as.integer(train_samples / batch_size), 
  epochs = epochs, 
  
  # validation data
  validation_data = valid_image_array_gen,
  validation_steps = as.integer(valid_samples / batch_size),
  
  # print progress
  verbose = 2,
  callbacks = list(
    # save best model after every epoch
    callback_model_checkpoint("/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/pets_checkpoints.h5", save_best_only = TRUE),
    # only needed for visualising with TensorBoard
    callback_tensorboard(log_dir = "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/")
  )
)













### OTHER WAY
mydir_cat_train_spec <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/train/cat_spec"
mydir_dog_train_spec <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/train/dog_spec"

mydir_cat_test_spec <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/test/cats_spec"
mydir_dog_test_spec <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/test/dogs_spec"


#### Train Files
fnam=file.path(mydir_cat_train_spec)
filist=list.files(fnam, recursive=TRUE, pattern="png")
filist1=paste(fnam, "/", filist, sep="")
nfiles=length(filist1)

cat_train_spec_fname <- filist1

fnam=file.path(mydir_dog_train_spec)
filist=list.files(fnam, recursive=TRUE, pattern="png")
filist1=paste(fnam, "/", filist, sep="")
nfiles=length(filist1)

dog_train_spec_fname <- filist1


fnam=file.path(mydir_cat_test_spec)
filist=list.files(fnam, recursive=TRUE, pattern="png")
filist1=paste(fnam, "/", filist, sep="")
nfiles=length(filist1)

cat_test_spec_fname <- filist1

fnam=file.path(mydir_dog_test_spec)
filist=list.files(fnam, recursive=TRUE, pattern="png")
filist1=paste(fnam, "/", filist, sep="")
nfiles=length(filist1)

dog_test_spec_fname <- filist1


### Bind train and test sets and label

train_spec <- c(cat_train_spec_fname, dog_train_spec_fname)
test_spec <- c(cat_test_spec_fname, dog_test_spec_fname)

train_spec2 <- as.data.frame(train_spec)
train_spec2$class[grepl('dog',train_spec)] <- "dog"
train_spec2$class[grepl('cat',train_spec)] <- "cat"

test_spec2 <- as.data.frame(test_spec)
test_spec2$class[grepl('dog',test_spec2)] <- "dog"
test_spec2$class[grepl('cat',test_spec2)] <- "cat"

