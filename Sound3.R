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
library(EBImage)
library(keras)

### Image folders
mydir_train_spec_cat <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/train/cat_spec"

fnam=file.path(mydir_train_spec_cat)
filist=list.files(fnam, recursive=TRUE, pattern="png")
filist1=paste(fnam, "/", filist, sep="")
nfiles=length(filist1)

train_cat <- lapply(filist1, readImage, "png")


mydir_train_spec_dog <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/train/dog_spec"

fnam=file.path(mydir_train_spec_dog)
filist=list.files(fnam, recursive=TRUE, pattern="png")
filist1=paste(fnam, "/", filist, sep="")
nfiles=length(filist1)

train_dog <- lapply(filist1, readImage, "png")


mydir_test_spec_cat <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/test/cat_specs"

fnam=file.path(mydir_test_spec_cat)
filist=list.files(fnam, recursive=TRUE, pattern="png")
filist1=paste(fnam, "/", filist, sep="")
nfiles=length(filist1)

test_cat <- lapply(filist1, readImage, "png")


mydir_test_spec_dog <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/test/dog_specs"

fnam=file.path(mydir_test_spec_dog)
filist=list.files(fnam, recursive=TRUE, pattern="png")
filist1=paste(fnam, "/", filist, sep="")
nfiles=length(filist1)

test_dog <- lapply(filist1, readImage, "png")

### Histograms

hist(train_cat[[5]])

### Resize 

train_cat2 <- lapply(train_cat, resize, 28, 28) 
train_dog2 <- lapply(train_dog, resize, 28, 28) 
test_cat2 <- lapply(test_cat, resize, 28, 28) 
test_dog2 <- lapply(test_dog, resize, 28, 28) 

#train_cat2 <- lapply(train_cat, array_reshape, 28, 28, 4) 
#train_dog2 <- lapply(train_dog, array_reshape, c(28, 28, 4))
#test_cat2 <- lapply(test_cat, array_reshape, c(28, 28, 4))
#test_dog2 <- lapply(test_dog, array_reshape, c(28, 28, 4)) 


### Function
library(pbapply)
library(magick)
width <- 28
height <- 28
img_size <- width*height

mydir_test_spec <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/test/test_specs"
mydir_train_spec <- "/Users/emily.webber/Dropbox/Website Dropbox 2/Sound2/train/train_spec"


fnam=file.path(mydir_test_spec)
filist=list.files(fnam, recursive=TRUE, pattern="png")
filist1=paste(fnam, "/", filist, sep="")
nfiles=length(filist1)

test <- lapply(filist1, readImage, "png")


fnam=file.path(mydir_train_spec)
filist=list.files(fnam, recursive=TRUE, pattern="png")
filist1=paste(fnam, "/", filist, sep="")
nfiles=length(filist1)

train <- lapply(filist1, readImage, "png")




image_names <- list.files(mydir_train_spec)
image_names2 <- as.data.frame(image_names) 
image_names2$label <- ifelse(grepl("cat", image_names2$image_names), "cat", "dog")

img <- lapply(filist1, readImage, "png")
img_resized <- lapply(img, resize, w = width, h = height)
grayimg <- channel(img_resized, "gray")
img_matrix <- grayimg@.Data
img_vector <- as.vector(t(img_matrix))
   

feature_list <- pblapply(images_names, function(imgname) {
  ## Read image
  img <- readImage(file.path(mydir_train_spec, imgname))
  ## Resize image
  img_resized <- resize(img, w = width, h = height)
  ## Set to grayscale
  grayimg <- channel(img_resized, "gray")
  ## Get the image as a matrix
  img_matrix <- grayimg@.Data
  ## Coerce to a vector
  img_vector <- as.vector(t(img_matrix))
  return(img_vector)
})

## bind the list of vector into matrix
feature_matrix <- do.call(rbind, feature_list)
feature_matrix <- as.data.frame(feature_matrix)
## Set names
names(feature_matrix) <- paste0("pixel", c(1:img_size))
if (add_label) {
  ## Add label
  feature_matrix <- cbind(label = image_names2$label, feature_matrix)
}
return(feature_matrix)


  ## bind the list of vector into matrix
  feature_matrix <- do.call(rbind, feature_list)
  feature_matrix <- as.data.frame(feature_matrix)
  ## Set names
  names(feature_matrix) <- paste0("pixel", c(1:img_size))
  if (add_label) {
    ## Add label
    feature_matrix <- cbind(label = label, feature_matrix)
  }
  return(feature_matrix)
}



cats_data <- extract_feature(dir_path = mydir_train_spec, width = width, height = height)
dogs_data <- extract_feature(dir_path = mydir_train_spec, width = width, height = height, is_cat = FALSE)


cats_data_test <- extract_feature(dir_path = mydir_test_spec, width = width, height = height)
dogs_data_test<- extract_feature(dir_path = mydir_test_spec, width = width, height = height, is_cat = FALSE)
dim(cats_data_test)
dim(dogs_data_test)





saveRDS(cats_data, "cat.rds")
saveRDS(dogs_data, "dog.rds")


library(caret)
## Bind rows in a single dataset
train_set <- rbind(cats_data, dogs_data)
test_set <- rbind(cats_data_test, dogs_data_test)

## Fix train and test datasets
train_data <- data.matrix(train_set)
train_x <- t(train_data[, -1])
train_y <- train_data[,1]
train_array <- train_x
dim(train_array) <- c(28, 28, 1, ncol(train_x))

test_data <- data.matrix(test_set)
test_x <- t(test_set[,-1])
test_y <- test_set[,1]
test_array <- test_x
dim(test_array) <- c(28, 28, 1, ncol(test_x))

