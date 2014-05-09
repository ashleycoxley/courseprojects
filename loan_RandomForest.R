library(ggplot2)
library(randomForest)

data <- read.csv(file.choose())
data <- data[,-1]

# plot example
ggplot(data, aes(x=X1, fill=as.factor(X0))) + geom_bar()

# remove noisy columns
datatrim <- data[,-c(2, 8, 10, 11, 13, 14, 16, 18, 21)]
head(datatrim)

# change certain variables to factors
datatrim$X5 <- as.numeric(datatrim$X5)
datatrim$X5 <- as.factor(datatrim$X5)

datatrim$X0 <- as.factor(datatrim$X0)

# split into testing/training
sub <- sample(nrow(datatrim), floor(nrow(datatrim) * 0.9))
training <- datatrim[sub, ]
testing <- datatrim[-sub, ]

# process data
for (n in names(training)){
  col = training[,n]
  na = is.na(col)
  if (any(na)){
    if (is.numeric(col)){
      print(paste("*",n))
      training[,paste(n,"NA",sep="_")]=na
      training[na,n]=median(col,na.rm=T)
    }else{
      print(paste(".",n))
      training[na,n] = '#NA#'
    }
  }
  if (nlevels(col)>32){
    print(paste("---",n))
    training[,n] =unclass(col)
  }
}

for (n in names(testing)){
  col =testing[,n]
  na= is.na(col)
  if (any(na)){
    if (is.numeric(col)){
      print(paste("*",n))
      testing[,paste(n,"NA",sep="_")]=na
      testing[na,n]=median(col,na.rm=T)
    }else{
      print(paste(".",n))
      testing[na,n] = '#NA#'
    }
  }
  if (nlevels(col)>32){
    print(paste("---",n))
    testing[,n] =unclass(col)
  }
}

# remove NAs missed by jeremy's code
training$X5 <- ifelse(training$X5 == '\\N', '14', training$X5)
training$X5 <- as.factor(training$X5)

# train the model
best_so_far <- randomForest(X0 ~ ., data=training, ntree=250, nodesize=6, sampsize=10000)
other <- randomForest(X0 ~ X2 + X3 + X4 + X5 + X6 + X8 + X11 + X16 + X18 + X19 + X21 + X22 + X23 + X24 + X25 + X26, data=training, ntree=250, nodesize=6, sampsize=10000)

# self-testing
testing_target <- testing$X0
testing <- testing[,-1]

self_test_predictions <- predict(best_so_far, testing)
table(self_test_predictions, testing_target)

# read in true testing and clean
raw_test <- read.csv(file.choose(), header=F)
colnames(raw_test) <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 'X15', 'X16', 
                       'X17', 'X18', 'X19', 'X20', 'X21', 'X22', 'X23', 'X24', 'X25', 'X26')

# clean
test <- raw_test[,-c(1, 7, 9, 10, 12, 13, 15, 17, 20)]
test$X5 <- as.numeric(test$X5)
test$X5 <- as.factor(test$X5)

# NA process
for (n in names(test)){
  col = test[,n]
  na = is.na(col)
  if (any(na)){
    if (is.numeric(col)){
      print(paste("*",n))
      test[,paste(n,"NA",sep="_")]=na
      test[na,n]=median(col,na.rm=T)
    }else{
      print(paste(".",n))
      test[na,n] = '#NA#'
    }
  }
  if (nlevels(col)>32){
    print(paste("---",n))
    test[,n] =unclass(col)
  }
}

# combine & split to match factor levels
training_for_combo <- training[,-c(1,14)]
nrow(training_for_combo)
combination <- rbind(training_for_combo, test)

training <- combination[1:226027,]
training <- cbind(data[1], training)
test <- combination[226027:nrow(combination),]

# predict using model
predictions <- predict(best_so_far, test)

