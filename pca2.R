## Principal component Analysis reduction technique this works on Linear data
# From the m independent variables of your dataset, PCA extracts p<= m new independent
# variables that explains the most variance of the dataset regardless of the dependent variable
# the fact that the DV is not considered makes PCA an unsupervisedmodel
# Import dataset
# All variables in the datase from alcohol to proline are the independent variable except for customer segment
dataset <- read.csv("Wine.csv")
str(dataset)
dataset$Customer_Segment <- factor(dataset$Customer_Segment, levels =c(1,2,3))
# Splitiing the dataset into training and test data set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
testing_set <- subset(dataset, split ==FALSE)

# Feature Scaling this is very imp for this PCA 
training_set[,-14]<- scale(training_set[,-14])
testing_set[,-14]<- scale(testing_set[,-14])

# Applying PCA
#install.packages("caret")
library(caret)
library(e1071)
# transform our original dataset into new dataset with new extracted feature
# pcacomp =2 means, that will transform 13 independent variables into 2 most variance variables
pca <-preProcess(x = training_set[-14], method = "pca", pcaComp = 2)

training_set <- predict(pca, training_set)
# Correct order of the indexes
training_set <- training_set[c(2,3,1)]
testing_set <- predict(pca, testing_set)
# Correct order of the indexes
testing_set <- testing_set[c(2,3,1)]
# Fitting SVM to the training set
classifier <- svm(formula = Customer_Segment~., data = training_set,
                  type = 'C-classification',
                  kernel = 'linear')
# Predicting the test results
y_pred <- predict(classifier, newdata = testing_set[-3])
y_pred
cm = table(testing_set[, 3], y_pred)
# ACcuracy is 100%
cm
# Visualize the training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Classifier (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2,"deepskyblue", ifelse(y_grid ==1,'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, "blue3", ifelse(set[,3]==1 ,'green4', 'red3')))

# Visulaize the testing set result
set <- testing_set
X1 <- seq(min(set[,1])-1, max(set[,1])+1, by =.01)
X2 <- seq(min(set[,2])-1, max(set[,2])+1, by = .01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) <- c("PC1","PC2")
y_grid <- predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = "Classifier(testing set)",
     xlab = "PC1", ylab = "PC2",
     xlim = range(X1), ylim= range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add= TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid ==2 , "deepskyblue", ifelse(y_grid ==1, "springgreen3", "tomato")))
points(set, pch =21, bg = ifelse(set[,3]==2, "blue3", ifelse(set[,3]==1, 'green4',"red3")))

# Fitiing naive bayes to training set
dataset <- read.csv("Wine.csv")
str(dataset)
#### IMP for naive bayes we have to convert Customer_segment variable into factor variable
dataset$Customer_Segment <- factor(dataset$Customer_Segment, levels =c(1,2,3))
# Splitiing the dataset into training and test data set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
testing_set <- subset(dataset, split ==FALSE)

# Feature Scaling this is very imp for this PCA 
training_set[,-14]<- scale(training_set[,-14])
testing_set[,-14]<- scale(testing_set[,-14])

# Applying PCA
#install.packages("caret")
library(caret)
library(e1071)
# transform our original dataset into new dataset with new extracted feature
# pcacomp =2 means, that will transform 13 independent variables into 2 most variance variables
pca <-preProcess(x = training_set[-14], method = "pca", pcaComp = 2)
training_set <- predict(pca, training_set)
# Correct order of the indexes
training_set <- training_set[c(2,3,1)]
testing_set <- predict(pca, testing_set)
# Correct order of the indexes
testing_set <- testing_set[c(2,3,1)]
classifier <- naiveBayes(x= training_set[-3],
                         y = training_set$Customer_Segment)
y_pred <- predict(classifier, newdata = testing_set[-3])
cm <- table(testing_set[,3], y_pred)
cm
# Accuracy 94.4%
34/36
# Visualize the training set
set <- training_set
X1 <- seq(min(set[,1])+1, max(set[,1])-1, by = .01)
X2 <- seq(min(set[,2])+1, max(set[,2])-1, by = .01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set)<- c("PC1", "PC2")
y_grid <- predict(classifier, newdata= grid_set)
plot(set[,-3],
     main = "Naive Bayes(testing set)",
     xlab = "PC1", ylab = "PC2",
     xlim = range(X1), ylim= range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)))
points(grid_set, pch = '.', col = ifelse(y_grid == 2,"deepskyblue", ifelse(y_grid ==1,'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, "blue3", ifelse(set[,3]==1 ,'green4', 'red3')))
# Visualize the test set same as we did in training set