###Linear Discriminant Analysis, this technique works on Linear data
### From the n independent variables of your dataset, LDA extracts p <= n new 
###independent variables that separate the most of the classes of the dependent variable.
###The fact that the Dv is considered makes LDA a supervised model.
dataset <- read.csv("Wine.csv")
str(dataset)
#dataset$Customer_Segment <- factor(dataset$Customer_Segment, levels =c(1,2,3))
# Splitiing the dataset into training and test data set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
testing_set <- subset(dataset, split ==FALSE)

# Feature Scaling this is very imp for this PCA 
training_set[,-14]<- scale(training_set[,-14])
testing_set[,-14]<- scale(testing_set[,-14])

#Applying LDA
library(MASS)
lda <- lda(Customer_Segment~.,
           data = training_set)
training_set <- as.data.frame(predict(lda, newdata= training_set))
# Correct order of Indexes 
training_set <- training_set[c(5,6,1)]
# Extract feature in testing set as well
testing_set <- as.data.frame(predict(lda, newdata = testing_set))
# Correct order and right variable in the dataframe
testing_set <- testing_set[c(5,6,1)]
# Build a SVM model to training set
library(e1071)
classifier <- svm(formula = class~.,
                  data = training_set,
                  type = "C-classification",
                  kernel = "linear")    
# prediciting the test results
y_pred <- predict(classifier, newdata =testing_set[-3])
y_pred
cm <- table(testing_set[,3], y_pred)
# Accuracy is 97.22%
cm
(12+13+10)/36
names(testing_set)
# Visualize the training set results
set <- training_set
X1 <- seq(min(set[,1])+1, max(set[,1])-1, by = .01)
X2 <- seq(min(set[,2])+1, max(set[,2])-1, by = .01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) <- c("x.LD1","x.LD2")
y_grid <- predict(classifier, newdata = grid_set)
plot(set[,-3],
          main = "Classifier(training set)",
          xlab = "xLD1", ylab = "xLD2",
           xlim = range(X1),ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2,"deepskyblue", ifelse(y_grid ==1,'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, "blue3", ifelse(set[,3]==1 ,'green4', 'red3')))








