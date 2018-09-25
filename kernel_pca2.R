## kernel PCA it works on non linearly separable data
# Load the data set
dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[3:5]
#  Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
# Spiltting the data set into training and test data sets
set.seed(123)
library(caTools)
split <- sample.split(dataset$Purchased, SplitRatio = 0.8)
training_set <- subset(dataset, split== TRUE)
testing_set <- subset(dataset, split == FALSE)

# Feature scaling of the training and test data sets
training_set[,-3]<- scale(training_set[,-3])
testing_set[,-3]<- scale(testing_set[,-3])

# Kernel PCA
#install.packages("kernlab)
library(kernlab)
kernpca <- kpca(~., data= training_set[-3], kernel = "rbfdot", features = 2)
# Training set
training_set_pca <-as.data.frame(predict(kernpca,  training_set))
# after dimensioanlity reduction we see that there are only two variables left. 
#We need to add purchase column to this training_set_pca
training_set_pca$Purchased <- training_set$Purchased
# Testset
testing_set_pca <- as.data.frame(predict(kernpca, testing_set))
# after dimensioanlity reduction we see that there are only two variables left. 
#We need to add purchase column to this testing_set_pca
testing_set_pca$Purchased <- testing_set$Purchased

# Fitting logistic Regression Model to training set

classifier <- glm(formula = Purchased ~., family = binomial, data = training_set_pca)
prob_pred<- predict(classifier, type = 'response', newdata= testing_set_pca[-3])
y_pred <- ifelse(prob_pred >0.5, 1,0)
y_pred
# Confusion Matrix
cm <- table(testing_set_pca[,3],y_pred)
cm
# Accuracy 81.25%
(44+21)/80

# Visualize the training set
library(ElemStatLearn)
set <- training_set_pca
X1 <- seq(min(set[,1])+1, max(set[,1])-1, by =.01)
X2 <- seq(min(set[,2])+1, max(set[,2])-1, by = .01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set)<- c("V1","V2")
prob_set<- predict(classifier, type = "response",newdata = grid_set)
y_grid <- ifelse(prob_set >0.5,1,0)
plot(set[,-3],
     main = "Classifier(training set)",
     xlab = "V1",ylab= "V2",
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1),length(X2)))
points(grid_set,pch ='.', col = ifelse(y_grid ==2, "deepskyblue",ifelse(y_grid ==1,"springgreen3","tomato")))
points(set, pch =21, bg = ifelse(set[,3]==2, "blue3",ifelse(set[,3]==1,"green4","red3")))
                                                                       
# Visualize the test set
library(ElemStatLearn)
set <- testing_set_pca
X1 <- seq(min(set[,1])+1, max(set[,1])-1, by =.01)
X2 <- seq(min(set[,2])+1, max(set[,2])-1, by = .01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set)<- c("V1","V2")
prob_set<- predict(classifier, type = "response",newdata = grid_set)
y_grid <- ifelse(prob_set >0.5,1,0)
plot(set[,-3],
     main = "Classifier(training set)",
     xlab = "V1",ylab= "V2",
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1),length(X2)))
points(grid_set,pch ='.', col = ifelse(y_grid ==2, "deepskyblue",ifelse(y_grid ==1,"springgreen3","tomato")))
points(set, pch =21, bg = ifelse(set[,3]==2, "blue3",ifelse(set[,3]==1,"green4","red3")))





