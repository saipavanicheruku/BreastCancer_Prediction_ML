#The data set
cancer = read.csv("DataSet/breastcancer.csv")

#Loading the libraries and setting the random number generator seed to 23
library(class)
library(tidyverse)
library(caTools)
library(rpart)
set.seed(23)
names(cancer) = c("Sample_ID",
                  "Clump_Thickness",
                  "Uniformity_of_Cell_Size",
                  "Uniformity_of_Cell_Shape",
                  "Marginal_Adhesion",
                  "Single_Epithelial_Cell_Size",
                  "Bare_Nuclei",
                  "Bland_Chromatin",
                  "Normal_Nucleoli",
                  "Mitoses",
                  "Class")
cancer = na.omit(cancer)
cancer$Class = factor(cancer$Class,
                      levels=c(2,4),
                      labels=c("Benign","Malignant"))

str(cancer)
#Question 1 - Create a scatter plot of Clump_Thickness vs. Uniformity_of_Cell_Size, color coded by the benign or malignant nature of the cell
ggplot(cancer) +
  geom_point(aes(x = Clump_Thickness, y = Uniformity_of_Cell_Size, color = Class))

#Question 2 - Create functions that, given a confusion matrix, calculate sensitivity, specificity, accuracy and precision.

sensitivity = function(cm) 
{
  return(cm[1,1]/(cm[1,1]+cm[1,2]))
}
specificity = function(cm)
{
  return(cm[2,2]/(cm[2,1]+cm[2,2]))
}
accuracy = function(cm) 
{
  return((cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2]))
}
precision = function(cm)
{
  return(cm[1,1]/(cm[1,1]+cm[2,1]))
}

#Question 3: Separate Training and Test Data Sets
ind = sample(2, nrow(cancer), replace=TRUE, prob=c(0.67, 0.33))

#Question 4: Applying KNN
cancer.training = cancer[ind==1, 2:6]
cancer.test = cancer[ind==2, 2:6]
cancer.trainLabels = cancer[ind==1, 11]
cancer.testLabels = cancer[ind==2, 11]

prediction = knn(train = cancer.training,
                 test = cancer.test,
                 cl = cancer.trainLabels,
                 k=3)

(confusionMatrix = table(Actual_Value = cancer.testLabels,
                         Predicted_Value = prediction))

sensitivity(confusionMatrix)
specificity(confusionMatrix)
accuracy(confusionMatrix)
precision(confusionMatrix)

#Question 5: Logistic Regression
#trainingWithLabel$CancerousOrNot = cancer.trainLabels
(logisticModel = glm(cancer.trainLabels ~ Clump_Thickness +
                       Uniformity_of_Cell_Size +
                       Uniformity_of_Cell_Shape +
                       Marginal_Adhesion +
                       Single_Epithelial_Cell_Size,
                     data=cancer.training,
                     family='binomial'))

prediction = predict(logisticModel, cancer.test, type='response')
cancer.test$predicted = ifelse(prediction>0.7, TRUE, FALSE)

(confusionMatrix = table(Actual_Value = cancer.testLabels,
                         Predicted_Value = prediction>0.7))

sensitivity(confusionMatrix)
specificity(confusionMatrix)
accuracy(confusionMatrix)
precision(confusionMatrix)

#Question 6: Decision Tree
model = rpart(cancer.trainLabels ~
                Clump_Thickness +
                Uniformity_of_Cell_Size +
                Uniformity_of_Cell_Shape +
                Marginal_Adhesion +
                Single_Epithelial_Cell_Size,
              data=cancer.training,
              control=rpart.control(maxdepth=3),
              method='class')

prediction = predict(model, cancer.test, type='class')

(confusionMatrix = table(Actual_Value = cancer.testLabels,
                         Predicted_Value = prediction))

sensitivity(confusionMatrix)
specificity(confusionMatrix)
accuracy(confusionMatrix)
precision(confusionMatrix)

#Question 7 - Is there one method that us best than the others, and why?

Decision tree algorithm works better compared to the other two models.
We can make this inference by looking at the specificity values from the three models.
Getting higher TypeI Errors (false positive) i.e, 'Cancerous/Malignant' predicted as 'Healthy/Beningn' is dangerous pertaining to this data set.
So, we must look at the specificity values to determine which model predictions are suitable.
88.9, 84.7, 91.6, so clearly we can say that decision tree algorithm is better than the others.