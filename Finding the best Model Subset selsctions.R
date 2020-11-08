#########################################################
## data Mining I HW3
## Subhashchandra Babu Madineni   UBIT = 50373860
## Created on 7 th october
## Edited: 
#########################################################
rm(list = ls())

#setwd('C:/University At Buffalo Fall 2020 Classes/EAS 506-CDA 541Stastical Data Mining/R for STA/homework_3')
#install.packages("pls")
#install.packages("ggplot2")
#install.packages("class")
#install.packages("caret")
install.packages("e1071")
library(tidyverse)
library(ggplot2)
require(class)
library(class)
library(caret)
library(pls)
library(e1071)
dataset <- iris

#########################################################
# Encoding the Categorical Variable "Private"
#########################################################
dataset$Species <- factor(dataset$Species,
                      levels = c("versicolor","virginica","setosa"),
                      labels = c(1,2,3))

#########################################################
# splitting the dataset into Train and Test sets
#########################################################

set.seed(12)
indices <- sample(nrow(dataset), .80*nrow(dataset))

Training_set <- dataset[indices,]
Test_set <- as.data.frame(dataset[-indices,])




################################################
# Building the model for knn
###############################################
y_pred = (knn(train = X_train,test = X_test,cl = Y_train,k=100))

cm_100 = table(Y_test,y_pred)
cm_100

y_pred = knn(train = X_train,
             test = X_test,
             cl = Y_train,
             k=50)
cm_50 = table(Y_test,y_pred)
heatmap(cm,scale = "none")



##########################################
# Principal Components Regression
##########################################
set.seed(232)
pca = preProcess(x = dataset[-5],method = "pca",pcaComp = 2)
dataset_pca = predict(pca,dataset)

#########################################################
# splitting the dataset into Train and Test sets for PCA
#########################################################

set.seed(12)
indices <- sample(nrow(dataset_pca), .80*nrow(dataset_pca))

Training_set_pca <- dataset_pca[indices,]
Test_set_pca <- as.data.frame(dataset_pca[-indices,])

X_Training_set_pca = Training_set_pca[,2:3]
Y_Training_set_pca = Training_set_pca[,1]

X_Test_set_pca = Test_set_pca[,2:3]
Y_Test_set_pca = Test_set_pca[,1]


#########################################################
# Applying pca on the dataset
#########################################################

y_pred_pca_3 = knn(train = X_Training_set_pca,
                   test = X_Test_set_pca,
                   cl = Y_Training_set_pca,
                   k=3)
cm_3_pca = table(Y_Test_set_pca,y_pred_pca)
cm_3_pca
heatmap(cm_3_pca,scale = "none")


#########################################################
#visualising the training results
#########################################################


set =Training_set_pca
X1 =seq(min(set[,2])-1,max(set[,2])+1,by =0.01)
X2 =seq(min(set[,3])-1,max(set[,3])+1,by=0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = c("PC1","PC2")
y_grid = knn(train = X_Training_set_pca,
             test = X_Test_set_pca,
             cl = Y_Training_set_pca,
             k=3)
plot(set[,1],
     main = "KNN classifier",
     xlab = 'PC1',
     ylab = 'PC2',
     xlim = range(X1),ylim = range(X2))
contour(X1,X2,matrix(as.numeric(y_grid), length(X1), length(X2)), add =TRUE)
points(grid_set,pch ='.',col = ifelse(y_grid ==2,'deepskyblue',ifelse(y_grid==1,'springgreen3','tomato')))
points(set,pch =21,bg = ifelse(set[,1]==2,'blue3',ifelse(set[,1]==1,'green4','red')))







