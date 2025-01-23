library(MASS)
library(rpart)

data("Pima.tr")
summary(Pima.tr)

set.seed(1111) #設定亂數種子
cart=rpart(type~.,Pima.tr,control=rpart.control(cp=0)) #訓練CART模型
summary(cart)

par(xpd=TRUE);plot(cart);text(cart)
###############

#cp=0.1->葉節點數=3, cp=0.03->葉節點數=5
cart_prune=prune(cart, cp=0.03) 
par(xpd=TRUE);plot(cart_prune);text(cart_prune)

cart_prune=prune(cart, cp=0.1) 
par(xpd=TRUE);plot(cart_prune);text(cart_prune)
###############

# Values from the provided code
accuracy_1 <-  # Accuracy for Pima.te with cp=0
accuracy_2 <-  # Accuracy for Pima.te with cp=0.03
accuracy_3 <-  # Accuracy for Pima.te with cp=0.1
accuracy_4 <-  # Accuracy for Pima.tr with cp=0
accuracy_5 <-  # Accuracy for Pima.tr with cp=0.03
accuracy_6 <-  # Accuracy for Pima.tr with cp=0.1

#cart -> cart_prune
# Calculate accuracy_3 (Pima.te with cp=0.1)
pre <- predict(cart, Pima.te, type = "class")
confusion_matrix <- table(Type = Pima.te$type, Predict = pre)
accuracy_1 <- sum(diag(confusion_matrix) / sum(confusion_matrix))

# Calculate accuracy_2 (Pima.te with cp=0.03)
cart_prune <- prune(cart, cp = 0.03)
pre <- predict(cart_prune, Pima.te, type = "class")
confusion_matrix <- table(Type = Pima.te$type, Predict = pre)#建立預測交叉矩陣
accuracy_2 <- sum(diag(confusion_matrix) / sum(confusion_matrix))#計算正確率

# Calculate accuracy_3 (Pima.te with cp=0.1)
cart_prune <- prune(cart, cp = 0.1)
pre <- predict(cart_prune, Pima.te, type = "class")
confusion_matrix <- table(Type = Pima.te$type, Predict = pre)
accuracy_3 <- sum(diag(confusion_matrix) / sum(confusion_matrix))

#Pima.test -> Pima.train
# Calculate accuracy_4 (Pima.tr with cp=0)
pre <- predict(cart, Pima.tr, type = "class")
confusion_matrix <- table(Type = Pima.tr$type, Predict = pre)
accuracy_4 <- sum(diag(confusion_matrix) / sum(confusion_matrix))

# Calculate accuracy_5 (Pima.tr with cp=0.03)
cart_prune <- prune(cart, cp = 0.03)
pre <- predict(cart_prune, Pima.tr, type = "class")
confusion_matrix <- table(Type = Pima.tr$type, Predict = pre)
accuracy_5 <- sum(diag(confusion_matrix) / sum(confusion_matrix))

# Calculate accuracy_6 (Pima.tr with cp=0.1)
cart_prune <- prune(cart, cp = 0.1)
pre <- predict(cart_prune, Pima.tr, type = "class")
confusion_matrix <- table(Type = Pima.tr$type, Predict = pre)
accuracy_6 <- sum(diag(confusion_matrix) / sum(confusion_matrix))

# Organize accuracy values into a table
accuracy_table <- matrix(c(accuracy_4, accuracy_5, accuracy_6, accuracy_1, accuracy_2, accuracy_3 ), nrow = 2, byrow = TRUE)
colnames(accuracy_table) <- c("葉節點數為8（不修剪）", "葉節點數為5", "葉節點數為3")
rownames(accuracy_table) <- c("訓練資料", "測試資料")

library(knitr)

# Convert the accuracy table to a data frame
accuracy_df <- data.frame(accuracy_table)

# Print the formatted table with centered alignment
knitr::kable(accuracy_df, align = "c", format = "markdown")

###############

library(C50)
library(MASS)

# Load training and test data
data("Pima.tr")
data("Pima.te")

# Train C5.0 decision tree with no global pruning
C50_tree <- C5.0(type ~ ., Pima.tr, control = C5.0Control(noGlobalPruning = TRUE))
summary(C50_tree)

# Plot the decision tree
par(xpd = TRUE)
plot(C50_tree)

# Predict on test data
pre <- predict(C50_tree, Pima.te, type = "class")
confusion_matrix <- table(Type = Pima.te$type, Predict = pre)
accuracy_1 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Predict on training data
pre <- predict(C50_tree, Pima.tr, type = "class")
confusion_matrix <- table(Type = Pima.tr$type, Predict = pre)
accuracy_2 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Train C5.0 decision tree with global pruning
C50_tree <- C5.0(type ~ ., Pima.tr, control = C5.0Control(noGlobalPruning = FALSE))
summary(C50_tree)

# Plot the decision tree
par(xpd = TRUE)
plot(C50_tree)

# Predict on test data
pre <- predict(C50_tree, Pima.te, type = "class")
confusion_matrix <- table(Type = Pima.te$type, Predict = pre)
accuracy_3 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Predict on training data
pre <- predict(C50_tree, Pima.tr, type = "class")
confusion_matrix <- table(Type = Pima.tr$type, Predict = pre)
accuracy_4 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Organize accuracy values into a table
accuracy_table <- matrix(c(accuracy_2, accuracy_4, accuracy_1, accuracy_3 ), nrow = 2, byrow = TRUE)
colnames(accuracy_table) <- c("葉節點數為7（不修剪）", "葉節點數為4")
rownames(accuracy_table) <- c( "訓練資料", "測試資料")

library(knitr)

# Print the formatted table with centered alignment
knitr::kable(accuracy_table, align = "c", format = "markdown")

###############

library(CHAID)
library(MASS)

data("Pima.tr")
data("Pima.te")
Pima=rbind(Pima.tr, Pima.te)
level_name={}

for(i in 1:7){
  Pima[,i]=cut(Pima[,i], breaks=3, ordered_result=T, include.lowest=T)
  level_name<-rbind(level_name, levels(Pima[,i]))
}

level_name=data.frame(level_name)
row.names(level_name)=colnames(Pima)[1:7]
colnames(level_name)=paste("L",1:3,sep="")
level_name

###############
Pima.tr=Pima[1:200,]
Pima.te=Pima[201:nrow(Pima),]
set.seed(1111)

CHAID_tree=chaid(type~.,Pima.tr)
CHAID_tree
plot(CHAID_tree)
###############

library(MASS)
library(randomForest)

set.seed(1111)
data("Pima.tr")
data("Pima.te")

# Fit a random forest model
rf.model <- randomForest(type ~ ., data = Pima.tr)

# Make predictions on test data
pre.te <- predict(rf.model, Pima.te)

# Create confusion matrix
confusion_matrix <- table(Pima.te$type, pre.te)
confusion_matrix

# Calculate test accuracy
test_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
test_accuracy

# Tune random forest model
rftune <- tuneRF(y = Pima.tr$type, x = Pima.tr[, 1:7], ntreeTry = 500)

# Determine optimal mtry
mtry <- rftune[mtry, which.min(rftune[, 2])]

# Fit random forest model with optimal mtry
rf.model <- randomForest(type ~ ., data = Pima.tr, mtry = mtry, ntree = 500, importance = TRUE)

# Plot variable importance
varImpPlot(rf.model)

















