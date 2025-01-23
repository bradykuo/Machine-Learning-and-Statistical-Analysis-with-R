library(MASS)
library(rpart)

data("Pima.tr")
summary(Pima.tr)

set.seed(1111)
cart_prune <- prune(cart, cp = 0.03)
pre <- predict(cart_prune, Pima.te, type = "class")
confusion_matrix <- table(Type = Pima.te$type, Predict = pre)#建立預測交叉矩陣
CARTacc_te <- sum(diag(confusion_matrix) / sum(confusion_matrix))#計算正確率

pre <- predict(cart_prune, Pima.tr, type = "class")
confusion_matrix <- table(Type = Pima.tr$type, Predict = pre)
CARTacc_tr <- sum(diag(confusion_matrix) / sum(confusion_matrix))

summary(cart_prune)

CARTacc_te
CARTacc_tr
CART_leaf <- sum(cart_prune$frame$var == "<leaf>")
CART_leaf
CART_dep <- 

################
  
library(C50)
library(MASS)

# Load training and test data
data("Pima.tr")
data("Pima.te")
  
# Train C5.0 decision tree with global pruning
C50_tree <- C5.0(type ~ ., Pima.tr, control = C5.0Control(noGlobalPruning = FALSE))

# Predict on test data
pre <- predict(C50_tree, Pima.te, type = "class")
confusion_matrix <- table(Type = Pima.te$type, Predict = pre)
C50acc_te <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Predict on training data
pre <- predict(C50_tree, Pima.tr, type = "class")
confusion_matrix <- table(Type = Pima.tr$type, Predict = pre)
C50acc_tr <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
summary(C50_tree)

C50acc_te
C50acc_tr
C50_leaf <- C50_tree$size
C50_leaf 

################

# 建立比較項目的資料框
comparison_data <- data.frame(
  模型 = c("CART", "C5.0", "CHAID"),
  訓練資料正確率 = c(0.835, 0.815, 0.755),
  測試資料正確率 = c(0.756, 0.735, 0.789),
  葉節點數 = c(5, 4, 5),
  深度 = c(3, 3, 2),
  使用屬性 = c("葡萄糖濃度、糖尿病家族病因指數、身體質量指數", "葡萄糖濃度、糖尿病家族病因指數、身體質量指數", "葡萄糖濃度、年齡、糖尿病家族病因指數")
)

# 輸出表格並將每一欄置中對齊
knitr::kable(comparison_data, align = rep("c", ncol(comparison_data)), format = "markdown")






