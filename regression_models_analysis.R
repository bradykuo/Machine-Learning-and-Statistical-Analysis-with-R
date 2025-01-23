# Image 1: 載入套件和資料
library(car)
library(MASS)
library(lmtest)

data(UScrime)
UScrime$So=factor(UScrime$So)
summary(UScrime)
scatterplotMatrix(UScrime)

# Image 2: 線性迴歸分析和診斷檢定
reg=lm(y~.,data=UScrime)
summary(reg)
residualPlots(reg)
raintest(y~.,data=UScrime)
qqnorm(residuals(reg));qqline(residuals(reg))
shapiro.test(residuals(reg))
durbinWatsonTest(reg)
bptest(reg)
outlierTest(reg)
vif(reg)

# Image 3: 羅吉斯迴歸分析
data(Pima.tr)
data(Pima.te)

LG = glm(type~., data=Pima.tr, family=binomial)
summary(LG)

# 訓練集預測
pre_LG = predict(LG, Pima.tr, type = "response")

# 測試不同閾值
acc = numeric(9)
thresholds = seq(0.1, 0.9, 0.1)

for(i in 1:9) {
  t = thresholds[i]
  pre = ifelse(pre_LG >= t, 1, 0)
  tab = table(Pima.tr$type, pre)
  acc[i] = sum(diag(tab))/nrow(Pima.tr)
}

# 找出最佳閾值
t = thresholds[which.max(acc)]

# 測試集預測
pre_LG = predict(LG, Pima.te, type = "response")
pre = ifelse(pre_LG >= t, 1, 0)
tab = table(Pima.te$type, pre)
test_acc = sum(diag(tab))/nrow(Pima.te)

# 印出結果
coef(LG)
summary(LG)$coefficients[,4]
AIC(LG)
data.frame(threshold = thresholds, accuracy = acc)
cat("Best threshold:", t, "\n")
print(tab)
cat("Test accuracy:", test_acc)