library(MASS)
library(RSNNS)

data(Pima.tr)
set.seed(1111)

# 將資料順序重新排列
# 從Pima.tr中隨機抽樣並重新排列數據
Pima.tr <- Pima.tr[sample(1:nrow(Pima.tr), length(1:nrow(Pima.tr))), ]

# 提取輸入值
PimaValues <- Pima.tr[, 1:7]

# 目標屬性重新編碼
# 解碼目標屬性
PimaTargets <- decodeClassLabels(Pima.tr[, 8])

# 將數據集分為訓練集和測試集，比例為0.1
Pima.tr <- splitForTrainingAndTest(PimaValues, PimaTargets, ratio = 0.1)

# 對訓練集和測試集進行歸一化處理
Pima.tr <- normTrainingAndTestSet(Pima.tr)

# 訓練MLP模型
model<-mlp(Pima.tr$inputsTrain, Pima.tr$targetsTrain,
           size=14, learnFuncParams=0.01, maxit=100, inputsTest=Pima.tr$inputsTest,
           targetsTest=Pima.tr$targetsTest)

#size: 隱藏層神經元個數
#learnFuncParams:學習率
#maxit:最大迭代次數

# 繪製迭代錯誤曲線
plotIterativeError(model)

# 繪製權重矩陣
weightMatrix(model)

# 建立參數表格
p_table <- expand.grid(size = c(12, 13, 14, 15, 16), learning.rate = c(0.001, 0.01, 0.1))

# 計算不同參數組合的測試誤差
for (i in 1:nrow(p_table)) {
  model <- mlp(Pima.tr$inputsTrain, Pima.tr$targetsTrain,
               size = p_table[i, 1], learnFuncParams = p_table[i, 2], maxit = 100,
               inputsTest = Pima.tr$inputsTest, targetsTest = Pima.tr$targetsTest)
  p_table$TestError[i] <- model$IterativeTestError[100]
}

# 輸出參數表格
p_table

# 對測試資料進行標準化
Pima.te[, 1:7] <- normalizeData(Pima.te[, 1:7])

# 進行預測
predictions <- predict(model, Pima.te[, 1:7])

# 計算準確率
table <- confusionMatrix(Pima.te[, 8], predictions)
accuracy <- sum(diag(table)) / sum(table)
accuracy

library(reshape2)
library(knitr)

# 轉換成 5*3 的表格
reshaped_table <- reshape(p_table, direction = "wide", idvar = "size", timevar = "learning.rate")

# 設置列名和行名
colnames(reshaped_table) <- c("隱藏層神經元個數", "學習率 0.001", "學習率 0.01", "學習率 0.1")
rownames(reshaped_table) <- NULL

# 輸出表格
kable(reshaped_table, align = "c", format = "markdown")

# 載入必要的套件
library(MASS)

# 載入Pima.tr資料
data("Pima.tr")

# 合併Pima.tr和Pima.te，並提取目標類別
Pima_class <- rbind(Pima.tr, Pima.te)[, 8]

# 對Pima.tr和Pima.te的特徵資料進行標準化
Pima <- scale(rbind(Pima.tr, Pima.te)[, -8])

# 載入kohonen套件
library(kohonen)

# 設定種子以確保結果可重現
set.seed(1111)

# 建立自組織映射模型
Pima_som <- som(X = Pima, grid = somgrid(4, 4, "hexagonal"), rlen = 1000, alpha = c(0.05, 0.01))

#grid 可設定輸出層大小，"hexagonal"代表六角形網路結構，"rectangular"代表正方形網路拓墣結構
#rlen為最大迭代次數
#alpha為學習率，兩個數字分別為變化前起始值與變化後結束值

plot(Pima_som, type="changes") # 模型收斂圖
plot(Pima_som, type = "dist.neighbours") # U-matrix
plot(Pima_som, type="codes") # 神經元與屬性間的權重比例
plot(Pima_som, type="counts") # 神經元包含樣本數

# 載入 RSNNS 套件
library(RSNNS)

# 載入 snnsData 資料集
data(snnsData)

# 從資料集中提取 art1_letters.pat 資料
patterns <- snnsData$art1_letters.pat

# 將資料轉換成神經元活性圖列表
inputMaps <- matrixToActMapList(patterns, nrow = 7)

# 將圖形排列成 3x3 的矩陣
par(mfrow = c(3, 3))

# 逐一繪製神經元活性圖
for (i in 1:9) {
  plotActMap(inputMaps[[i]])
}

# 建立 ART1 模型
model <- art1(patterns, dimX = 7, dimY = 5, learnFuncParams = c(0.5, 0, 0), maxit = 100)

#learnFuncParams 為學習率
#maxit 為最大迭代次數

# 輸出聚類結果的表格
result_table <- table(encodeClassLabels(model$fitted.values))

# 輸出編碼後的聚類結果
encodeClassLabels(model$fitted.values) 

# 設置繪圖視窗為 3x2
par(mfrow = c(3, 2))

# 遍歷所有聚類結果，如果是群 5，則繪製神經元活性圖
for (i in 1:length(encodeClassLabels(model$fitted.values))) {
  if (encodeClassLabels(model$fitted.values)[i] == 5) {
    plotActMap(inputMaps[[i]])
  }
}

# 設置繪圖視窗為 4x2
par(mfrow = c(4, 2))

# 遍歷所有聚類結果，如果是群 7，則繪製神經元活性圖
for (i in 1:length(encodeClassLabels(model$fitted.values))) {
  if (encodeClassLabels(model$fitted.values)[i] == 7) {
    plotActMap(inputMaps[[i]])
  }
}

# 將樣本數添加到表格中
result_table <- rbind("樣本數" = result_table)

# 設置列名
colnames(result_table) <- paste("群", 1:7)

# 輸出表格
print(result_table)


