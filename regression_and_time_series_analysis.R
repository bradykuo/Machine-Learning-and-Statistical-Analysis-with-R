# 加載所需的函式庫
library(car)
library(MASS)
library(lmtest)
library(knitr)

# 載入數據
data(UScrime)
UScrime$So = factor(UScrime$So)

# 數據概述和散點圖矩陣
summary(UScrime)
scatterplotMatrix(UScrime)

# 建立回歸模型
reg = lm(y ~ ., data = UScrime)
summary(reg)

# 殘差圖
residualPlots(reg)

# QQ圖和QQ線
qqnorm(residuals(reg))
qqline(residuals(reg))

# 線性檢驗（Ramsey RESET 檢驗）
reset_test <- resettest(reg, power = 2:3, type = "regressor")

# 常態性檢驗（Shapiro-Wilk 檢驗）
shapiro_test <- shapiro.test(residuals(reg))

# Durbin-Watson 檢驗
dw_test <- durbinWatsonTest(reg)

# Breusch-Pagan 檢驗
bp_test <- bptest(reg)

# 異常值檢驗
outlier_test <- outlierTest(reg)

# 方差膨脹因子
vif_values <- vif(reg)

reset_test
shapiro_test
dw_test
bp_test
outlier_test

# 創建包含檢定結果的數據框
test_results <- data.frame(
  Test = c("Ramsey RESET Test", "Shapiro-Wilk Test", "Durbin-Watson Test", "Breusch-Pagan Test", "Outlier Test"),
  `P-Value` = c(0.0218, 0.7849, 0.364, 0.2388, 0.07564)
)

# 顯示結果表格
kable(test_results, format = "markdown", col.names = c("Test", "P-Value"))


# 加載所需的函式庫
library(car)
library(MASS)
library(lmtest)
library(forecast)
library(TSA)

# 使用數據集 Pima.tr 建立邏輯回歸模型
LG = glm(type ~ ., data = Pima.tr, family = binomial)
summary(LG)

# 對訓練數據集進行預測，type = "response" 表示返回概率
pre_LG = predict(LG, Pima.tr, type = "response")

# 初始化一個空向量來存儲準確度
acc = {}

# 循環遍歷閾值 t，從 0.1 到 0.9，每次增加 0.1
for (t in seq(0.1, 0.9, 0.1)) {
  # 根據閾值 t 將概率轉換為預測類別
  pre = ifelse(pre_LG >= t, 1, 0)
  # 創建實際值和預測值的混淆矩陣
  tab = table(Pima.tr$type, pre)
  # 計算準確度並將其添加到 acc 向量中
  acc = c(acc, sum(diag(tab)) / nrow(Pima.tr))
}

# 找到最佳閾值 t 對應的準確度最大值
t = seq(0.1, 0.9, 0.1)[which.max(acc)]

# 使用最佳閾值 t 對測試數據集進行預測
pre_LG = predict(LG, Pima.te, type = "response")
pre = ifelse(pre_LG >= t, 1, 0)
tab = table(Pima.te$type, pre)
acc = sum(diag(tab)) / nrow(Pima.te)

# 打印測試數據集上的準確度
print(acc)

# 定義銷售數據
data <- "
Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
2001 545 556 572 579 581 574 583 583 564 551 543 561
2002 548 562 578 584 585 584 597 597 579 562 552 578
2003 571 580 602 603 617 602 613 613 595 582 577 595
2004 587 598 610 613 623 622 627 633 607 598 593 611
2005 601 606 630 639 645 633 641 644 620 608 598 614
2006 605 602 630 636 647 641 655 652 629 617 610 627
2007 622 624 642 653 662 659 673 669 647 634 625 646
2008 638 641 660 668 678 676 686 684 660 646 639 656
2009 648 649 671 678 689 688 698 698 673 658 651 665
2010 656 655 673 678 691 691 703 706 673 661 652 666
2011 662 662 684 691 706 699 714 716 687 674 668 684
2012 660 665 678 707 718 712 727 724 696 686 675 691
"

# 使用 read.table 從文本字符串中讀取數據
sales_ <- read.table(text = data, header = TRUE, row.names = 1)

# 將數據轉置並轉換為向量
sales_vector <- as.vector(t(sales_))

# 創建時間序列對象
sales <- ts(sales_vector, start = c(2001, 1), frequency = 12)

# 檢查是否成功轉換為時間序列對象
print(is.ts(sales))

# 顯示時間序列數據
tsdisplay(sales)

# 拆分數據為訓練集和測試集
train = ts(sales[seq(1, length(sales)-12)], frequency = 12, start = c(2001,1))
test = ts(sales[seq(length(sales)-11, length(sales))], frequency = 12, start =c(2012,1))

# 顯示訓練集的差分時間序列圖
tsdisplay(diff(train), main = "First difference of sales")
tsdisplay(diff(diff(train), lag = 12), main = "First and seasonal difference of sales")

# 自動 ARIMA 模型擬合
auto.fit = auto.arima(sales, approximation = FALSE, trace = FALSE)
summary(auto.fit)

# 手動 ARIMA 模型擬合
m1 = Arima(train, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
m1$var.coef

# 顯示模型診斷圖
tsdiag(m1, gof = 36)

# QQ 圖檢查殘差的正態性
qqnorm(residuals(m1))
qqline(residuals(m1))
legend("topleft", legend = paste("p-value = ", round(shapiro.test(residuals(m1))$p.value, 4)))

# 使用自動 ARIMA 模型進行預測
pred = predict(auto.fit, n.ahead = 12)

# 繪製訓練數據和預測結果圖
plot(train, type = "l", xlim = c(2001, 2012), main = "Prediction", xlab = "Time", ylab = "Sales")
lines(pred$pred, col = "blue", lwd = 3)
lines(test, lty = 2, col = "red")

















