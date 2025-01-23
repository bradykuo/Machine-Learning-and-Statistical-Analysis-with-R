#ch4.6_p100
# 安裝資源包
install.packages("arules")
install.packages("arulesViz")

# 引用資料庫
library(arules)
library(arulesViz)

# 加載數據
data("IncomeESL")
# 去除缺失值
IncomeESL <- IncomeESL[complete.cases(IncomeESL),]
# 檢查數據維度
dim(IncomeESL)

#ch4.6_p100
# 將數據轉換為交易格式
Income <- as(IncomeESL, "transactions")
# 摘要
summary(Income)
# 按項目頻率排序
sort(itemFrequency(Income), decreasing = T)
# 項目頻率圖
itemFrequencyPlot(Income, support=0.2, cex.names=0.8)

#ch4.6_p101
# 生成關聯規則
rules <- apriori(Income, parameter=list(support=0.1, confidence=0.6))
summary(rules)
# 繪製關聯規則圖
plot(rules, measure=c("confidence", "lift"),shading="support")
# 分組繪製關聯規則圖
plot(rules, method="grouped")

#ch4.6_p103
# 篩選特定右手邊的規則並檢查
rulesOwn <- subset(rules, subset=rhs %in% "householder status=own" & lift>1)
inspect(head(sort(rulesOwn, by="support"), n=5))

# 篩選特定右手邊的規則並檢查，設定信賴度大於0.9且支持度小於等於0.11
rulesOwn <- subset(rules, subset=confidence > 0.9 & support <= 0.11)
# 檢視並排序篩選後的規則，按照支持度排序
inspect(head(sort(rulesOwn, by="support"), n=5))
