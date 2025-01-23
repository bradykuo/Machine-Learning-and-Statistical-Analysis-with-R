#ch4.6_p105

library(arules)
library(arulesViz)

data("IncomeESL")
#remove incomplete cases
IncomeESL <- IncomeESL[complete.cases(IncomeESL),]

# preparing the data set
#將 IncomeESL 數據集中的 income 列轉換為因子變量
#根據數值是否大於6，將其轉換為1或2（對應 $40- 和 $40+），並指定了相應的標籤
IncomeESL[["income"]] <- factor((as.numeric(IncomeESL[["income"]])>6)+1, levels = 1:2, labels=c("$40-","$40+"))

#creating transactions  
#使用 as() 函數將處理過的數據集轉換為交易
Income <- as(IncomeESL, "transactions")

#generate rules
#使用 apriori() 函數根據指定的支持度和信心閾值來生成關聯規則
rules <- apriori(Income, parameter=list(support=0.2, confidence=0.6))

#screen rules by rhs & lift
#使用 subset() 函數篩選出右手邊為 "income=$40+" 且提升度大於 1 的規則
rulesIncome <- subset(rules, subset=rhs %in% "income=$40+" & lift > 1)

#使用 inspect() 函數檢視並按照信心值排序篩選後的規則
inspect(sort(rulesIncome, by="confidence"))
