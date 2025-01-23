library(MASS)
library(RSNNS)
data("Pima.tr")
data("Pima.te")
set.seed(1111)

#7.2
# 資料合併與預處理
Pima=rbind(Pima.tr,Pima.te)
level_name={}
for (i in 1:7) {
  Pima[,i]=cut(Pima[,i],breaks=2,ordered_result=T,include.lowest=T)
  level_name<-rbind(level_name,levels(Pima[,i]))
}
level_name=data.frame(level_name)
row.names(level_name)=colnames(Pima)[1:7]
colnames(level_name)=paste("L",1:2,sep="")
level_name

#7.12
# 切分訓練集和測試集
Pima.tr=Pima[1:200,]
Pima.te=Pima[201:nrow(Pima),]

# Naive Bayes 模型
library(bnlearn)
bn = naive.bayes(Pima.tr, "type")
plot(bn)
bn
pred = predict(bn, Pima.te)
tab=table(pred, Pima.te[, "type"])
tab
acc=sum(diag(tab))/sum(tab)
acc

#7.13
# Tree Bayes 模型
tan = tree.bayes(Pima.tr, "type")
plot(tan)
tan
#whitelist 引數可設定要增加的連結箭頭；blacklist 引數可設定要取消的箭頭
fitted = bn.fit(tan, Pima.tr, method = "bayes")
pred = predict(fitted, Pima.te)
tab=table(pred, Pima.te[, "type"])
tab
acc=sum(diag(tab))/sum(tab)
acc