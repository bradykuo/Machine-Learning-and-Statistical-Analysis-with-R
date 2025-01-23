# 載入套件與資料
library(RoughSets)
data(RoughSetData)

# 建立決策表
decision_table <- SF.asDecisionTable(dataset = RoughSetData$hiring.dt,
                                     decision.attr = 5, 
                                     indx.nominal = 1:5)

# dataset 引數為要轉換成決策表的資料集
# decision.attr 引數為指定資料集中的決策屬性欄位
# indx.nominal 引數為指定資料集中哪些欄位為類別尺度

# 計算不可分辨關係
IND <- BC.IND.relation.RST(decision_table, c(2,3))
summary(IND)

# 計算近似集
roughset <- BC.LU.approximation.RST(decision_table, IND)
RX_lower <- roughset$lower.approximation$Accept
RX_upper <- roughset$upper.approximation$Accept
BN_RX <- setdiff(RX_upper, RX_lower)

# 計算相關係數
alpha_R_X <- nrow(data.frame(RX_lower))/nrow(data.frame(RX_upper))

DX_lower0 <- roughset$lower.approximation$Reject
DX_upper0 <- roughset$upper.approximation$Reject

# 計算近似集
RX_lower <- roughset$lower.approximation$Accept
RX_upper <- roughset$upper.approximation$Accept
RX_lower0 <- roughset$lower.approximation$Reject 
RX_upper0 <- roughset$upper.approximation$Reject

# 計算係數
alpha_R_F <- (nrow(data.frame(RX_lower))+nrow(data.frame(RX_lower0)))/
  (nrow(data.frame(RX_upper))+nrow(data.frame(RX_upper0)))

gamma_R_F <- (nrow(data.frame(RX_lower))+nrow(data.frame(RX_lower0)))/
  nrow(decision_table)

res=BC.discernibility.mat.RST(decision_table)
reduct=FS.all.reducts.computation(res);reduct

rule_rst = RST.rule.induction(dataset = RoughSetData$hiring.dt,  # 改為 hiring.dt
                              decision.attr = 5,
                              indx.nominal = 1:5,
                              min.sup = 0.25)
rule_rst

RST.rule.induction <- function(dataset, decision.attr = NULL, indx.nominal = NULL, min.sup = NULL, min.conf = NULL) {
  require(RoughSets)
  
  decision.table <- SF.asDecisionTable(dataset, decision.attr, indx.nominal)
  n <- nrow(decision.table)
  p <- ncol(decision.table)-1
  pset <- setdiff(seq(1,ncol(decision.table)),decision.attr)
  
  rule <- {}; support.n <- {}; conf.X <- {}; lift.Y <- {}
  
  for (i in 1:n) {
    Dset <- as.character(which(decision.table[,decision.attr]==decision.table[i,decision.attr]))
    
    for (m in 1:(p-1)) {
      comb <- combn(p,m)
      
      for (j in 1:ncol(comb)) {
        set <- decision.table[decision.table[,pset[comb[1,j]]] == decision.table[i,pset[comb[1,j]]],]
        if(m > 1) {
          for(k in 2:m) {
            set <- set[set[,pset[comb[k,j]]] == decision.table[i,pset[comb[k,j]]],]
          }
        }
        Cset <- rownames(set)
        
        if (is.null(min.sup)) {
          if (is.null(min.conf)) {
            if (setequal(intersect(Dset, Cset), Cset)) {
              reduct <- decision.table[i,]
              fe <- setdiff(seq(1,ncol(decision.table)),union(decision.attr,pset[comb[,j]]))
              reduct[,fe] <- "x"
              rule <- rbind(rule,reduct)
              support.n <- c(support.n,length(intersect(Dset, Cset)))
              conf.X <- c(conf.X,length(Cset))
              lift.Y <- c(lift.Y,length(Dset))
            }
          } else {
            if (length(intersect(Dset, Cset))/length(Cset) >= min.conf) {
              reduct <- decision.table[i,]
              fe <- setdiff(seq(1,ncol(decision.table)),union(decision.attr,pset[comb[,j]]))
              reduct[,fe] <- "x"
              rule <- rbind(rule,reduct)
              support.n <- c(support.n,length(intersect(Dset, Cset)))
              conf.X <- c(conf.X,length(Cset))
              lift.Y <- c(lift.Y,length(Dset))
            }
          }
        } else if (is.null(min.conf)) {
          if (length(intersect(Dset, Cset))/n >= min.sup) {
            reduct <- decision.table[i,]
            fe <- setdiff(seq(1,ncol(decision.table)),union(decision.attr,pset[comb[,j]]))
            reduct[,fe] <- "x" 
            rule <- rbind(rule,reduct)
            support.n <- c(support.n,length(intersect(Dset, Cset)))
            conf.X <- c(conf.X,length(Cset))
            lift.Y <- c(lift.Y,length(Dset))
          }
        } else {
          if (length(intersect(Dset, Cset))/n >= min.sup & length(intersect(Dset, Cset))/length(Cset) >= min.conf) {
            reduct <- decision.table[i,]
            fe <- setdiff(seq(1,ncol(decision.table)),union(decision.attr,pset[comb[,j]]))
            reduct[,fe] <- "x"
            rule <- rbind(rule,reduct)
            support.n <- c(support.n,length(intersect(Dset, Cset)))
            conf.X <- c(conf.X,length(Cset))
            lift.Y <- c(lift.Y,length(Dset))
          }
        }
      }
    }
  }
  
  if (!is.null(rule)) {
    rule2 <- rule[!duplicated(rule),]
    support.n <- support.n[!duplicated(rule)]
    conf.X <- conf.X[!duplicated(rule)]
    lift.Y <- lift.Y[!duplicated(rule)]
    rownames(rule2) <- seq(1,nrow(rule2))
    support <- support.n/n
    conf <- support.n/conf.X
    lift <- conf/(lift.Y/n)
    rule <- cbind(rule2,support,conf,lift)
  }
  
  return(rule)
}