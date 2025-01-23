# 安裝並載入ggplot2套件
install.packages("ggplot2")
library(ggplot2)
# 顯示mpg資料集
#在 R 中，mpg 通常是指 "miles per gallon"，也就是每加侖的英里數。
#在 mpg 資料集中，這個詞指的是一個包含有關汽車燃油效率的資料集。
#該資料集包含多個變數，如城市里程 (cty)、高速里程 (hwy)、
#引擎排氣量 (displ)、汽缸數 (cyl)、車型 (class) 等。
mpg

# 散點圖，x軸為displ，y軸為hwy
ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()

# 散點圖，x軸為cty，y軸為hwy
ggplot(mpg, aes(cty, hwy)) + geom_point()

# 散點圖，x軸為carat，y軸為price
ggplot(diamonds, aes(carat, price)) + geom_point()

# 折線圖，x軸為date，y軸為unemploy
ggplot(economics, aes(date, unemploy)) + geom_line()

# 直方圖，x軸為cty
ggplot(mpg, aes(cty)) + geom_histogram()

# 散點圖，x軸為displ，y軸為hwy，以class為顏色
ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()

# 散點圖，x軸為displ，y軸為hwy，顏色為固定的"blue"
ggplot(mpg, aes(displ, hwy)) + geom_point(aes(colour = "blue"))

# 散點圖，x軸為displ，y軸為hwy，顏色為固定的"blue"
ggplot(mpg, aes(displ, hwy)) + geom_point(colour = "blue")

# 散點圖，x軸為displ，y軸為hwy，以class為分面變量
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~class)

# 散點圖，x軸為displ，y軸為hwy，並加上平滑曲線
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth()

# 資料使用寬度0.2的平滑
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(span = 0.2)

# 資料使用寬度1的平滑
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(span = 1)

# 使用方法 "gam" 擬合一個一般化加法模型
library(mgcv)

ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x))

# 使用方法 "lm" 擬合一個線性模型
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(method = "lm")

# 將 drv 類別變量和 hwy 數值變量繪製散點圖
ggplot(mpg, aes(drv, hwy)) + geom_point()

#期末考考density
# 使用 jitter() 函數來增加視覺效果，讓點不會重疊
ggplot(mpg, aes(drv, hwy)) + geom_jitter()

# 繪製 drv 和 hwy 的箱形圖
ggplot(mpg, aes(drv, hwy)) + geom_boxplot()

# 繪製 drv 和 hwy 的小提琴圖
ggplot(mpg, aes(drv, hwy)) + geom_violin()

# 繪製 hwy 的直方圖
ggplot(mpg, aes(hwy)) + geom_histogram()

# 繪製 hwy 的頻率多邊形
ggplot(mpg, aes(hwy)) + geom_freqpoly()

# 繪製 hwy 的頻率多邊形，指定 bin 寬度為2.5
ggplot(mpg, aes(hwy)) + geom_freqpoly(binwidth = 2.5)

# 繪製 hwy 的頻率多邊形，指定 bin 寬度為1
ggplot(mpg, aes(hwy)) + geom_freqpoly(binwidth = 1)

# 繪製 displ 和 drv 的頻率多邊形，指定 bin 寬度為0.5，並以 drv 為顏色
ggplot(mpg, aes(displ, colour = drv)) + geom_freqpoly(binwidth = 0.5)

# 將 displ 和 drv 結合的直方圖 # 將版面排列為三列
ggplot(mpg, aes(displ, fill = drv)) + geom_histogram(binwidth = 0.5) + facet_wrap(~drv, ncol = 3)
 
# 使用 mpg 資料集，以製造商（manufacturer）為 x 軸，繪製柱狀圖
ggplot(mpg, aes(manufacturer)) + geom_bar()

# 創建一個名為 drugs 的資料框，包含藥物和效應的資訊
drugs <- data.frame(
  drug = c("a", "b", "c"),
  effect = c(4.2, 9.7, 6.1)
)

# 以藥物和效應為 x 和 y 軸，繪製直條圖
ggplot(drugs, aes(drug, effect)) + geom_bar(stat = "identity")

# 以藥物和效應為 x 和 y 軸，繪製散點圖
ggplot(drugs, aes(drug, effect)) + geom_point()

# 使用 economics 資料集，以時間序列繪製經濟失業率的折線圖
ggplot(economics, aes(date, unemploy / pop)) + geom_line()

# 使用 economics 資料集，以時間序列繪製失業天數的折線圖
ggplot(economics, aes(date, uempmed)) + geom_line()

# 使用 economics 資料集，以失業率和失業天數為 x 和 y 軸，繪製路徑圖和散點圖
ggplot(economics, aes(unemploy / pop, uempmed)) + geom_path() + geom_point()

# 定義一個函數用於從日期中提取年份
year <- function(x) as.POSIXlt(x)$year + 1900

# 使用 economics 資料集，以失業率和失業天數為 x 和 y 軸，繪製路徑圖和散點圖，並根據年份著色
ggplot(economics, aes(unemploy / pop, uempmed)) + geom_path(colour = "grey50") + geom_point(aes(colour = year(date)))

# 列印 economics 資料集的列名
names(economics)

# 使用 year 函數提取日期的年份
year(economics$date)

# 使用 mpg 資料集，以城市和高速公路行駛里程為 x 和 y 軸，繪製散點圖，並設置透明度
ggplot(mpg, aes(cty, hwy)) + geom_point(alpha = 1 / 3)

# 使用 mpg 資料集，以城市和高速公路行駛里程為 x 和 y 軸，繪製散點圖，並設置透明度，並設置 x 和 y 軸標籤
ggplot(mpg, aes(cty, hwy)) + geom_point(alpha = 1 / 3) + xlab("city driving (mpg)") + ylab("highway driving (mpg)")

# 使用 mpg 資料集，以城市和高速公路行駛里程為 x 和 y 軸，繪製散點圖，並設置透明度，並將 x 和 y 軸標籤清除
ggplot(mpg, aes(cty, hwy)) + geom_point(alpha = 1 / 3) + xlab(NULL) +  ylab(NULL)

# 使用 mpg 資料集，以駕駛類型和高速公路行駛里程為 x 和 y 軸，繪製 jitter 圖
ggplot(mpg, aes(drv, hwy)) + geom_jitter(width = 0.25)

# 使用 mpg 資料集，以駕駛類型和高速公路行駛里程為 x 和 y 軸，繪製 jitter 圖，並設置 x 和 y 軸的範圍
ggplot(mpg, aes(drv, hwy)) + geom_jitter(width = 0.25) + xlim("f", "r") + ylim(20, 30)

# 使用 mpg 資料集，以駕駛類型和高速公路行駛里程為 x 和 y 軸，繪製 jitter 圖，並將缺失值忽略
ggplot(mpg, aes(drv, hwy)) + geom_jitter(width = 0.25, na.rm = TRUE) + ylim(NA, 30)

# 使用 mpg 資料集，以汽缸數和高速公路行駛里程為 x 和 y 軸，繪製散點圖，並根據汽缸數著色
p <- ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) + geom_point()

# 列印圖形 p
print(p)

# 將圖形 p 儲存為 plot.png 文件
ggsave("plot.png", width = 5, height = 5)

# 摘要圖形 p
summary(p)

# 將圖形 p 儲存為 plot.rds 文件
saveRDS(p, "plot.rds")

# 從 plot.rds 文件中讀取圖形
q <- readRDS("plot.rds")

# 使用 qplot 函數繪製散點圖
qplot(displ, hwy, data = mpg)

# 使用 qplot 函數繪製散點圖，省略 y 軸
qplot(displ, data = mpg)

# 使用 qplot 函數繪製散點圖，並指定顏色
qplot(displ, hwy, data = mpg, colour = "blue")

# 使用 qplot 函數繪製散點圖，並指定顏色為藍色
qplot(displ, hwy, data = mpg, colour = I("blue"))
