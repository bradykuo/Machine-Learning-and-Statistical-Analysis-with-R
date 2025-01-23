# 創建一個包含 x、y 座標和標籤的資料框
df <- data.frame(
  x = c(3, 1, 5), 
  y = c(2, 4, 6), 
  label = c("a","b","c")
)

# 創建一個 ggplot 物件 p，設置 x 和 y 軸標籤為 NULL，設置標題大小為 12
p <- ggplot(df, aes(x, y, label = label)) + labs(x = NULL, y = NULL) + theme(plot.title = element_text(size = 12)) 

# 繪製點圖，並設置標題
p + geom_point() + ggtitle("point")

# 繪製文字，並設置標題
p + geom_text() + ggtitle("text")

# 繪製柱狀圖，並設置標題
p + geom_bar(stat = "identity") + ggtitle("bar")

# 繪製熱圖，並設置標題
p + geom_tile() + ggtitle("raster")

# 繪製折線圖，並設置標題
p + geom_line() + ggtitle("line")

# 繪製面積圖，並設置標題
p + geom_area() + ggtitle("area")

# 繪製路徑圖，並設置標題
p + geom_path() + ggtitle("path")

# 繪製多邊形，並設置標題
p + geom_polygon() + ggtitle("polygon")

# 創建一個包含字型資訊的資料框
df <- data.frame(x = 1, y = 3:1, family = c("sans", "serif", "mono"))

# 繪製文字，指定不同字型，根據 family 欄位設置字型
ggplot(df, aes(x, y)) + geom_text(aes(label = family, family = family))

# 創建一個包含字體樣式資訊的資料框
df <- data.frame(x = 1, y = 3:1, face = c("plain", "bold", "italic"))

# 繪製文字，指定不同字體樣式，根據 face 欄位設置字體樣式
ggplot(df, aes(x, y)) + geom_text(aes(label = face, fontface = face))

# 創建一個包含文本位置資訊的資料框
df <- data.frame(
  x = c(1, 1, 2, 2, 1.5),
  y = c(1, 2, 1, 2, 1.5),
  text = c("bottom-left", "bottom-right", "top-left", "top-right", "center")
)

# 繪製文字，指定不同位置，根據 text 欄位設置位置
ggplot(df, aes(x, y)) + geom_text(aes(label = text))

# 繪製文字，指定不同位置並調整文字位置，根據 text 欄位設置位置
ggplot(df, aes(x, y)) + geom_text(aes(label = text), vjust = "inward", hjust = "inward")

# 創建一個資料框包含待標籤的資訊
df <- data.frame(trt = c("a", "b", "c"), resp = c(1.2, 3.4, 2.5))

# 繪製點圖和標籤，調整標籤位置，根據 resp 欄位設置標籤位置
ggplot(df, aes(resp, trt)) + geom_point() + geom_text(aes(label = paste0("(", resp, ")")), nudge_y = -0.25) + xlim(1, 3.6)

# 繪製散點圖並標示車型名稱，設置 x 軸範圍
ggplot(mpg, aes(displ, hwy)) + geom_text(aes(label = model)) + xlim(1, 8)

# 繪製散點圖並標示車型名稱，設置 x 軸範圍，檢查重疊
ggplot(mpg, aes(displ, hwy)) + geom_text(aes(label = model), check_overlap = TRUE) + xlim(1, 8)

# 創建一個資料框包含標籤位置資訊
label <- data.frame(
  waiting = c(55, 80), 
  eruptions = c(2, 4.3), 
  label = c("peak one", "peak two")
)

# 繪製熱圖並標示峰值標籤
ggplot(faithfuld, aes(waiting, eruptions)) + geom_tile(aes(fill = density)) + geom_label(data = label, aes(label = label))

# 繪製散點圖，根據車輛類型著色
ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()

install.packages("directlabels")

# 繪製散點圖，根據車輛類型著色，不顯示圖例，並使用 smart.grid 方法添加標籤
ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point(show.legend = FALSE) + directlabels::geom_dl(aes(label = class), method = "smart.grid")

# 繪製經濟資料中的失業率趨勢圖
ggplot(economics, aes(date, unemploy)) + geom_line()

# 從總統資料中篩選出符合
presidential <- subset(presidential, start > economics$date[1])

# 使用 ggplot 和 geom_rect 函數繪製經濟數據和總統任期的矩形
ggplot(economics) + geom_rect(
  aes(xmin = start, xmax = end, fill = party), 
  ymin = -Inf, ymax = Inf, alpha = 0.2, 
  data = presidential
) + 
  # 使用 geom_vline 函數繪製垂直線
  geom_vline(
    aes(xintercept = as.numeric(start)), 
    data = presidential,
    colour = "grey50", alpha = 0.5
  ) + 
  # 使用 geom_text 函數添加文字標籤
  geom_text(
    aes(x = start, y = 2500, label = name), 
    data = presidential, 
    size = 3, vjust = 0, hjust = 0, nudge_x = 50
  ) + 
  # 使用 geom_line 函數繪製經濟數據的折線圖
  geom_line(aes(date, unemploy)) + 
  # 使用 scale_fill_manual 函數自定義填充顏色
  scale_fill_manual(values = c("blue", "red"))

# 設定 y 和 x 軸的範圍
yrng <- range(economics$unemploy)
xrng <- range(economics$date)

# 創建標題
caption <- paste(strwrap("Unemployment rates in the US have varied a lot over the years", 40), collapse = "\n")

# 使用 geom_text 函數添加文字標籤
ggplot(economics, aes(date, unemploy)) + geom_line() + 
  geom_text(
    aes(x, y, label = caption), 
    data = data.frame(x = xrng[1], y = yrng[2], caption = caption), 
    hjust = 0, vjust = 1, size = 4
  )

# 使用 annotate 函數添加文字標籤
ggplot(economics, aes(date, unemploy)) + geom_line() + 
  annotate("text", x = xrng[1], y = yrng[2], label = caption,
           hjust = 0, vjust = 1, size = 4
  )

# 使用 geom_bin2d 函數繪製二維直方圖
ggplot(diamonds, aes(log10(carat), log10(price))) + geom_bin2d() + facet_wrap(~cut, nrow = 1)

# 假設您已經進行了線性回歸分析並得到了模型 mod
mod <- lm(log10(price) ~ log10(carat), data = diamonds)

# 獲取模型的係數
mod_coef <- coef(mod)

# 使用 geom_abline 函數添加斜率線
ggplot(diamonds, aes(log10(carat), log10(price))) + geom_bin2d() + 
  geom_abline(intercept = mod_coef[1], slope = mod_coef[2], colour = "white", size = 1) + 
  facet_wrap(~cut, nrow = 1)

install.packages("nlme") 
library(mgcv)

# 載入 nlme 套件中的 Oxboys 資料集
data(Oxboys, package = "nlme")

# 查看 Oxboys 資料集的前幾行
head(Oxboys)

# 使用 geom_point 函數繪製點圖
ggplot(Oxboys, aes(age, height, group = Subject)) + geom_point() + geom_line()

# 使用 geom_line 函數繪製折線圖
ggplot(Oxboys, aes(age, height)) + geom_point() + geom_line()

# 使用 geom_smooth 函數添加平滑線
ggplot(Oxboys, aes(age, height, group = Subject)) + geom_line() + geom_smooth(method = "lm", se = FALSE)

# 使用 geom_line 函數添加折線並指定分組
ggplot(Oxboys, aes(age, height)) + geom_line(aes(group = Subject)) + geom_smooth(method = "lm", size = 2, se = FALSE)

# 使用 geom_boxplot 函數繪製箱形圖
ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot()

# 使用 geom_boxplot 函數繪製箱形圖和折線圖
ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot() + geom_line(colour = "#3366FF", alpha = 0.5)

# 使用 geom_boxplot 函數繪製箱形圖和折線圖並指定分組
ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot() + geom_line(aes(group = Subject), colour = "#3366FF", alpha = 0.5)

# 使用 geom_line 函數繪製折線圖
df <- data.frame(x = 1:3, y = 1:3, colour = c(1,3,5))
ggplot(df, aes(x, y, colour = factor(colour))) + geom_line(aes(group = 1), size = 2) + geom_point(size = 5)

# 使用 geom_line 函數繪製折線圖
ggplot(df, aes(x, y, colour = colour)) + geom_line(aes(group = 1), size = 2) + geom_point(size = 5)

# 插值
xgrid <- with(df, seq(min(x), max(x), length = 50))
interp <- data.frame(
  x = xgrid,
  y = approx(df$x, df$y, xout = xgrid)$y,
  colour = approx(df$x, df$colour, xout = xgrid)$y  
)

# 使用 geom_line 函數繪製插值折線圖
ggplot(interp, aes(x, y, colour = colour)) + geom_line(size = 2) + geom_point(data = df, size = 5)

# 使用 geom_bar 函數繪製直方圖
ggplot(mpg, aes(class)) + geom_bar()

# 使用 geom_bar 函數繪製堆疊直方圖
ggplot(mpg, aes(class, fill = drv)) + geom_bar()

# 使用 geom_bar 函數繪製堆疊直方圖
ggplot(mpg, aes(class, fill = hwy)) + geom_bar()

# 使用 geom_bar 函數繪製堆疊直方圖並指定分組
ggplot(mpg, aes(class, fill = hwy, group = hwy)) + geom_bar()

# 使用 geom_contour 函數繪製等高線圖
ggplot(faithfuld, aes(eruptions, waiting)) + geom_contour(aes(z = density, colour = ..level..))

# 使用 geom_raster 函數繪製光栅圖
ggplot(faithfuld, aes(eruptions, waiting)) + geom_raster(aes(fill = density))

# 子樣本
small <- faithfuld[seq(1, nrow(faithfuld), by = 10), ]

# 使用 geom_point 函數繪製點圖
ggplot(small, aes(eruptions, waiting)) + geom_point(aes(size = density), alpha = 1/3) + scale_size_area()
