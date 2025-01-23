# 安装所需的R包
install.packages("remotes") 
remotes::install_github("ropensci/USAboundaries") 
remotes::install_github("ropensci/USAboundariesData")
install.packages("sf")

# 载入所需的R包
library(USAboundaries) 
library(ggplot2) 
library(sf) 
library(dplyr)

# 获取密歇根州县级边界数据
mi_counties <- map_data("county", "michigan") %>% select(lon = long, lat, group, id = subregion)

# 展示数据前几行
head(mi_counties)

# 绘制密歇根州县级边界地图
ggplot(mi_counties, aes(lon, lat)) + geom_polygon(aes(group = group)) + coord_quickmap()

# 绘制仅显示边界线的密歇根州县级地图
ggplot(mi_counties, aes(lon, lat)) + geom_polygon(aes(group = group), fill = NA, colour = "grey50") + coord_quickmap()

# 获取美国国会区边界数据
cong.map <- us_congressional(resolution = "high") 

# 绘制美国国会区地图
ggplot(cong.map) + geom_sf() 

# 仅保留美国大陆州份的国会区边界并绘制地图
cong.map <- filter(cong.map, state_name %in% state.name & !(state_name %in% c("Hawaii", "Alaska"))) 
ggplot(cong.map) + geom_sf(color = "black", fill = "white") + theme_void()

# 获取密歇根州城市数据
mi_cities <- maps::us.cities %>% tbl_df() %>%
  filter(country.etc == "MI") %>% select(-country.etc, lon = long) %>% arrange(desc(pop))

# 展示数据
mi_cities

# 绘制密歇根州城市地图，点的大小根据人口数量
ggplot(mi_cities, aes(lon, lat)) + geom_point(aes(size = pop)) + scale_size_area() + coord_quickmap()

# 绘制密歇根州城市地图，添加县级边界并根据人口数量调整点的大小和颜色
ggplot(mi_cities, aes(lon, lat)) +
  geom_polygon(aes(group = group), mi_counties, fill = NA, colour = "grey50") +  # 添加县级边界
  geom_point(aes(size = pop), colour = "red") +  # 添加城市点，根据人口数量设置点的大小
  scale_size_area() +  # 设置点大小的比例尺
  coord_quickmap()  # 设置坐标系统，快速绘制地图

library(raster)
library(ggplot2)

# 读取 mi_raster.rds 文件
mi_raster <- readRDS("/Users/brady/Desktop/R code/mi_raster.rds")

# 将 raster 对象转换为 data.frame 以进行绘图
df <- as.data.frame(raster::rasterToPoints(mi_raster))
names(df) <- c("lon", "lat", "x")

# 绘制地图
ggplot(df, aes(lon, lat)) + 
  geom_raster(aes(fill = x)) + 
  scale_fill_gradient(low = "white", high = "blue") +
  labs(fill = "Value") +
  theme_minimal()

library(dplyr)

# 从 midwest 数据中选择密歇根州（MI）的数据，并将县名转换为小写
mi_census <- midwest %>%
  dplyr::tbl_df() %>%
  dplyr::filter(state == "MI") %>%
  dplyr::mutate(county = tolower(county)) %>%
  dplyr::select(county, area, poptotal, percwhite, percblack)

# 输出密歇根州的人口普查数据
mi_census

# 使用 left_join 函数将密歇根州的人口普查数据与密歇根州的县界数据合并
census_counties <- left_join(mi_census, mi_counties, by = c("county" = "id")) 

# 输出合并后的数据
census_counties

# 绘制密歇根州县级地图，并根据人口数量填充颜色
ggplot(census_counties, aes(lon, lat, group = county)) + 
  geom_polygon(aes(fill = poptotal)) + 
  coord_quickmap()

# 绘制密歇根州县级地图，并根据白人比例填充颜色
ggplot(census_counties, aes(lon, lat, group = county)) + 
  geom_polygon(aes(fill = percwhite)) + 
  coord_quickmap()

# 创建数据框
y <- c(18, 11, 16)
df <- data.frame(x = 1:3, y = y, se = c(1.2, 0.5, 1.0))

# 创建基础图形对象
base <- ggplot(df, aes(x, y, ymin = y - se, ymax = y + se)) 

# 添加误差条
base + geom_crossbar()
base + geom_pointrange()
base + geom_smooth(stat = "identity")

# 添加误差线
base + geom_errorbar() 
base + geom_linerange() 
base + geom_ribbon()

# 未加权的散点图
ggplot(midwest, aes(percwhite, percbelowpoverty)) + geom_point()

# 根据人口加权的散点图
ggplot(midwest, aes(percwhite, percbelowpoverty)) + geom_point(aes(size = poptotal / 1e6)) + scale_size_area("人口\n(百万)", breaks = c(0.5, 1, 2, 4))

# 未加权的散点图与线性拟合线
ggplot(midwest, aes(percwhite, percbelowpoverty)) + geom_point() + geom_smooth(method = lm, size = 1)

# 根据人口加权的散点图与线性拟合线
ggplot(midwest, aes(percwhite, percbelowpoverty)) + geom_point(aes(size = poptotal / 1e6)) + geom_smooth(aes(weight = poptotal), method = lm, size = 1) + scale_size_area(guide = "none")

# 直方图
ggplot(midwest, aes(percbelowpoverty)) + geom_histogram(binwidth = 1) + ylab("县数")

# 根据人口加权的直方图
ggplot(midwest, aes(percbelowpoverty)) + geom_histogram(aes(weight = poptotal), binwidth = 1) + ylab("人口（千）")

# 钻石数据集
diamonds

# 深度的直方图
ggplot(diamonds, aes(depth)) + geom_histogram()

# 深度的直方图（设置条带宽度）
ggplot(diamonds, aes(depth)) + geom_histogram(binwidth = 0.1) + xlim(55, 70)

# 深度的频率多边形图
ggplot(diamonds, aes(depth)) + geom_freqpoly(aes(colour = cut), binwidth = 0.1, na.rm = TRUE) + xlim(58, 68) + theme(legend.position = "none")

# 深度的堆积直方图
ggplot(diamonds, aes(depth)) + geom_histogram(aes(fill = cut), binwidth = 0.1, position = "fill", na.rm = TRUE) + xlim(58, 68) + theme(legend.position = "none")

# 深度的密度图
ggplot(diamonds, aes(depth)) + geom_density(na.rm = TRUE) + xlim(58, 68) + theme(legend.position = "none")

# 深度的密度图（设置颜色）
ggplot(diamonds, aes(depth, fill = cut, colour = cut)) + geom_density(alpha = 0.2, na.rm = TRUE) + xlim(58, 68) + theme(legend.position = "none")

# 不同清晰度的箱线图
ggplot(diamonds, aes(clarity, depth)) + geom_boxplot()

# 根据克拉加宽的箱线图
ggplot(diamonds, aes(carat, depth)) + geom_boxplot(aes(group = cut_width(carat, 0.1))) + xlim(NA, 2.05)

# 不同清晰度的小提琴图
ggplot(diamonds, aes(clarity, depth)) + geom_violin()

# 根据克拉加宽的小提琴图
ggplot(diamonds, aes(carat, depth)) + geom_violin(aes(group = cut_width(carat, 0.1))) + xlim(NA, 2.05)

# 散点图与标签
df <- data.frame(x = rnorm(2000), y = rnorm(2000))
norm <- ggplot(df, aes(x, y)) + xlab(NULL) + ylab(NULL) 

# 标准散点图
norm + geom_point()
norm + geom_point(shape = 1) # 空心圆
norm + geom_point(shape = ".") # 像素大小

# 透明度设置
norm + geom_point(alpha = 1 / 3) 
norm + geom_point(alpha = 1 / 5) 
norm + geom_point(alpha = 1 / 10)

# 二维频次直方图
norm + geom_bin2d()
norm + geom_bin2d(bins = 10)

# 六边形直方图
norm + geom_hex()
norm + geom_hex(bins = 10)

# 钻石颜色的柱状图
ggplot(diamonds, aes(color)) + geom_bar()

# 钻石颜色与价格的柱状图
ggplot(diamonds, aes(color, price)) + geom_bar(stat = "summary_bin", fun.y = mean)

# 二维直方图
ggplot(diamonds, aes(table, depth)) + geom_bin2d(binwidth = 1, na.rm = TRUE) + xlim(50, 70) + ylim(50, 70)

# 二维光栅图
ggplot(diamonds, aes(table, depth, z = price)) + geom_raster(binwidth = 1, stat = "summary_2d", fun = mean, na.rm = TRUE) + xlim(50, 70) + ylim(50, 70)
