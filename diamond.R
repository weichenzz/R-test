library('ggplot2')
data("diamonds")

#创建散点图
ggplot(aes(x = carat, y = price), data = diamonds,
       xlim = c(0, quantile(diamonds$carat, 0.99)),
       ylim = c(0, quantile(diamonds$price, 0.99)))+
  geom_point(fill = I("red"), color = I("black"), shape = 21)

#统计平滑函数
ggplot(aes(x = carat, y = price), data = diamonds) +
  geom_point(color = "#191970", alpha = 1/4) +
#与geom_smooth一样
#'lm'绘制一条直线，'loess'绘制一条非常平滑的曲线
  stat_smooth(method = 'lm') +
  scale_x_continuous(lim = c(0, quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(lim = c(0, quantile(diamonds$price, 0.99)))

library(ggplot2)
library(scales)
library(memisc)
install.packages('memisc')
install.packages('lattice')
library(lattice)
library(MASS)
library(car)
library(reshape2)
library(plyr)
library(GGally)
library(gridExtra)


#设置随机化种子
set.seed(4324)

diamond_samp <- diamonds[sample(1 : length(diamonds$price), 10000), ]
ggpairs(diamond_samp, lower = list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

#绘制直方图
p1 <- qplot(x = price, data = diamond_samp, binwidth = 100, fill = I("red")) +
#添加标题
  ggtitle("Price")

p2 <- qplot(x = price, data = diamond_samp, binwidth = 0.01, fill = I("blue")) + 
  ggtitle("Price(log10") + 
  scale_x_log10()
#使用grid包来拼接图
grid.arrange(p1, p2, ncol = 2)


#trans_new函数
#transform 接一个函数，数据要作为参数输入，表用怎么使用数据x
#inverse 输入transform 的反作用函数（使用）
#domain 接一个二元数值向量，表明只对这个范围之内的数据实行这种变换
#name参数是什么都无所谓（但是必须要有），其实在这里inverse参数是什么也无所谓
library(scales)
cuberoot_trans <- function() trans_new('cuberoot', 
                                      transform = function(x) x^(1/3),
                                      inverse = function(x) x^3)

ggplot(aes(carat, price), data = diamonds) +
  geom_point(alpha = 0.4, size = 0.7, position = 'jitter') +
  #trans这个参数是针对数字进行操作而不是y轴的刻度
  scale_x_continuous(trans = cuberoot_trans(), limits = c(.2, 3),
                    breaks = c(.2, .5, 1, 2, 3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price(log10) by cube-root of Carat')


#table 函数则是统计所有因子对出现的情况的频数
head(sort(diamonds$carat))
head(sort(table(diamonds$carat)))

#绘制净度相关图像
install.packages("RColorBrewer")
library(RColorBrewer)
#'color = clarity'自动的区分clarity列的所有属性来绘图
ggplot(aes(carat, price, color = clarity), data = diamonds) + 
  geom_point(alpha = 0.4, size = 0.7, position = 'jitter') +
  #scale_color_brewer进行自定义颜色配置
  scale_color_brewer(type = 'div', 
                     #guide_legend函数，reverse是调整顺序
                     guide = guide_legend(title = 'Clarity', reverse = TRUE,
                                          #	override.aes指定图例键的美学参数的列表
                                          override.aes = list(alpha = 1, size = 2))) +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(.2, 3),
                     breaks = c(.2, .5, 1, 2, 3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price(log10) vs\nCube Root of Carat and Clarity')

#color 为cut时
ggplot(aes(carat, price, color = cut), data = diamonds) + 
  geom_point(alpha = 0.4, size = 0.7, position = 'jitter') +
  #scale_color_brewer进行自定义颜色配置
  scale_color_brewer(type = 'div', 
                     #guide_legend函数，reverse是调整顺序
                     guide = guide_legend(title = 'Clarity', reverse = TRUE,
                                          #	override.aes指定图例键的美学参数的列表
                                          override.aes = list(alpha = 1, size = 2))) +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(.2, 3),
                     breaks = c(.2, .5, 1, 2, 3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price(log10) vs\nCube Root of Carat and Cut')


#线性模型
m1 <- lm(I(log(price)) ~ I(carat ^ (1/3)), data = diamonds)
m2 <- update(m1, ~ . + carat)
m3 <- update(m2, ~ . + cut)
m4 <- update(m3, ~ . + color)
m5 <- update(m4, ~ . + clarity)
mtable(m1, m2, m3, m4, m5)

install.packages('RCurl')
library(RCurl)
install.packages('bitops')
library(bitops)

#下载数据
diamondsurl = getBinaryURL('https://raw.github.com/solomonm/diamonds-data/master/BigDiamonds.Rda')
load(rawConnection(diamondsurl))














