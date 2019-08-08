getwd()
list.files()
yo_pf <- read.csv('yogurt.csv')
names(yo_pf)

#把int类型ID列变为factor类型
yo_pf$id <- factor(yo_pf$id)

#绘制直方图
qplot(data = yo_pf, x = price, fill = I('red'))

#transform()函数为原数据添加新的列
yo_pf <- transform(yo_pf, all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry)
summary(yo_pf)
#绘制直方图
qplot(x = all.purchases, data = yo_pf, binwidth = 1, fill = I('red'))

#使用抖动图
ggplot(aes(x = time, y = price), data = yo_pf) +
 geom_jitter(alpha = 1/4, shape = 21, fill = I('red'))

#set.seed()的作用是可以取相同的随机数，括号中的数字是一个key而并非运算次数
set.seed(420)
#levels()表示因子水平,因为yo_pf$id是一个factor类型
sample.ids <- sample(levels(yo_pf$id), 16)

yo_pf$all.purchases
# x %in% y：对于 x 中的每一个条目，该向量都会检查这一条目是否也出现在 y 中
ggplot(aes(x = time, y = price), data = subset(yo_pf,id %in% sample.ids)) +
 facet_wrap(~ id) +
 geom_line() +
#pch表示绘图时点的形状
 geom_point(aes(size = all.purchases), pch = 21)

library(GGally)

set.seed(1000)
pf_subset <- pf[, c(2:15)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset),1000), ])
