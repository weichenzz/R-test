#查看当前路径
getwd()
#进入目标路径
setwd('E:/study/数据分析课件/r-project')
#查看文件
list.files()
#读取tsv文件，分割为'\t'
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
#查看pf中的列名称
names(pf)

#安装包
install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)
install.packages('ggplot2')
library(ggplot2)
#切换主题
theme_set(theme_minimal(25))
#绘制直方图
qplot(x = dob_day, data = pf) +
  #断点为1-31
  scale_x_continuous(breaks = 1:31) +
  #以’dob_month‘分割图层
  facet_wrap(~dob_month, ncol = 4)

#qplot(x = friend_count, data = pf, xlim = c(0,1000))
qplot(x= friend_count, data = subset(pf,!is.na(gender)), binwidth = 25) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender, ncol = 2)
#统计性别出现的频数
table(pf$gender)
by(pf$friend_count, pf$gender, summary)

#填充颜色
qplot(x = tenure, data = pf, binwidth = 30, color = I('black'), fill = I('#FF1493'))

qplot(x = tenure/365, xlab = 'Number of years using Facebook',
      ylab = 'Number of users in sample',data = pf, binwidth = .25, color = I('black'), fill = I('#FA1492')) +
  scale_x_continuous(limits = c(0, 7.5), breaks = seq(0, 7.5, 0.5))

qplot(x = age, data = pf, color = I('black'), fill = I('yellow'), binwidth = 1) +
  scale_x_continuous(limits = c(0, 113), breaks = seq(0, 113, 3))

qplot(x = friend_count, data = pf)
summary(pf$friend_count)
summary(log10(pf$friend_count + 1))
summary(sqrt(pf$friend_count))

install.packages('gridExtra')
library(gridExtra)


p1 <- qplot(x = friend_count, data = pf)
p2 <- qplot(x = log10(pf$friend_count + 1), data = pf)
p3 <- qplot(x = sqrt(pf$friend_count), data = pf)
#使用grid包来拼接图表
grid.arrange(p1,p2,p3,ncol = 1)

#绘制曲线图
qplot(x= friend_count, y = ..count../sum(..count..),
      data = subset(pf,!is.na(gender)), binwidth = 10,
      geom = 'freqpoly', color = gender) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))

by(pf$www_likes, pf$gender, sum)

#qplot(x = age, y = friend_count, data = pf)
ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_point(alpha = .05, position = position_jitter(h = 0), color = 'orange') + 
  #geom_jitter(alpha = .05) + 
  xlim(13, 90) +
  coord_trans(y = 'sqrt') +
  #使用geom_line函数绘制曲线
  geom_line(stat = 'summary', fun.y = mean) + 
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5), color = 'red')

install.packages('dplyr')
library(dplyr)

#拆分数据重新定义一个新的数据集
age_groups <- group_by(pf, age)
pf.fc_by_age <- summarise(age_groups, 
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),
                          n = n()
                          )
#排序
pf.fc_by_age <- arrange(pf.fc_by_age, age)
p1 <- ggplot(aes(x = age, y = friend_count_mean), data = subset(pf.fc_by_age, age < 71)) +
  geom_point() +
  geom_line()

#获取方法为pearson的相关系数
with(subset(pf, age <= 70), cor.test(age, friend_count, method = 'pearson'))

ggplot(aes(x = www_likes_received, y = likes_received), data = pf) + 
  geom_point() +
  #添加限制
  xlim(0, quantile(pf$www_likes_received, 0.95)) +
  ylim(0, quantile(pf$likes_received, 0.95)) +
  #平滑线
  geom_smooth(method = 'lm', color = 'red')
  
cor.test(pf$www_likes_received, pf$likes_received)

install.packages('alr3')
library(alr3)

data("Mitchell")

with(Mitchell, cor.test(Month, Temp))

ggplot(aes(x = Month, y = Temp), data = Mitchell) +
  geom_point() + 
  scale_x_continuous(breaks = seq(0, 203, 12))

pf$age_with_months <- pf$age + (12 - pf$dob_month) / 12

pf.fc_by_age_month <- group_by(pf, age_with_months)
pf.fc_by_age_month <- summarise(pf.fc_by_age_month,
                                friend_count_mean = mean(friend_count),
                                friend_count_median = median(friend_count),
                                n = n())
pf.fc_by_age_month <- arrange(pf.fc_by_age_month)
p2 <- ggplot(aes(x = age_with_months, y = friend_count_mean), 
       data = subset(pf.fc_by_age_month, age_with_months < 71)) +
  geom_line() +
  geom_smooth()

p3 <- ggplot(aes(x = round(age / 5) * 5, y = friend_count), 
             data = subset(pf, age < 71)) +
  geom_line(stat = 'summary', fun.y = mean)

grid.arrange(p1, p2, p3, ncol = 1)















