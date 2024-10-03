library(readxl)
#2
table(survey$Gender)

#3
ECN <- table(survey$Gender)
prop.table(ECN)

#4
table(survey$Gender, survey $`Grade`)

#5
barplot(table(survey$Nationality))

#6
barplot(table(survey $`residential area`), xlab = "수량", ylab = "주거 지역", xlim=c(0,100), horiz=TRUE)

#7
cross_tab <- table(survey$Gender, survey$Grade)

# 打印交叉表
print(cross_tab)
# 绘制堆叠条形图
barplot(cross_tab, beside=FALSE, legend=TRUE, 
        col=c("blue", "red"),  # 可以更改颜色以更好地区分不同的年级
        main="Gender and Grade Distribution",
        xlab="Gender", ylab="Count")

#8
pie(table(survey $Gender))

#9
hist(survey$Age, main="age", col=terrain.colors(12))

#10
# 绘制每个 Grade 对应的 Age 的盒须图
boxplot(Age ~ Grade, data = survey, main="Grade 별 Age에 대한 기술적 분석", col="yellow", xlab="Grade", ylab="Age")

#11
plot(x=survey$Grade, y=survey$Age, xlab="grade", ylab="age", main="비교")