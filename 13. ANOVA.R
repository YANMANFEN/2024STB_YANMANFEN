# 필요한 패키지 로드
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# 데이터 준비
# 예제 데이터: 세 가지 광고 모델(model1, model2, model3)과 소비자 태도 점수
data <- data.frame(
  group = rep(c("model1", "model2", "model3"), each = 3),  # 처리 그룹
  response = c(4, 6, 8, 2, 4, 6, 3, 5, 7)  # 소비자 태도 점수
)

# 데이터 확인
print("데이터 미리보기:")
print(data)

# 데이터 시각화 (박스플롯)
boxplot(response ~ group, data = data, 
        main = "광고 모델이 소비자 태도에 미치는 영향",
        xlab = "광고 모델",
        ylab = "소비자 태도 점수",
        col = c("lightblue", "lightgreen", "lightpink"))

# 일원분산분석(One-Way ANOVA) 실행
anova_result <- aov(response ~ group, data = data)

# ANOVA 결과 출력
print("일원분산분석 결과:")
summary(anova_result)

# 그룹 간 평균 시각화
group_means <- aggregate(response ~ group, data = data, mean)
ggplot(data = group_means, aes(x = reorder(group, -response), y = response)) +
  geom_col(fill = "skyblue") +
  ggtitle("광고 모델 간 평균 비교") +
  xlab("광고 모델") +
  ylab("평균 소비자 태도 점수") +
  theme_minimal()

# 사후 분석(Tukey HSD) 실행
tukey_result <- TukeyHSD(anova_result)

# Tukey 사후 분석 결과 출력
print("Tukey 사후 분석 결과:")
print(tukey_result)

# 사후 분석 결과 시각화
plot(tukey_result)
