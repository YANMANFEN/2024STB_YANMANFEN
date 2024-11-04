#실습에 필요한 packages를 라이브러리에 등록
library(dplyr)
library(ggplot2)

#CSV형식의 파일 불러와서 congestion객체에 입력하고 구조 확인
str(congestion)

#변수의 이상치와 결측치 확인하고 처리
summary(congestion)
#결측치 갯수 확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))
#결측치가 있는 행을 제거한 새로운 데이터 프레임 생성
#6시 출발기차의 결측치를 제거
congestion1 <- congestion[!is.na(congestion$s0600),]
colSums(is.na(congestion1))
#23시 30분 출발기차의 결측치를 제거
congestion1 <- congestion1[!is.na(congestion1$s2330),]
colSums(is.na(congestion1))
#남은 결측치를 0으로 대체
congestion1[is.na(congestion1)] <- 0
colSums(is.na(congestion1))

#이상치 확인
ggplot(congestion1, aes(y=s0530))+
  geom_boxplot()
summary(congestion1$s0530)

#파생변수만들기
#1.지하철역의 하루 평균 혼잡도
congestion1$day_mean <- rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])

#데이터분석
#1.  수도권 지하철의 하루 평균 혼잡도
# 데이터프레임의 이름을 `congestion`으로 가정
# 그리고 열 이름은 "s0530", "s0600", "s0630", "s0700", "s0730", ..., "s2300"이라고 가정

# 데이터프레임의 열 이름과 데이터 유형을 확인
str(congestion)

# 특정 열을 숫자형으로 변환 (만약 숫자형이 아닌 경우)
cols_to_numeric <- c("s0530", "s0600", "s0630", "s0700", "s0730", "s0800", 
                     "s0830", "s0900", "s0930", "s1000", "s1030", "s1100", 
                     "s1130", "s1200", "s1230", "s1300", "s1330", "s1400", 
                     "s1430", "s1500", "s1530", "s1600", "s1630", "s1700", 
                     "s1730", "s1800", "s1830", "s1900", "s1930", "s2000", 
                     "s2030", "s2100", "s2130", "s2200", "s2230", "s2300",
                     "s2330")

congestion[cols_to_numeric] <- lapply(congestion[cols_to_numeric], as.numeric)

# 각 행의 평균값을 계산하고 새로운 열 `day_mean`에 저장
congestion$day_mean <- rowMeans(congestion[, cols_to_numeric], na.rm = TRUE)

# 결과의 첫 몇 행을 출력하여 계산 결과 확인
head(congestion)
#2. 호선별 하루평균혼잡도
# 호선별 하루평균혼잡도

# 데이터프레임의 이름을 `congestion`으로 가정
# 예를 들어 "type", "line", "station","station", "s0530", ..., "s0700"으로 가정

# 데이터프레임의 구조 확인
str(congestion)

# 시간대 열 이름을 벡터로 정의
time_columns <- c("s0530", "s0600", "s0630", "s0700", "s0730", "s0800", "s0830")

# 특정 열을 숫자형으로 변환 (필요한 경우)
congestion[time_columns] <- lapply(congestion[time_columns], as.numeric)

# 각 행의 평균 혼잡도 계산
congestion$day_mean <- rowMeans(congestion[, time_columns], na.rm = TRUE)

# dplyr 패키지 로드
library(dplyr)

# 호선별 생성 및 방향별 평균 혼잡도 계산
average_congestion_by_line <- congestion %>%
  group_by(line,station ) %>%
  summarise(average_day_mean = mean(day_mean, na.rm = TRUE))

# 결과 출력
print(average_congestion_by_line)

#2. 호선별 출근시간(07:00~09:00)의 혼잡도 평균
# 07:00부터 09:00까지의 시간대 열을 추출하여 각 호선의 해당 시간대 평균 혼잡도를 계산합니다.
morning_cols <- c("s0700", "s0730", "s0800", "s0830", "s0900")
line_morning_mean <- congestion %>%
  group_by(line) %>%
  summarise(morning_mean = mean(rowMeans(.[, morning_cols], na.rm = TRUE), na.rm = TRUE))

print("호선별 출근시간(07:00~09:00)의 혼잡도 평균:")
print(line_morning_mean)
#2-1. 호선별 출근시간(07:00~09:00)의 기술통계
line_morning_stats <- congestion %>%
  group_by(line) %>%
  summarise(
    morning_min = min(rowMeans(.[, morning_cols], na.rm = TRUE)),
    morning_max = max(rowMeans(.[, morning_cols], na.rm = TRUE)),
    morning_mean = mean(rowMeans(.[, morning_cols], na.rm = TRUE), na.rm = TRUE),
    morning_sd = sd(rowMeans(.[, morning_cols], na.rm = TRUE), na.rm = TRUE)
  )

print("호선별 출근시간(07:00~09:00)의 기술통계:")
print(line_morning_stats)
#2-2. 평균혼잡도가 가장 높은 시간대를 막대그래프로 그리기
# 각 시간대의 평균 혼잡도를 계산합니다.
time_cols <- cols_to_numeric
time_means <- colMeans(congestion[, time_cols], na.rm = TRUE)
time_means_df <- data.frame(Time = time_cols, MeanCongestion = time_means)

# 그래프 그리기
ggplot(time_means_df, aes(x = Time, y = MeanCongestion)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(title = "평균 혼잡도가 가장 높은 시간대", x = "시간대", y = "평균 혼잡도") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 각 호선의 평균 혼잡도 계산
line_daily_mean <- congestion %>%
  group_by(line) %>%
  summarise(line_mean = mean(day_mean, na.rm = TRUE))

# 평균 혼잡도가 가장 높은 호선 추출
highest_line <- line_daily_mean %>%
  filter(line_mean == max(line_mean)) %>%
  pull(line)

# 해당 호선의 각 역의 평균 혼잡도를 구하고 내림차순으로 정렬
highest_line_stations <- congestion %>%
  filter(line == highest_line) %>%
  group_by(station) %>%
  summarise(station_mean = mean(day_mean, na.rm = TRUE)) %>%
  arrange(desc(station_mean))

print(paste("평균 혼잡도가 가장 높은 호선에서 기여도가 높은 역:"))
print(highest_line_stations)

#2-3. 평균혼잡도가 가장 높은 호선에서 기여도가 높은 역
highest_line <- line_daily_mean %>%
  filter(line_mean == max(line_mean)) %>%
  pull(line)

# 해당 호선의 각 역의 평균 혼잡도를 구하고 내림차순으로 정렬합니다.
highest_line_stations <- congestion %>%
  filter(line == highest_line) %>%
  group_by(station) %>%
  summarise(station_mean = mean(day_mean, na.rm = TRUE)) %>%
  arrange(desc(station_mean))

print(paste("평균 혼잡도가 가장 높은 호선에서 기여도가 높은 역:"))
print(highest_line_stations)

#3.08시 지하철 혼잡도 범주화/범주별 빈도분석
congestion1 %>%
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%
  group_by(s80_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  select(s80_grade,n,pct)%>%
  arrange(desc(n))

#3-1. 호선별로 08시 지하철 혼잡도 범주화
congestion1 %>%
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%
  group_by(line, s80_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  filter(s80_grade=="caution")%>%
  select(line, s80_grade,n,pct)%>%
  arrange(desc(pct))%>%
  head(5)

#4. 호선별 퇴근시간(18:00~20:00)의 혼잡도 평균
# 18:00부터 20:00까지의 시간대 열을 벡터로 정의
evening_cols <- c("s1800", "s1830", "s1900", "s1930", "s2000")

# 각 호선의 해당 시간대 평균 혼잡도를 계산합니다.
line_evening_mean <- congestion %>%
  group_by(line) %>%
  summarise(evening_mean = mean(rowMeans(.[, evening_cols], na.rm = TRUE), na.rm = TRUE))

print("호선별 퇴근시간(18:00~20:00)의 혼잡도 평균:")
print(line_evening_mean)


#4-1. 호선별 퇴근시간(18:00~20:00)의 기술통계

# 18:00부터 20:00까지의 시간대 열을 벡터로 정의
evening_cols <- c("s1800", "s1830", "s1900", "s1930", "s2000")

# 각 호선의 퇴근 시간대 혼잡도의 기술 통계를 계산합니다.
line_evening_stats <- congestion %>%
  group_by(line) %>%
  summarise(
    evening_min = min(rowMeans(.[, evening_cols], na.rm = TRUE)),
    evening_max = max(rowMeans(.[, evening_cols], na.rm = TRUE)),
    evening_mean = mean(rowMeans(.[, evening_cols], na.rm = TRUE), na.rm = TRUE),
    evening_sd = sd(rowMeans(.[, evening_cols], na.rm = TRUE), na.rm = TRUE)
  )

print("호선별 퇴근시간(18:00~20:00)의 기술통계:")
print(line_evening_stats)


#4-2. 평균혼잡도가 가장 높은 시간대를 막대그래프로 그리기
# 모든 시간대의 평균 혼잡도를 계산합니다.
time_cols <- c("s0530", "s0600", "s0630", "s0700", "s0730", "s0800", "s0830",
               "s0900", "s0930", "s1000", "s1030", "s1100", "s1130", "s1200",
               "s1230", "s1300", "s1330", "s1400", "s1430", "s1500", "s1530",
               "s1600", "s1630", "s1700", "s1730", "s1800", "s1830", "s1900",
               "s1930", "s2000", "s2030", "s2100", "s2130", "s2200", "s2230",
               "s2300", "s2330")

# 각 시간대의 평균 혼잡도를 계산합니다.
time_means <- colMeans(congestion[, time_cols], na.rm = TRUE)

# 데이터 프레임으로 변환
time_means_df <- data.frame(Time = time_cols, MeanCongestion = time_means)

# 평균 혼잡도가 가장 높은 순서로 정렬합니다.
time_means_df <- time_means_df[order(-time_means_df$MeanCongestion), ]

# 막대그래프 그리기
library(ggplot2)
ggplot(time_means_df, aes(x = reorder(Time, -MeanCongestion), y = MeanCongestion)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "평균 혼잡도가 가장 높은 시간대", x = "시간대", y = "평균 혼잡도") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#4-3. 평균혼잡도가 가장 높은 호선에서 기여도가 높은 역
# 각 호선의 평균 혼잡도 계산
line_daily_mean <- congestion %>%
  group_by(line) %>%
  summarise(line_mean = mean(day_mean, na.rm = TRUE))

# 평균 혼잡도가 가장 높은 호선 추출
highest_line <- line_daily_mean %>%
  filter(line_mean == max(line_mean)) %>%
  pull(line)

# 해당 호선의 각 역의 평균 혼잡도를 구하고 내림차순으로 정렬
highest_line_stations <- congestion %>%
  filter(line == highest_line) %>%
  group_by(station) %>%
  summarise(station_mean = mean(day_mean, na.rm = TRUE)) %>%
  arrange(desc(station_mean))

print(paste("평균 혼잡도가 가장 높은 호선에서 기여도가 높은 역:"))
print(highest_line_stations)