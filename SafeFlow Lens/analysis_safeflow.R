# SafeFlow Lens 데이터 분석 코드

# 1. 패키지 설치 및 불러오기
packages <- c("readr", "dplyr", "ggplot2", "stringr")

for (p in packages) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# 2. 데이터 불러오기
df <- read_csv("data/safeflow_lens_publicdata_combined.csv",
               locale = locale(encoding = "UTF-8"))

# 3. 데이터 확인
print(head(df))
print(str(df))
print(table(df$facility_type))

# 4. 위험도 점수 계산
# accident_count: 사고건수
# casualty_count: 사상자수
# user_scale: 시설 이용 규모
# 같은 동에 사고다발지역이 연결된 경우 위험도가 높아지도록 계산

df <- df %>%
  mutate(
    accident_count = ifelse(is.na(accident_count), 0, accident_count),
    casualty_count = ifelse(is.na(casualty_count), 0, casualty_count),
    user_scale = ifelse(is.na(user_scale), 0, user_scale),
    
    risk_score =
      accident_count * 0.45 +
      casualty_count * 0.35 +
      log1p(user_scale) * 0.20
  )

# 5. 위험도 점수 확인
print(df %>% select(facility, facility_type, dong, accident_count, casualty_count, user_scale, risk_score) %>% head(20))

# 6. 결과 저장
write_csv(df, "data/safeflow_lens_analyzed.csv")

# 7. img 폴더가 없으면 생성
if (!dir.exists("img")) {
  dir.create("img")
}

# 그래프 1. 시설 유형별 평균 위험도
graph1 <- df %>%
  group_by(facility_type) %>%
  summarise(avg_risk = mean(risk_score, na.rm = TRUE)) %>%
  arrange(desc(avg_risk))

p1 <- ggplot(graph1, aes(x = reorder(facility_type, avg_risk), y = avg_risk)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "시설 유형별 평균 교통위험도",
    x = "시설 유형",
    y = "평균 위험도 점수"
  ) +
  theme_minimal(base_family = "sans")

ggsave("img/graph_facility_type_risk.png", p1, width = 8, height = 5)


# 그래프 2. 행정동별 평균 위험도 TOP 10
graph2 <- df %>%
  filter(!is.na(dong), dong != "") %>%
  group_by(dong) %>%
  summarise(avg_risk = mean(risk_score, na.rm = TRUE)) %>%
  arrange(desc(avg_risk)) %>%
  slice_head(n = 10)

p2 <- ggplot(graph2, aes(x = reorder(dong, avg_risk), y = avg_risk)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "행정동별 평균 교통위험도 TOP 10",
    x = "행정동",
    y = "평균 위험도 점수"
  ) +
  theme_minimal(base_family = "sans")

ggsave("img/graph_dong_risk_top10.png", p2, width = 8, height = 5)


# 그래프 3. 위험도 상위 생활시설 TOP 10
graph3 <- df %>%
  arrange(desc(risk_score)) %>%
  slice_head(n = 10)

p3 <- ggplot(graph3, aes(x = reorder(facility, risk_score), y = risk_score)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "위험도 상위 생활시설 TOP 10",
    x = "생활시설",
    y = "위험도 점수"
  ) +
  theme_minimal(base_family = "sans")

ggsave("img/graph_top10_facility.png", p3, width = 8, height = 5)


# 8. 웹사이트용 TOP 10 CSV 저장
top10 <- df %>%
  arrange(desc(risk_score)) %>%
  select(facility, facility_type, dong, near_accident_area, accident_type,
         accident_count, casualty_count, user_scale, risk_score) %>%
  slice_head(n = 10)

write_csv(top10, "data/safeflow_top10.csv")

print("분석 완료: img 폴더에 그래프 3개가 저장되었습니다.")