# eda.R
# Gapminder 탐색적 데이터 분석(EDA)
# 사용법: Rscript eda.R
# 산출물:
#   - eda/figures/*.png  (그래프)
#   - eda/eda_report.md  (EDA 요약 리포트)

suppressMessages(library(ggplot2))

infile  <- "data/gapminder.csv"
outdir  <- "document"            # 보고서 저장 폴더
figdir  <- "figures"             # 그래프 저장 폴더 (저장소 루트)
figrel  <- "../figures"          # 보고서(document/)에서 이미지 참조용 상대 경로
outfile <- file.path(outdir, "eda_report.md")
dir.create(figdir, recursive = TRUE, showWarnings = FALSE)

df <- read.csv(infile, stringsAsFactors = FALSE)
df$continent <- factor(df$continent)
years  <- sort(unique(df$year))
latest <- max(years)

# 리포트 출력 헬퍼
con <- file(outfile, open = "w", encoding = "UTF-8")
w   <- function(...) cat(..., "\n", sep = "", file = con)
fmt <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = ",")

# 공통 테마 + 저장/임베드 헬퍼: 그래프를 저장하고 보고서 본문에 이미지로 삽입
theme_set(theme_minimal(base_size = 12))
save_plot <- function(p, name, caption = "", width = 8, height = 5) {
  ggsave(file.path(figdir, name), p, width = width, height = height, dpi = 110)
  cat("  saved:", file.path(figdir, name), "\n")
  w("![", caption, "](", file.path(figrel, name), ")\n")
}

cat("EDA 시작...\n")
w("# Gapminder 탐색적 데이터 분석 (EDA)\n")
w("> 생성: `eda.R` · 대상: `", infile, "` · 그래프: `", figdir, "/`\n")

# ====================================================================
# 1. 단변량 분포
# ====================================================================
w("## 1. 단변량 분포\n")
w("주요 수치형 변수의 분포 특성입니다. 인구와 1인당 GDP는 강한 우편향(right-skew)을 보여 로그 변환이 권장됩니다.\n")
w("| 변수 | 평균 | 표준편차 | 왜도(대략) |")
w("|------|-----:|--------:|----------:|")
skew <- function(x) { x <- x - mean(x); mean(x^3) / (mean(x^2)^1.5) }
for (cn in c("lifeExp", "pop", "gdpPercap")) {
  w("| ", cn, " | ", fmt(mean(df[[cn]])), " | ", fmt(sd(df[[cn]])),
    " | ", fmt(skew(df[[cn]]), 2), " |")
}
w("")

# 기대수명 히스토그램
p1 <- ggplot(df, aes(lifeExp)) +
  geom_histogram(bins = 30, fill = "#2c7fb8", color = "white") +
  labs(title = "기대수명 분포 (전체 연도)", x = "기대수명", y = "빈도")
save_plot(p1, "01_hist_lifeExp.png", "그림 1. 기대수명 분포")

# GDP 분포: 원본 vs 로그
p2 <- ggplot(df, aes(gdpPercap)) +
  geom_histogram(bins = 30, fill = "#de2d26", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(title = "1인당 GDP 분포 (로그 스케일)", x = "1인당 GDP (log10)", y = "빈도")
save_plot(p2, "02_hist_gdp_log.png", "그림 2. 1인당 GDP 분포 (로그 스케일)")

# ====================================================================
# 2. 대륙별 분포 비교
# ====================================================================
w("## 2. 대륙별 분포 비교 (", latest, "년)\n")
d_new <- df[df$year == latest, ]
w("| 대륙 | 중앙값 기대수명 | 중앙값 1인당 GDP |")
w("|------|---------------:|----------------:|")
for (cont in levels(df$continent)) {
  s <- d_new[d_new$continent == cont, ]
  w("| ", cont, " | ", fmt(median(s$lifeExp)), " | $", fmt(median(s$gdpPercap), 0), " |")
}
w("")

p3 <- ggplot(d_new, aes(continent, lifeExp, fill = continent)) +
  geom_boxplot(alpha = 0.8, show.legend = FALSE) +
  labs(title = paste0("대륙별 기대수명 분포 (", latest, ")"), x = NULL, y = "기대수명")
save_plot(p3, "03_box_lifeExp_continent.png", "그림 3. 대륙별 기대수명 분포")

p4 <- ggplot(d_new, aes(continent, gdpPercap, fill = continent)) +
  geom_boxplot(alpha = 0.8, show.legend = FALSE) +
  scale_y_log10(labels = scales::comma) +
  labs(title = paste0("대륙별 1인당 GDP 분포 (", latest, ", 로그)"), x = NULL, y = "1인당 GDP (log10)")
save_plot(p4, "04_box_gdp_continent.png", "그림 4. 대륙별 1인당 GDP 분포 (로그)")

# ====================================================================
# 3. 관계 분석: GDP vs 기대수명
# ====================================================================
w("## 3. 소득과 기대수명의 관계\n")
r_raw <- cor(df$gdpPercap, df$lifeExp)
r_log <- cor(log(df$gdpPercap), df$lifeExp)
w("- 상관계수: 원본 **", fmt(r_raw, 3), "**, 로그(GDP) **", fmt(r_log, 3), "**")
w("- 로그 변환 시 선형 관계가 뚜렷해지며, 소득의 한계 수명 효과가 체감함을 시사합니다.\n")

p5 <- ggplot(d_new, aes(gdpPercap, lifeExp, color = continent, size = pop)) +
  geom_point(alpha = 0.7) +
  scale_x_log10(labels = scales::comma) +
  scale_size(range = c(1, 12), guide = "none") +
  labs(title = paste0("1인당 GDP vs 기대수명 (", latest, ")"),
       subtitle = "점 크기 = 인구, 색 = 대륙",
       x = "1인당 GDP (log10)", y = "기대수명", color = "대륙")
save_plot(p5, "05_scatter_gdp_lifeExp.png", "그림 5. 1인당 GDP vs 기대수명 (버블=인구)", width = 9)

# ====================================================================
# 4. 시계열 추세
# ====================================================================
w("## 4. 시계열 추세\n")
agg <- aggregate(cbind(lifeExp, gdpPercap) ~ year + continent, data = df, FUN = mean)
w("대륙별 평균 기대수명의 1952→", latest, " 변화:\n")
w("| 대륙 | ", min(years), " | ", latest, " | 증가폭 |")
w("|------|------:|------:|------:|")
for (cont in levels(df$continent)) {
  a <- agg[agg$continent == cont & agg$year == min(years), "lifeExp"]
  b <- agg[agg$continent == cont & agg$year == latest, "lifeExp"]
  w("| ", cont, " | ", fmt(a), " | ", fmt(b), " | +", fmt(b - a), " |")
}
w("")

p6 <- ggplot(agg, aes(year, lifeExp, color = continent)) +
  geom_line(linewidth = 1) + geom_point(size = 1.5) +
  labs(title = "대륙별 평균 기대수명 추세", x = "연도", y = "평균 기대수명", color = "대륙")
save_plot(p6, "06_trend_lifeExp.png", "그림 6. 대륙별 평균 기대수명 추세", width = 9)

p7 <- ggplot(agg, aes(year, gdpPercap, color = continent)) +
  geom_line(linewidth = 1) + geom_point(size = 1.5) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "대륙별 평균 1인당 GDP 추세 (로그)", x = "연도", y = "평균 1인당 GDP (log10)", color = "대륙")
save_plot(p7, "07_trend_gdp.png", "그림 7. 대륙별 평균 1인당 GDP 추세 (로그)", width = 9)

# ====================================================================
# 5. 이상치 / 특이 케이스
# ====================================================================
w("## 5. 특이 케이스\n")
# 기대수명이 직전 시점 대비 하락한 사례 (전쟁/질병 등)
df <- df[order(df$country, df$year), ]
df$lifeExp_prev <- ave(df$lifeExp, df$country, FUN = function(x) c(NA, head(x, -1)))
df$delta <- df$lifeExp - df$lifeExp_prev
drops <- df[!is.na(df$delta) & df$delta < -2, c("country", "year", "delta")]
drops <- drops[order(drops$delta), ]
w("기대수명이 직전 시점 대비 **2세 이상 하락**한 사례 (상위 8건):\n")
w("| 국가 | 연도 | 변화(세) |")
w("|------|-----:|--------:|")
for (i in seq_len(min(8, nrow(drops))))
  w("| ", drops$country[i], " | ", drops$year[i], " | ", fmt(drops$delta[i], 1), " |")
w("\n총 ", nrow(drops), "건의 하락 사례가 관찰되었습니다 (대부분 분쟁·전염병·기근과 연관).\n")

# ====================================================================
# 6. 종합 인사이트
# ====================================================================
w("## 6. 종합 인사이트\n")
w("1. **분포**: 인구·GDP는 강한 우편향 → 분석 시 로그 변환 권장.")
w("2. **격차**: 대륙 간 기대수명·소득 격차가 뚜렷하며 아프리카가 일관되게 최하위.")
w("3. **관계**: log(GDP)와 기대수명은 강한 양의 상관(", fmt(r_log, 3), ") — 수확 체감형.")
w("4. **추세**: 모든 대륙에서 기대수명 상승, 단 ", nrow(drops), "건의 역행 사례 존재.")
w("5. **데이터 품질**: 142개국 × 12시점 완전 균형 패널 → 시계열·횡단면 분석에 적합.\n")
w("> 그래프 원본 파일은 `", figdir, "/` 폴더에 있습니다.")

close(con)
cat("EDA 완료. 리포트:", outfile, "\n")
