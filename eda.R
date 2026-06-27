# eda.R
# Gapminder 탐색적 데이터 분석 (EDA) — 개정판
# 사용법: Rscript eda.R
# 산출물:
#   - figures/*.png         (그래프)
#   - document/eda_report.md (EDA 보고서, 그래프 임베드)
#
# 개정 포인트 (초판 대비 보완):
#   1) 집계를 단순평균 + 인구가중평균 병기 (대표성 왜곡 교정)
#   2) 상관을 풀링(pooled) + 연도별로 분리 검증 (시점 혼입 점검)
#   3) 파생변수 totalGDP(=pop*gdpPercap) 도입, 인구 변수 본격 분석
#   4) 회귀 적합 + 잔차로 '소득 대비 이탈 국가' 식별
#   5) σ-수렴 분석(국가 간 분산의 시간 추이)
#   6) 성장률(CAGR)·최대 변동국 등 체계적 탐색
#   7) 대륙 내 이질성(변동계수 CV) 정량화
#   8) 한계·전제 명시 섹션 추가

suppressMessages(library(ggplot2))

infile  <- "data/gapminder.csv"
outdir  <- "document"
figdir  <- "figures"
figrel  <- "../figures"   # 보고서(document/)에서 그림을 참조하는 상대 경로
outfile <- file.path(outdir, "eda_report.md")
dir.create(figdir, recursive = TRUE, showWarnings = FALSE)
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# ---- 데이터 적재 + 파생변수 ----------------------------------------
df <- read.csv(infile, stringsAsFactors = FALSE)
df$continent <- factor(df$continent)
df$totalGDP  <- df$pop * df$gdpPercap        # 국가 총생산 (파생)
df$logGdp    <- log10(df$gdpPercap)          # 로그 소득 (분석용)
df$logPop    <- log10(df$pop)
years   <- sort(unique(df$year))
first_y <- min(years); latest <- max(years)
span    <- latest - first_y
d_new   <- df[df$year == latest, ]
d_old   <- df[df$year == first_y, ]

# ---- 출력/포맷 헬퍼 ------------------------------------------------
con <- file(outfile, open = "w", encoding = "UTF-8")
w   <- function(...) cat(..., "\n", sep = "", file = con)
fmt <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = ",")
wmean <- function(x, wt) sum(x * wt) / sum(wt)   # 가중 평균

theme_set(theme_minimal(base_size = 12))
save_plot <- function(p, name, caption = "", width = 8, height = 5) {
  ggsave(file.path(figdir, name), p, width = width, height = height, dpi = 110)
  cat("  saved:", file.path(figdir, name), "\n")
  w("![", caption, "](", file.path(figrel, name), ")\n")
  w("*", caption, "*\n")
}

cat("EDA 시작...\n")
w("# Gapminder 탐색적 데이터 분석 (EDA)\n")
w("> 생성: `eda.R` · 대상: `", infile, "` · 기간: ", first_y, "–", latest,
  " (", length(years), "개 시점) · 단위: 국가×연도\n")
w("본 보고서는 분포 → 관계 → 집단 비교 → 시간 추세 → 수렴 → 성장/충격 순으로 ",
  "데이터를 탐색하며, 각 단계의 **방법론적 함정**을 함께 점검합니다.\n")

# ====================================================================
# 0. 데이터 구조 요약
# ====================================================================
w("## 0. 데이터 구조\n")
w("| 항목 | 값 |")
w("|------|-----|")
w("| 관측치 | ", nrow(df), " (", length(unique(df$country)), "개국 × ", length(years), "시점) |")
w("| 결측치 | ", sum(is.na(df[, c("country","year","pop","continent","lifeExp","gdpPercap")])), " |")
w("| 변수 | lifeExp, pop, gdpPercap (+파생: totalGDP, logGdp) |")
w("| 패널 | 완전 균형(국가별 ", length(years), "시점) |\n")

# ====================================================================
# 1. 단변량 분포 (왜도·첨도 + 로그 효과)
# ====================================================================
w("## 1. 단변량 분포\n")
skew <- function(x) { x <- x - mean(x); mean(x^3) / (mean(x^2)^1.5) }
kurt <- function(x) { x <- x - mean(x); mean(x^4) / (mean(x^2)^2) - 3 }
w("왜도(skew)는 비대칭, 초과첨도(kurtosis)는 꼬리 두께를 나타냅니다. ",
  "0에 가까울수록 정규에 가깝습니다.\n")
w("| 변수 | 중앙값 | 평균 | 표준편차 | 왜도 | 초과첨도 |")
w("|------|------:|----:|--------:|----:|--------:|")
uni_vars <- list(lifeExp = df$lifeExp, pop = df$pop, gdpPercap = df$gdpPercap,
                 `log10(pop)` = df$logPop, `log10(gdp)` = df$logGdp)
for (nm in names(uni_vars)) {
  x <- uni_vars[[nm]]
  w("| ", nm, " | ", fmt(median(x)), " | ", fmt(mean(x)), " | ", fmt(sd(x)),
    " | ", fmt(skew(x), 2), " | ", fmt(kurt(x), 2), " |")
}
w("\n인구·GDP는 원본에서 강한 우편향(왜도 ≫ 0)이나, **로그 변환 후 왜도가 0 부근으로 정규화**됩니다. ",
  "따라서 이후 분석은 소득·인구에 로그 척도를 사용합니다.\n")

p1 <- ggplot(df, aes(lifeExp)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#2c7fb8", color = "white") +
  geom_density(color = "#08306b", linewidth = 0.8) +
  labs(title = "기대수명 분포 (전체 연도 풀링)", x = "기대수명", y = "밀도")
save_plot(p1, "01_hist_lifeExp.png", "그림 1. 기대수명 분포 — 좌측 꼬리(개도국·충격)가 존재해 약한 좌편향")

p2 <- ggplot(df, aes(gdpPercap)) +
  geom_histogram(bins = 30, fill = "#de2d26", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(title = "1인당 GDP 분포 (로그 스케일)", x = "1인당 GDP (log10)", y = "빈도")
save_plot(p2, "02_hist_gdp_log.png", "그림 2. 1인당 GDP — 로그 변환 시 근사적으로 대칭")

# ====================================================================
# 2. 분포의 시간 변화 (수렴/양극화 단서)
# ====================================================================
w("## 2. 기대수명 분포의 시간 변화\n")
w("연도별 분포 형태 변화를 보면 단순 평균 상승 외에 **양봉(개도국/선진국 분리) → 단봉으로의 수렴** ",
  "여부를 읽을 수 있습니다.\n")
p3 <- ggplot(df, aes(lifeExp, group = year, color = year)) +
  geom_density(linewidth = 0.7) +
  scale_color_viridis_c(option = "C") +
  labs(title = "기대수명 분포의 변화 (1952→2007)",
       subtitle = "초기의 양봉(개도국·선진국 분리)이 후기로 갈수록 단봉화",
       x = "기대수명", y = "밀도", color = "연도")
save_plot(p3, "03_density_lifeExp_byyear.png",
          "그림 3. 기대수명 분포의 연도별 이동 — 좌측 봉우리가 우측으로 흡수")

# ====================================================================
# 3. 대륙 비교: 중심 + 내부 이질성(분산/CV)
# ====================================================================
w("## 3. 대륙별 비교 — 중심과 내부 이질성 (", latest, ")\n")
w("중앙값만 보면 대륙 내부 편차를 놓칩니다. **변동계수(CV=표준편차/평균)**로 이질성을 함께 봅니다. ",
  "CV가 클수록 같은 대륙 안에서도 국가 간 차이가 큽니다.\n")
w("| 대륙 | 국가수 | 기대수명 중앙값 | 기대수명 CV | GDP 중앙값 | GDP CV |")
w("|------|------:|--------------:|-----------:|----------:|------:|")
cv <- function(x) sd(x) / mean(x)
for (cont in levels(df$continent)) {
  s <- d_new[d_new$continent == cont, ]
  w("| ", cont, " | ", nrow(s),
    " | ", fmt(median(s$lifeExp)), " | ", fmt(cv(s$lifeExp), 3),
    " | $", fmt(median(s$gdpPercap), 0), " | ", fmt(cv(s$gdpPercap), 3), " |")
}
w("\n아프리카는 기대수명 CV가 가장 커서 '대륙=단일 집단' 가정이 가장 위험합니다 ",
  "(보츠와나·리비아 vs 분쟁국의 공존).\n")

p4 <- ggplot(d_new, aes(continent, lifeExp, fill = continent)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1, show.legend = FALSE) +
  labs(title = paste0("대륙별 기대수명 분포 (", latest, ")"),
       subtitle = "박스=사분위, 점=개별 국가(내부 산포 확인)", x = NULL, y = "기대수명")
save_plot(p4, "04_box_lifeExp_continent.png", "그림 4. 대륙별 기대수명 — 점으로 내부 산포 표시")

p5 <- ggplot(d_new, aes(continent, gdpPercap, fill = continent)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1, show.legend = FALSE) +
  scale_y_log10(labels = scales::comma) +
  labs(title = paste0("대륙별 1인당 GDP 분포 (", latest, ", 로그)"),
       x = NULL, y = "1인당 GDP (log10)")
save_plot(p5, "05_box_gdp_continent.png", "그림 5. 대륙별 1인당 GDP — 아시아 내부 편차가 가장 큼")

# ====================================================================
# 4. 소득–수명 관계: 풀링 vs 연도별 상관 + 회귀·잔차
# ====================================================================
w("## 4. 소득과 기대수명의 관계\n")
r_raw <- cor(df$gdpPercap, df$lifeExp)
r_log <- cor(df$logGdp, df$lifeExp)
w("### 4-1. 상관계수 — 풀링은 함정\n")
w("전 연도를 섞은 **풀링 상관**은 시간 추세까지 흡수해 과대평가될 수 있습니다. ",
  "연도별로 끊어 보면 관계의 **안정성**을 확인할 수 있습니다.\n")
w("- 풀링(전체): 원본 r=**", fmt(r_raw, 3), "**, 로그 r=**", fmt(r_log, 3), "**\n")
w("| 연도 | r( log10(GDP), 기대수명 ) |")
w("|------|-------------------------:|")
yr_cor <- sapply(years, function(y) {
  s <- df[df$year == y, ]; cor(s$logGdp, s$lifeExp)
})
for (i in seq_along(years)) w("| ", years[i], " | ", fmt(yr_cor[i], 3), " |")
w("\n연도별 상관이 ", fmt(min(yr_cor), 2), "~", fmt(max(yr_cor), 2),
  " 범위에서 안정적이므로, 로그소득–수명 관계는 시점과 무관하게 견고합니다. ",
  "(풀링 r ", fmt(r_log, 3), "이 특정 연도에 의해 부풀려진 것이 아님)\n")

# 회귀 적합 + 잔차로 '소득 대비 이탈국' 식별
m <- lm(lifeExp ~ logGdp, data = d_new)
d_new$fit   <- predict(m)
d_new$resid <- residuals(m)
r2 <- summary(m)$r.squared
w("### 4-2. 회귀 적합과 잔차 — 소득으로 설명되지 않는 국가\n")
w("선형회귀 `기대수명 ~ log10(GDP)`의 결정계수 **R²=", fmt(r2, 3),
  "** → 기대수명 분산의 약 ", fmt(r2 * 100, 0), "%가 소득만으로 설명됩니다. ",
  "나머지는 보건·제도·역사적 충격의 몫입니다. 잔차가 큰 국가는 *소득 대비* 특이 사례입니다.\n")
ord <- order(d_new$resid)
under <- head(d_new[ord, c("country", "gdpPercap", "lifeExp", "resid")], 6)   # 소득 대비 단명
over  <- head(d_new[rev(ord), c("country", "gdpPercap", "lifeExp", "resid")], 6) # 소득 대비 장수
w("**소득 대비 기대수명이 낮은 국가(음의 잔차)** — 산유국·HIV 피해국 등\n")
w("| 국가 | 1인당 GDP | 실제 수명 | 잔차(세) |")
w("|------|---------:|--------:|--------:|")
for (i in seq_len(nrow(under)))
  w("| ", under$country[i], " | $", fmt(under$gdpPercap[i], 0),
    " | ", fmt(under$lifeExp[i]), " | ", fmt(under$resid[i], 1), " |")
w("\n**소득 대비 기대수명이 높은 국가(양의 잔차)** — 보건 효율 우수\n")
w("| 국가 | 1인당 GDP | 실제 수명 | 잔차(세) |")
w("|------|---------:|--------:|--------:|")
for (i in seq_len(nrow(over)))
  w("| ", over$country[i], " | $", fmt(over$gdpPercap[i], 0),
    " | ", fmt(over$lifeExp[i]), " | +", fmt(over$resid[i], 1), " |")
w("")

d_new$flag <- ifelse(d_new$resid < 0, "소득 대비 단명", "소득 대비 장수")
p6 <- ggplot(d_new, aes(gdpPercap, lifeExp)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40", linewidth = 0.8) +
  geom_point(aes(color = continent, size = pop), alpha = 0.75) +
  scale_x_log10(labels = scales::comma) +
  scale_size(range = c(1, 12), guide = "none") +
  labs(title = paste0("1인당 GDP vs 기대수명 + 회귀선 (", latest, ")"),
       subtitle = paste0("R²=", fmt(r2, 3), " · 회귀선 위/아래 = 소득 대비 장수/단명 · 점=인구"),
       x = "1인당 GDP (log10)", y = "기대수명", color = "대륙")
save_plot(p6, "06_scatter_fit.png", "그림 6. 소득–수명 회귀와 이탈 국가", width = 9)

# ====================================================================
# 5. 시간 추세: 단순평균 vs 인구가중평균
# ====================================================================
w("## 5. 전세계 추세 — 단순평균 vs 인구가중평균\n")
w("국가별 단순평균은 투발루와 중국을 동일 취급해 **실제 인류 경험을 왜곡**합니다. ",
  "인구가중평균을 병기하면 '평균적 국가' vs '평균적 사람'의 차이가 드러납니다.\n")
w("| 연도 | 기대수명(단순) | 기대수명(인구가중) | GDP(단순) | GDP(인구가중) |")
w("|------|-------------:|-----------------:|---------:|------------:|")
for (y in years) {
  s <- df[df$year == y, ]
  w("| ", y,
    " | ", fmt(mean(s$lifeExp)),
    " | ", fmt(wmean(s$lifeExp, s$pop)),
    " | $", fmt(mean(s$gdpPercap), 0),
    " | $", fmt(sum(s$totalGDP) / sum(s$pop), 0), " |")
}
le_simple_gap <- mean(d_new$lifeExp) - wmean(d_new$lifeExp, d_new$pop)
w("\n", latest, "년 기준 단순평균 기대수명이 인구가중보다 ", fmt(le_simple_gap, 1),
  "세 ", ifelse(le_simple_gap > 0, "높습니다", "낮습니다"),
  " — 인구 대국(중국·인도)이 평균 부근에 있어 가중 시 끌려 내려갑니다. ",
  "단순평균만 보고하면 소국 다수의 성취에 치우칩니다.\n")

agg <- aggregate(cbind(lifeExp, gdpPercap) ~ year + continent, data = df, FUN = mean)
p7 <- ggplot(agg, aes(year, lifeExp, color = continent)) +
  geom_line(linewidth = 1) + geom_point(size = 1.5) +
  labs(title = "대륙별 평균 기대수명 추세", x = "연도", y = "평균 기대수명", color = "대륙")
save_plot(p7, "07_trend_lifeExp.png", "그림 7. 대륙별 기대수명 추세 — 아시아의 빠른 추격", width = 9)

p8 <- ggplot(agg, aes(year, gdpPercap, color = continent)) +
  geom_line(linewidth = 1) + geom_point(size = 1.5) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "대륙별 평균 1인당 GDP 추세 (로그)", x = "연도",
       y = "평균 1인당 GDP (log10)", color = "대륙")
save_plot(p8, "08_trend_gdp.png", "그림 8. 대륙별 소득 추세 — 아프리카 정체 구간 확인", width = 9)

# ====================================================================
# 6. σ-수렴: 국가 간 격차는 줄고 있나?
# ====================================================================
w("## 6. 수렴 분석 (σ-convergence)\n")
w("'격차가 줄고 있나'는 이 데이터의 핵심 질문입니다. 연도별로 국가 간 **표준편차(분산)**가 ",
  "감소하면 수렴, 증가하면 양극화입니다. 척도가 다른 두 지표를 비교하기 위해 ",
  first_y, "년을 100으로 지수화했습니다.\n")
disp <- data.frame(
  year      = years,
  sd_life   = sapply(years, function(y) sd(df$lifeExp[df$year == y])),
  sd_loggdp = sapply(years, function(y) sd(df$logGdp[df$year == y]))
)
disp$idx_life   <- 100 * disp$sd_life   / disp$sd_life[1]
disp$idx_loggdp <- 100 * disp$sd_loggdp / disp$sd_loggdp[1]
w("| 연도 | 기대수명 SD | 로그GDP SD | 기대수명 분산지수 | 로그GDP 분산지수 |")
w("|------|----------:|----------:|----------------:|---------------:|")
for (i in seq_along(years))
  w("| ", years[i], " | ", fmt(disp$sd_life[i], 2), " | ", fmt(disp$sd_loggdp[i], 3),
    " | ", fmt(disp$idx_life[i], 0), " | ", fmt(disp$idx_loggdp[i], 0), " |")
conv_life <- disp$idx_life[nrow(disp)]
conv_gdp  <- disp$idx_loggdp[nrow(disp)]
w("\n**기대수명**은 분산지수가 ", fmt(conv_life, 0), "로 ",
  ifelse(conv_life < 100, "감소 → 국가 간 수명 격차가 좁혀짐(수렴)", "증가"), ". ",
  "**로그소득**은 ", fmt(conv_gdp, 0), "로 ",
  ifelse(conv_gdp < 100, "감소(소득도 수렴)", "오히려 증가 → 소득 격차는 좁혀지지 않음(양극화 경향)"),
  ". 즉 수명은 수렴하되 소득은 그렇지 않다는 비대칭이 핵심 발견입니다.\n")

disp_long <- rbind(
  data.frame(year = disp$year, idx = disp$idx_life,   지표 = "기대수명 분산"),
  data.frame(year = disp$year, idx = disp$idx_loggdp, 지표 = "로그소득 분산")
)
p9 <- ggplot(disp_long, aes(year, idx, color = 지표)) +
  geom_line(linewidth = 1) + geom_point(size = 1.5) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey60") +
  labs(title = "국가 간 분산의 추이 (1952=100)",
       subtitle = "100 미만 = 격차 축소(수렴), 초과 = 격차 확대",
       x = "연도", y = "분산 지수", color = NULL)
save_plot(p9, "09_convergence.png", "그림 9. σ-수렴 — 수명은 수렴, 소득은 비수렴", width = 9)

# ====================================================================
# 7. 성장과 충격: CAGR 최대 변동국 + 수명 역행 사례
# ====================================================================
w("## 7. 성장과 충격\n")
# 국가별 1952→2007 소득 연평균성장률(CAGR)
g <- merge(d_old[, c("country", "continent", "gdpPercap", "lifeExp")],
           d_new[, c("country", "gdpPercap", "lifeExp")],
           by = "country", suffixes = c("_old", "_new"))
g$continent <- as.character(g$continent)   # cat()에서 factor 코드 대신 라벨 출력
g$cagr      <- (g$gdpPercap_new / g$gdpPercap_old)^(1 / span) - 1
g$life_gain <- g$lifeExp_new - g$lifeExp_old
w("### 7-1. 1인당 GDP 연평균성장률(CAGR), ", first_y, "→", latest, "\n")
top_g <- head(g[order(-g$cagr), ], 5)
bot_g <- head(g[order(g$cagr), ], 5)
w("**최고 성장 5개국**\n")
w("| 국가 | 대륙 | CAGR | ", first_y, " GDP | ", latest, " GDP |")
w("|------|------|----:|--------:|--------:|")
for (i in seq_len(nrow(top_g)))
  w("| ", top_g$country[i], " | ", top_g$continent[i], " | ", fmt(top_g$cagr[i] * 100, 2),
    "% | $", fmt(top_g$gdpPercap_old[i], 0), " | $", fmt(top_g$gdpPercap_new[i], 0), " |")
w("\n**최저(역성장) 5개국**\n")
w("| 국가 | 대륙 | CAGR | ", first_y, " GDP | ", latest, " GDP |")
w("|------|------|----:|--------:|--------:|")
for (i in seq_len(nrow(bot_g)))
  w("| ", bot_g$country[i], " | ", bot_g$continent[i], " | ", fmt(bot_g$cagr[i] * 100, 2),
    "% | $", fmt(bot_g$gdpPercap_old[i], 0), " | $", fmt(bot_g$gdpPercap_new[i], 0), " |")
w("\n일부 산유·분쟁국은 55년간 1인당 소득이 **역성장**했습니다 (성장은 보편 법칙이 아님).\n")

# 수명 역행(직전 시점 대비 하락) — 체계적 탐색
df <- df[order(df$country, df$year), ]
df$delta <- ave(df$lifeExp, df$country, FUN = function(x) c(NA, diff(x)))
drops <- df[!is.na(df$delta) & df$delta < 0, ]
big   <- head(drops[order(drops$delta), c("country", "year", "delta", "continent")], 8)
big$continent <- as.character(big$continent)   # cat()에서 factor 코드 대신 라벨 출력
w("### 7-2. 기대수명 역행 사례\n")
w("직전 시점(5년) 대비 기대수명이 **하락**한 경우는 총 ", nrow(drops),
  "건이며, 그중 ", sum(drops$delta < -2), "건은 2세 이상 급락입니다.\n")
w("| 국가 | 연도 | 변화(세) | 대륙 | 추정 배경 |")
w("|------|-----:|--------:|------|------|")
bg <- c(Rwanda = "1994 제노사이드", Zimbabwe = "HIV/경제붕괴", Lesotho = "HIV/AIDS",
        Swaziland = "HIV/AIDS", Botswana = "HIV/AIDS", Cambodia = "크메르루주",
        Namibia = "HIV/AIDS", `South Africa` = "HIV/AIDS")
for (i in seq_len(nrow(big)))
  w("| ", big$country[i], " | ", big$year[i], " | ", fmt(big$delta[i], 1),
    " | ", big$continent[i], " | ", ifelse(is.na(bg[big$country[i]]), "—", bg[big$country[i]]), " |")
w("\n역행은 거의 전부 **사하라 이남 아프리카의 HIV/AIDS**와 **분쟁·기근**에 집중됩니다.\n")

p10 <- ggplot(g, aes(cagr * 100, life_gain, color = continent)) +
  geom_hline(yintercept = 0, color = "grey70") + geom_vline(xintercept = 0, color = "grey70") +
  geom_point(alpha = 0.75, size = 2) +
  labs(title = paste0("소득 성장 vs 수명 개선 (", first_y, "→", latest, ")"),
       subtitle = "우상단=둘 다 개선 / 좌하단=둘 다 악화",
       x = "1인당 GDP 연평균성장률 (%)", y = "기대수명 증가폭 (세)", color = "대륙")
save_plot(p10, "10_growth_vs_lifegain.png", "그림 10. 성장과 수명 개선의 동반 관계", width = 9)

# ====================================================================
# 8. 한계와 전제 (자기비판)
# ====================================================================
w("## 8. 분석의 한계와 전제\n")
w("탐색 결과를 인과적으로 해석하기 전 다음을 유의해야 합니다.\n")
w("1. **생태학적 오류**: 모든 지표는 국가 *집계값*입니다. ‘소득이 높으면 오래 산다’는 ",
  "국가 수준 패턴이며, 개인 수준 인과로 직결되지 않습니다.")
w("2. **국가 내 불평등 미반영**: 1인당 GDP·평균 기대수명은 분배를 숨깁니다. ",
  "같은 평균이라도 내부 격차는 천차만별입니다.")
w("3. **상관 ≠ 인과**: 회귀 R²=", fmt(r2, 3), "는 설명력일 뿐, 보건·교육·제도 등 ",
  "누락변수와 역인과(부유해서 건강 vs 건강해서 생산적) 가능성이 있습니다.")
w("4. **데이터 범위**: ", latest, "년에서 종료되어 최근 추세(코로나19 등)는 불포함. ",
  "5년 간격이라 시점 내 변동도 평활화됩니다.")
w("5. **풀링 상관의 함정**: 전 연도를 섞은 상관은 추세를 흡수합니다(본문 4-1에서 연도별로 검증).")
w("6. **단순평균의 대표성**: 국가 단순평균은 인구를 무시합니다(본문 5에서 가중평균 병기).")
w("7. **CAGR 민감도**: 시작·종료 두 시점만으로 계산해 중간 변동·기저효과에 민감합니다.\n")

# ====================================================================
# 9. 핵심 결론
# ====================================================================
w("## 9. 핵심 결론\n")
w("1. **분포**: 소득·인구는 로그 정규에 가까움 → 로그 척도가 표준.")
w("2. **관계**: 로그소득–수명 상관은 연도와 무관하게 견고(r≈", fmt(mean(yr_cor), 2),
  "), 단 소득은 수명 분산의 ", fmt(r2 * 100, 0), "%만 설명.")
w("3. **이탈 국가**: 산유국(고소득·상대적 단명)과 보건 효율국이 회귀선에서 크게 벗어남.")
w("4. **비대칭 수렴**: 기대수명은 수렴(격차 축소)하나 **소득 격차는 좁혀지지 않음** — 본 EDA의 핵심.")
w("5. **충격의 지리**: 수명 역행은 사하라 이남 HIV/AIDS·분쟁에 집중.")
w("6. **해석 주의**: 집계·평균·풀링의 함정을 8장에 명시 — 인과 결론은 개체수준 데이터 필요.\n")
w("> 그림 원본은 `", figdir, "/` 폴더(그림 1–10)에 있습니다.")

close(con)
cat("EDA 완료. 리포트:", outfile, "\n")
