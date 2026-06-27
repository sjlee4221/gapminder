# clean.R
# Gapminder 데이터 품질 점검 → 마크다운 리포트 생성
# 사용법: Rscript clean.R  (결과: data/data_quality_report.md)

infile  <- "data/gapminder.csv"
outfile <- "data/data_quality_report.md"

con <- file(outfile, open = "w", encoding = "UTF-8")
w   <- function(...) cat(..., "\n", sep = "", file = con)

# 컬럼 타입을 명시해 읽기 (자동 추론 대신 의도한 타입으로 검증)
col_types <- c(
  country    = "character",
  year       = "integer",
  pop        = "numeric",
  continent  = "character",
  lifeExp    = "numeric",
  gdpPercap  = "numeric"
)
df <- read.csv(infile, colClasses = col_types, stringsAsFactors = FALSE)

w("# Gapminder 데이터 품질 점검 리포트\n")
w("> 생성: `clean.R` · 점검 대상: `", infile, "`\n")

# ---- 1. 기본 구조 ----------------------------------------------------
w("## 1. 기본 구조\n")
w("| 항목 | 값 |")
w("|------|-----|")
w("| 행 수 | ", nrow(df), " |")
w("| 열 수 | ", ncol(df), " |")
w("| 컬럼 | ", paste(names(df), collapse = ", "), " |\n")
w("**컬럼 타입**\n")
w("| 컬럼 | 타입 |")
w("|------|------|")
types <- sapply(df, class)
for (nm in names(types)) w("| ", nm, " | ", types[[nm]], " |")
w("")

# ---- 2. 결측치 ------------------------------------------------------
na_counts <- colSums(is.na(df))
w("## 2. 결측치(NA)\n")
w("| 컬럼 | 결측치 수 |")
w("|------|----------:|")
for (nm in names(na_counts)) w("| ", nm, " | ", na_counts[[nm]], " |")
w("| **총합** | **", sum(na_counts), "** |\n")

# ---- 3. 중복 행 ------------------------------------------------------
full_dups <- sum(duplicated(df))
key_dups  <- sum(duplicated(df[, c("country", "year")]))
w("## 3. 중복 점검\n")
w("| 점검 | 결과 |")
w("|------|-----:|")
w("| 완전 중복 행 | ", full_dups, " |")
w("| (country, year) 키 중복 | ", key_dups, " |\n")

# ---- 4. 값 범위 / 이상치 --------------------------------------------
w("## 4. 값 범위 점검\n")
num_cols <- c("year", "pop", "lifeExp", "gdpPercap")
w("| 변수 | 최소 | 1사분위 | 중앙값 | 평균 | 3사분위 | 최대 |")
w("|------|-----:|--------:|-------:|-----:|--------:|-----:|")
fmt <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
for (cn in num_cols) {
  q <- quantile(df[[cn]], probs = c(0, .25, .5, .75, 1))
  w("| ", cn, " | ", fmt(q[1]), " | ", fmt(q[2]), " | ", fmt(q[3]),
    " | ", fmt(mean(df[[cn]])), " | ", fmt(q[4]), " | ", fmt(q[5]), " |")
}
w("")

# 도메인 규칙 위반 점검
checks <- list(
  "lifeExp <= 0"             = which(df$lifeExp <= 0),
  "lifeExp > 120"            = which(df$lifeExp > 120),
  "pop <= 0"                 = which(df$pop <= 0),
  "gdpPercap <= 0"           = which(df$gdpPercap <= 0),
  "year 범위(1900~2100) 밖"  = which(df$year < 1900 | df$year > 2100)
)
w("**도메인 규칙 위반**\n")
w("| 규칙 | 위반 건수 |")
w("|------|----------:|")
any_violation <- FALSE
for (nm in names(checks)) {
  n <- length(checks[[nm]])
  w("| ", nm, " | ", n, " |")
  if (n > 0) any_violation <- TRUE
}
w("")

# ---- 5. 범주형 일관성 ------------------------------------------------
w("## 5. 범주형 값\n")
w("- 고유 국가 수: **", length(unique(df$country)), "**")
w("- 대륙 값: ", paste(sort(unique(df$continent)), collapse = ", "))
w("- 연도 값: ", paste(sort(unique(df$year)), collapse = ", "), "\n")

cont_per_country <- tapply(df$continent, df$country, function(x) length(unique(x)))
inconsistent <- names(cont_per_country[cont_per_country > 1])
w("국가별 대륙 분류 일관성: ",
  if (length(inconsistent) == 0) "**불일치 없음** ✔" else paste("⚠ 불일치:", paste(inconsistent, collapse = ", ")), "\n")

# ---- 6. 패널 균형성 -------------------------------------------------
w("## 6. 패널 완전성\n")
n_years <- length(unique(df$year))
obs_per_country <- table(df$country)
unbalanced <- obs_per_country[obs_per_country != n_years]
w("- 국가별 기대 관측 수: **", n_years, "** (연도 수 기준)")
if (length(unbalanced) == 0) {
  w("- 결과: 모든 국가가 동일한 연도 수를 가짐 → **완전 균형 패널** ✔\n")
} else {
  w("- 결과: ⚠ 관측 수가 다른 국가 존재\n")
  w("| 국가 | 관측 수 |")
  w("|------|--------:|")
  for (nm in names(unbalanced)) w("| ", nm, " | ", unbalanced[[nm]], " |")
  w("")
}

# ---- 7. 종합 결과 ----------------------------------------------------
w("## 7. 종합 결과\n")
problems <- (sum(na_counts) > 0) || (full_dups > 0) || (key_dups > 0) ||
            any_violation || (length(inconsistent) > 0) || (length(unbalanced) > 0)
if (problems) {
  w("⚠ **품질 이슈가 발견되었습니다.** 위 항목을 확인하세요.")
} else {
  w("✔ **모든 점검 통과** — 결측치/중복/이상치/일관성/패널 완전성에서 문제가 없습니다.")
}

close(con)
cat("품질 점검 리포트 생성 완료:", outfile, "\n")
