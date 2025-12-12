install.packages("openxlsx")

library(openxlsx)

# 1. 读入数据
dat <- read.csv("merged_all_tables.csv")

# 2. 初步规范化
dat$Gender <- trimws(dat$Gender)
dat$Gender <- tolower(dat$Gender)

# 3. 统一合法标签
dat$Gender[dat$Gender %in% c("m", "male")] <- "Male"
dat$Gender[dat$Gender %in% c("f", "female")] <- "Female"

# 4. 处理非法 / 无法判断的类别
dat$Gender[!dat$Gender %in% c("Male", "Female")] <- NA

# 5. 转成 factor
dat$Gender <- factor(dat$Gender, levels = c("Male", "Female"))

# 6. 按 sex 统计
table(dat$Gender, useNA = "ifany")
# =========================
# Belief 变量清洗
# =========================

# 1. 去首尾空格
dat$Belief <- trimws(dat$Belief)

# 2. 转小写，方便统一判断
dat$Belief <- tolower(dat$Belief)

# 3. 合法取值统一
dat$Belief[dat$Belief %in% c("y", "yes", "true", "t", "1")] <- "Yes"
dat$Belief[dat$Belief %in% c("n", "no", "false", "f", "0")] <- "No"

# 4. 无法判断 / 非法取值设为 NA
dat$Belief[!dat$Belief %in% c("Yes", "No")] <- NA

# 5. 转为 factor（明确水平顺序）
dat$Belief <- factor(dat$Belief, levels = c("No", "Yes"))
table(dat$Belief, useNA = "ifany")
# =========================
# PH & MH 变量清洗
# =========================

# ---------- PH ----------

# 1. 去首尾空格
dat$PH <- trimws(dat$PH)

# 2. 转小写
dat$PH <- tolower(dat$PH)

# 3. 统一合法取值
dat$PH[dat$PH %in% c("y", "yes", "true", "t", "1")] <- "Yes"
dat$PH[dat$PH %in% c("n", "no",  "false", "f", "0")] <- "No"

# 4. 其他非法值设为 NA
dat$PH[!dat$PH %in% c("Yes", "No")] <- NA

# 5. 转为 factor
dat$PH <- factor(dat$PH, levels = c("No", "Yes"))


# ---------- MH ----------

# 1. 去首尾空格
dat$MH <- trimws(dat$MH)

# 2. 转小写
dat$MH <- tolower(dat$MH)

# 3. 统一合法取值
dat$MH[dat$MH %in% c("y", "yes", "true", "t", "1")] <- "Yes"
dat$MH[dat$MH %in% c("n", "no",  "false", "f", "0")] <- "No"

# 4. 其他非法值设为 NA
dat$MH[!dat$MH %in% c("Yes", "No")] <- NA

# 5. 转为 factor
dat$MH <- factor(dat$MH, levels = c("No", "Yes"))

table(dat$PH, useNA = "ifany")
table(dat$MH, useNA = "ifany")
# =========================
# Smoker 变量清洗（Yes / No）
# =========================

# 1. 去首尾空格
dat$Smoker <- trimws(dat$Smoker)

# 2. 转小写
dat$Smoker <- tolower(dat$Smoker)

# 3. 统一合法取值
dat$Smoker[dat$Smoker %in% c("y", "yes", "current", "smoker", "true", "t", "1")] <- "Yes"
dat$Smoker[dat$Smoker %in% c("n", "no", "non-smoker", "false", "f", "0")] <- "No"

# 4. 其他无法判断的设为 NA
dat$Smoker[!dat$Smoker %in% c("Yes", "No")] <- NA

# 5. 转为 factor
dat$Smoker <- factor(dat$Smoker, levels = c("No", "Yes"))
table(dat$Smoker, useNA = "ifany")
fit_q1 <- glm(
  Belief ~ PH + MH,
  data = dat,
  family = binomial
)

summary(fit_q1)
fit_q1_adj <- glm(
  Belief ~ PH + MH + Gender + Smoker,
  data = dat,
  family = binomial
)

summary(fit_q1_adj)
# =========================
# 合并 PH 和 MH → Ongoing health issue
# =========================

# 新变量初始化为 NA
dat$Ongoing <- NA

# 情况 1：只要 PH 或 MH 有一个是 Yes
dat$Ongoing[dat$PH == "Yes" | dat$MH == "Yes"] <- "Yes"

# 情况 2：PH 和 MH 都是 No
dat$Ongoing[dat$PH == "No" & dat$MH == "No"] <- "No"

# 转为 factor
dat$Ongoing <- factor(dat$Ongoing, levels = c("No", "Yes"))
fitted_q1_adj<-glm(
  Belief ~ Ongoing,
  data = dat,
  family = binomial
)
summary(fitted_q1_adj)
# unadjusted prevalence
mean(dat$Ongoing == "Yes", na.rm = TRUE) * 100
# Region 频数
table(dat$Region)

# Region 比例
prop.table(table(dat$Region))
chisq.test(table(dat$Region))
1


