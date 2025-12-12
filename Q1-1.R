install.packages("openxlsx")

library(openxlsx)

# 1. 读入数据
dat <- read.xlsx("Q1_data.xlsx")

# 2. 初步规范化
dat$Sex <- trimws(dat$Sex)
dat$Sex <- tolower(dat$Sex)

# 3. 统一合法标签
dat$Sex[dat$Sex %in% c("m", "male")] <- "Male"
dat$Sex[dat$Sex %in% c("f", "female")] <- "Female"

# 4. 处理非法 / 无法判断的类别
dat$Sex[!dat$Sex %in% c("Male", "Female")] <- NA

# 5. 转成 factor
dat$Sex <- factor(dat$Sex, levels = c("Male", "Female"))

# 6. 按 sex 统计
table(dat$Sex, useNA = "ifany")

