install.packages("openxlsx")

library(openxlsx)

dat <- read.csv("merged_all_tables.csv")

dat$Gender <- trimws(dat$Gender)
dat$Gender <- tolower(dat$Gender)


dat$Gender[dat$Gender %in% c("m", "male")] <- "Male"
dat$Gender[dat$Gender %in% c("f", "female")] <- "Female"

dat$Gender[!dat$Gender %in% c("Male", "Female")] <- NA

dat$Gender <- factor(dat$Gender, levels = c("Male", "Female"))


table(dat$Gender, useNA = "ifany")

dat$Belief <- trimws(dat$Belief)


dat$Belief <- tolower(dat$Belief)

dat$Belief[dat$Belief %in% c("y", "yes", "true", "t", "1")] <- "Yes"
dat$Belief[dat$Belief %in% c("n", "no", "false", "f", "0")] <- "No"


dat$Belief[!dat$Belief %in% c("Yes", "No")] <- NA


dat$Belief <- factor(dat$Belief, levels = c("No", "Yes"))
table(dat$Belief, useNA = "ifany")

dat$PH <- trimws(dat$PH)


dat$PH <- tolower(dat$PH)

dat$PH[dat$PH %in% c("y", "yes", "true", "t", "1")] <- "Yes"
dat$PH[dat$PH %in% c("n", "no",  "false", "f", "0")] <- "No"


dat$PH[!dat$PH %in% c("Yes", "No")] <- NA


dat$PH <- factor(dat$PH, levels = c("No", "Yes"))



dat$MH <- trimws(dat$MH)


dat$MH <- tolower(dat$MH)


dat$MH[dat$MH %in% c("y", "yes", "true", "t", "1")] <- "Yes"
dat$MH[dat$MH %in% c("n", "no",  "false", "f", "0")] <- "No"

dat$MH[!dat$MH %in% c("Yes", "No")] <- NA


dat$MH <- factor(dat$MH, levels = c("No", "Yes"))

table(dat$PH, useNA = "ifany")
table(dat$MH, useNA = "ifany")

dat$Smoker <- trimws(dat$Smoker)

dat$Smoker <- tolower(dat$Smoker)

dat$Smoker[dat$Smoker %in% c("y", "yes", "current", "smoker", "true", "t", "1")] <- "Yes"
dat$Smoker[dat$Smoker %in% c("n", "no", "non-smoker", "false", "f", "0")] <- "No"

dat$Smoker[!dat$Smoker %in% c("Yes", "No")] <- NA

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


dat$Ongoing <- NA


dat$Ongoing[dat$PH == "Yes" | dat$MH == "Yes"] <- "Yes"

dat$Ongoing[dat$PH == "No" & dat$MH == "No"] <- "No"


dat$Ongoing <- factor(dat$Ongoing, levels = c("No", "Yes"))
fitted_q1_adj<-glm(
  Belief ~ Ongoing,
  data = dat,
  family = binomial
)
summary(fitted_q1_adj)
mean(dat$Ongoing == "Yes", na.rm = TRUE) * 100

table(dat$Region)

prop.table(table(dat$Region))
chisq.test(table(dat$Region))


