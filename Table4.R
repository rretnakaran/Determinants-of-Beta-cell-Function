# Logistic regression models
## 1. Model I: Waist change and Matsuda index change
df1 <- df_logit[, c("stabilization", "Age", "Dura_Yr", "ISSI_2", "ISSI_2_chg", "Waist_chg", "IS_OGTTU_chg")]
df1 <- na.omit(df1)

m4 <- glm(stabilization ~ Age + Dura_Yr + log(ISSI_2) + ISSI_2_chg + Waist_chg + IS_OGTTU_chg, family = binomial(link = "logit"), data = df1)
sum4 <- summary(m4)

summary_tab4 <- data.frame(sum4$coefficients, exp(sum4$coefficients[,1]), exp(sum4$coefficients[,1]-1.96*sum4$coefficients[,2]),
                            exp(sum4$coefficients[,1]+1.96*sum4$coefficients[,2]))
names(summary_tab4) <- c("coef", "sd", "z value", "p value", "exp(coef)", "lower .95 CI", "upper .95 CI")
rownames(summary_tab4) <- c("Intercept", "Age", "Duration (yr)", "log base ISSI-2", "ISSI-2 base to 3w chg", "Waist 3w to 24m chg", "Matsuda 3w to 24m chg")

kable(summary_tab4, format = "html", caption = "Summary Table for Model I",
      digits = 4) %>%
  kable_paper(full_width = T)

### ROC
predicted_response <- predict(m4, type = "response")
par(pty = "s")
roc <- roc(df1$stabilization, predicted_response, legacy.axes = TRUE, smooth = T, direction = "<")
plot(roc, print.auc = T, legacy.axes = T, main = "ROC Curve")

cat("AIC of Model I: 91.097")


## 2. Model II: BMI change and Matsuda index change
df2 <- df_logit[, c("stabilization", "Age", "Dura_Yr", "ISSI_2", "ISSI_2_chg", "BMI_chg", "IS_OGTTU_chg")]
df2 <- na.omit(df2)

m3 <- glm(stabilization ~ Age + Dura_Yr + log(ISSI_2) + ISSI_2_chg + BMI_chg + IS_OGTTU_chg, family = binomial(link = "logit"), data = df2)
sum3 <- summary(m3)

summary_tab3 <- data.frame(sum3$coefficients, exp(sum3$coefficients[,1]), exp(sum3$coefficients[,1]-1.96*sum3$coefficients[,2]),
                            exp(sum3$coefficients[,1]+1.96*sum3$coefficients[,2]))
names(summary_tab3) <- c("coef", "sd", "z value", "p value", "exp(coef)", "lower .95 CI", "upper .95 CI")
rownames(summary_tab3) <- c("Intercept", "Age", "Duration (yr)", "log base ISSI-2", "ISSI-2 base to 3w chg", "BMI 3w to 24m chg", "Matsuda 3w to 24m chg")

kable(summary_tab3, format = "html", caption = "Summary Table for Model II",
      digits = 4) %>%
  kable_paper(full_width = T)

### ROC
predicted_response <- predict(m3, type = "response")
par(pty = "s")
roc <- roc(df2$stabilization, predicted_response, legacy.axes = TRUE, smooth = T, direction = "<")
plot(roc, print.auc = T, legacy.axes = T, main = "ROC Curve")

cat("AIC of Model II: 90.217")


## 3. Model III: BMI change and HOMA-IR change
df3 <- df_logit[, c("stabilization", "Age", "Dura_Yr", "ISSI_2", "ISSI_2_chg", "BMI_chg", "HOMA_IR_chg")]
df3 <- na.omit(df3)

m1 <- glm(stabilization ~ Age + Dura_Yr + log(ISSI_2) + ISSI_2_chg + BMI_chg + HOMA_IR_chg, family = binomial(link = "logit"), data = df3)
sum1 <- summary(m1)

summary_tab1 <- data.frame(sum1$coefficients, exp(sum1$coefficients[,1]), exp(sum1$coefficients[,1]-1.96*sum1$coefficients[,2]),
                            exp(sum1$coefficients[,1]+1.96*sum1$coefficients[,2]))
names(summary_tab1) <- c("coef", "sd", "z value", "p value", "exp(coef)", "lower .95 CI", "upper .95 CI")
rownames(summary_tab1) <- c("Intercept", "Age", "Duration (yr)", "log base ISSI-2", "ISSI-2 base to 3w chg", "BMI 3w to 24m chg", "HOMA-IR 3w to 24m chg")

kable(summary_tab1, format = "html", caption = "Summary Table for Model III",
      digits = 4) %>%
  kable_paper(full_width = T)

### ROC
predicted_response <- predict(m1, type = "response")
par(pty = "s")
roc <- roc(df3$stabilization, predicted_response, legacy.axes = TRUE, smooth = T, direction = "<")
plot(roc, print.auc = T, legacy.axes = T, main = "ROC Curve")

cat("AIC of Model III: 83.749")


## 4. Model IV: Model III + Average total physical activity over 2-years
df7 <- df_logit[, c("stabilization", "Age", "Dura_Yr", "ISSI_2", "ISSI_2_chg", "BMI_chg", "HOMA_IR_chg", "avg_phy_act")]
df7 <- na.omit(df7)

m7 <- glm(stabilization ~ Age + Dura_Yr + log(ISSI_2) + ISSI_2_chg + BMI_chg + HOMA_IR_chg + avg_phy_act, family = binomial(link = "logit"), 
          data = df7)
sum7 <- summary(m7)

summary_tab7 <- data.frame(sum7$coefficients, exp(sum7$coefficients[,1]), exp(sum7$coefficients[,1]-1.96*sum7$coefficients[,2]),
                            exp(sum7$coefficients[,1]+1.96*sum7$coefficients[,2]))
names(summary_tab7) <- c("coef", "sd", "z value", "p value", "exp(coef)", "lower .95 CI", "upper .95 CI")
rownames(summary_tab7) <- c("Intercept", "Age", "Duration (yr)", "log base ISSI-2", "ISSI-2 base to 3w chg", "BMI 3w to 24m chg", "HOMA-IR 3w to 24m chg",
                            "Average physical activity")

kable(summary_tab7, format = "html", caption = "Summary Table for Model IV",
      digits = 4) %>%
  kable_paper(full_width = T)

### ROC
predicted_response <- predict(m7, type = "response")
par(pty = "s")
roc <- roc(df7$stabilization, predicted_response, legacy.axes = TRUE, smooth = T, direction = "<")
plot(roc, print.auc = T, legacy.axes = T, main = "ROC Curve")

cat("AIC of Model IV: 54.364")


## 5. Model V: Model III + Change in ALTS form 3-weeks to 2-years
df8 <- df_logit[, c("stabilization", "Age", "Dura_Yr", "ISSI_2", "ISSI_2_chg", "BMI_chg", "HOMA_IR_chg", "ALTS_chg")]
df8 <- na.omit(df8)

m8 <- glm(stabilization ~ Age + Dura_Yr + log(ISSI_2) + ISSI_2_chg + BMI_chg + HOMA_IR_chg + ALTS_chg, family = binomial(link = "logit"), 
          data = df8)
sum8 <- summary(m8)

summary_tab8 <- data.frame(sum8$coefficients, exp(sum8$coefficients[,1]), exp(sum8$coefficients[,1]-1.96*sum8$coefficients[,2]),
                            exp(sum8$coefficients[,1]+1.96*sum8$coefficients[,2]))
names(summary_tab8) <- c("coef", "sd", "z value", "p value", "exp(coef)", "lower .95 CI", "upper .95 CI")
rownames(summary_tab8) <- c("Intercept", "Age", "Duration (yr)", "log base ISSI-2", "ISSI-2 base to 3w chg", "BMI 3w to 24m chg", "HOMA-IR 3w to 24m chg",
                            "change in ALTS from 3-weeks to 2-years")

kable(summary_tab8, format = "html", caption = "Summary Table for Model V",
      digits = 4) %>%
  kable_paper(full_width = T)

### ROC
predicted_response <- predict(m8, type = "response")
par(pty = "s")
roc <- roc(df8$stabilization, predicted_response, legacy.axes = TRUE, smooth = T, direction = "<")
plot(roc, print.auc = T, legacy.axes = T, main = "ROC Curve")

cat("AIC of Model V: 69.768")
