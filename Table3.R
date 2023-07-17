# 1. Characteristic comparison at 24-months
df_24m <- subset(df_final, Visit_new == 10)
Phys_sub2 <- subset(Phys_sub, Visit_New == 10)
df_24m2 <- merge(x = df_24m, y = Phys_sub2, by.x = c("StudyID", "Visit_new"), by.y = c("StudyID", "Visit_New"), all.x = T, all.y = F)

norm_var <- c("Weight", "BMI", "Waist", "ALTS", "CREAS", "HBACC", "GL01", "GL02", "GL03", "GL04", "GL05", "TOTAL_SCORE", "SPORT_INDEX", "LEISURE_INDEX", "WORK_INDEX")
skew_var <- c("IS_OGTTU","HOMA_IR", "ISSI_2", "INDEX_IR", "Cpep_Gluc", "ISR_Gluc")
rm_covsum(df_24m2, covs = c(norm_var, skew_var), digits = 2, include_missing = T)

rm_covsum(df_24m2, covs = norm_var, maincov = "stabilization",digits = 2, pvalue =T, IQR = T, testcont = "ANOVA", include_missing = T, full = FALSE)
rm_covsum(df_24m, covs = skew_var, maincov = "stabilization",digits = 2, pvalue =T, IQR = T, include_missing = T, full = FALSE)


# 2. Characteristic comparison for change from 3-weeks to 24-months
df_3w_24m <- subset(df_final, Visit_new %in% c(2, 10))
df_3w_24m <- subset(df_3w_24m, select = c(1, 2, 13:15, 19, 21:23, 25, 30, 33, 31, 32, 41, 42, 74))
df_wide <- reshape(df_3w_24m, idvar = c("StudyID", "stabilization"), timevar = "Visit_new", direction = "wide")
var <- names(df_3w_24m)[3:16]

for (i in 3:16){
  chg <- df_wide[,(i+14)] - df_wide[,(i)]
  df_wide <- cbind(df_wide, chg)
  names(df_wide)[28+i] <- paste0(var[i-2], "_chg")
}

var_list <- names(df_wide)[31:44]
rm_covsum(df_wide, covs = var_list, digits = 2, include_missing = T)
rm_covsum(df_wide, covs = var_list, maincov = "stabilization", digits = 2, pvalue =T, IQR = T, testcont = "ANOVA", include_missing = T, full = FALSE)
