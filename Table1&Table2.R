## 1. Characteristic comparison at baseline and induction IIT
df_base2 <- merge(x = df_base, y = Phys_sub2, by.x = c("StudyID", "Visit_new"), by.y = c("StudyID", "Visit_New"), all.x = T, all.y = F)
norm_var <- c("Age", "Dura_Yr", "Weight", "BMI", "Waist", "ALTS", "CREAS", "HBACC", "GL01", "GL02", "GL03", "GL04", "GL05", "Initial_basal", "Initial_meal",
              "Final_basal", "Final_meal", "GL02_GL01", "GL03_GL01", "GL04_GL01", "GL05_GL01", "AUC_GLUC_INC",
              "TOTAL_SCORE", "SPORT_INDEX", "LEISURE_INDEX", "WORK_INDEX")
skew_var <- c("IS_OGTTU","HOMA_IR", "ISSI_2", "INDEX_IR", "Cpep_Gluc", "ISR_Gluc", "A1c_Dura")
cat_var <- c("Sex", "Ethnicity_4grp", "AntiDM_Therapy", "Treatment")

rm_covsum(df_base2, covs = c(norm_var, skew_var, cat_var), digits = 2, include_missing = T)
rm_covsum(df_base2, covs = norm_var, maincov = "stabilization",digits = 2, pvalue =T, IQR = T, testcont = "ANOVA", include_missing = T, full = FALSE)
rm_covsum(df_base2, covs = skew_var, maincov = "stabilization",digits = 2, pvalue =T, IQR = T, include_missing = T, full = FALSE)
rm_covsum(df_base2, covs = cat_var, maincov = "stabilization",digits = 2, pvalue =T, IQR = T, include_missing = T, full = FALSE )


## 2. Characteristic comparison after 3-weeks of induction IIT
df_3w <- subset(df_final, Visit_new == 2)
norm_var <- c("Weight", "BMI", "Waist", "ALTS", "CREAS", "HBACC", "GL01", "GL02", "GL03", "GL04", "GL05")
skew_var <- c("IS_OGTTU","HOMA_IR", "ISSI_2", "INDEX_IR", "Cpep_Gluc", "ISR_Gluc")

rm_covsum(df_3w, covs = c(norm_var, skew_var), digits = 1, include_missing = T)
rm_covsum(df_3w, covs = norm_var, maincov = "stabilization",digits = 1, pvalue =T, IQR = T, testcont = "ANOVA", include_missing = T, full = FALSE)
rm_covsum(df_3w, covs = skew_var, maincov = "stabilization",digits = 2, pvalue =T, IQR = T, include_missing = T, full = FALSE)

## 3. Characteristic comparison for change from baseline to 3-weeks
df_base_3w <- subset(df_final, Visit_new %in% c(1, 2))
df_base_3w <- subset(df_base_3w, select = c(1, 2, 13:15, 19, 21:23, 25, 30, 33, 31, 32, 41, 42, 74))
df_wide <- reshape(df_base_3w, idvar = c("StudyID", "stabilization"), timevar = "Visit_new", direction = "wide")
var <- names(df_base_3w)[3:16]

for (i in 3:16){
  chg <- df_wide[,(i+14)] - df_wide[,(i)]
  df_wide <- cbind(df_wide, chg)
  names(df_wide)[28+i] <- paste0(var[i-2], "_chg")
}

var_list <- names(df_wide)[31:44]
rm_covsum(df_wide, covs = var_list, digits = 2, include_missing = T)
rm_covsum(df_wide, covs = var_list, maincov = "stabilization", digits = 2, pvalue =T, IQR = T, testcont = "ANOVA", include_missing = T, full = FALSE)
