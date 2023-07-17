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
