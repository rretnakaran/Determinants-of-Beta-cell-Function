# Code for generating the Kaplan Meier curve

df_base$Treatment <- as.factor(df_base$Treatment)
df_base <- within(df_base, Treatment <- relevel(Treatment, ref = "G2-Metformin"))
km_fit2 <- survfit(Surv(time = time, event = loss_stabilization) ~ factor(Treatment), data = df_base)
ggsurvplot(km_fit2, break.x.by = 3, pval = T, pval.method = T, risk.table = T, 
            title = "", xlab="Time (months)",legend= c(0.8,1),
            legend.title = "", color = "black", censor = F, linetype = c("solid", "dashed"),
            legend.labs = c("Metformin", "Metformin + IIT"), risk.table.height=0.3)
