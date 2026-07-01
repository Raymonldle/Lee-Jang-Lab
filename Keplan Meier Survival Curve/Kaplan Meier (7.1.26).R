# =========================
# LOAD BOTH DATASETS
# =========================

male_df <- read.csv("Cohort3_Mouse_Male_converted_survival_final.csv")
female_df <- read.csv("Cohort3_Mouse_Female_converted_survival_final.csv")

male_df$diet <- factor(male_df$diet, levels = c("Control","Inulin"))
female_df$diet <- factor(female_df$diet, levels = c("Control","Inulin"))

# =========================
# MALE PLOT
# =========================

male_fit <- survfit(Surv(time, status) ~ diet, data = male_df)

maleCurve <- ggsurvplot(
  male_fit,
  data = male_df,
  
  title = "Male Survival Curve",
  conf.int = FALSE,
  pval = TRUE,
  risk.table = TRUE,
  surv.scale = "percent",
  break.time.by = 3,
  
  xlab = "Months",
  ylab = "Survival probability (%)",
  palette = c("Control"="#000000","Inulin"="#00A5FF"),
  surv.median.line = "hv",
  
  font.main = c(21,"bold"),
  font.x = c(14),
  font.y = c(14),
  font.tickslab = c(12),
  
  risk.table.fontsize = 5,
  risk.table.height = 0.25,
  
  pval.size = 5,
  
  legend.title = "",
  legend.labs = c("Control","Inulin"),
  legend = "right",
  font.legend = 14
)

maleCurve$plot <- maleCurve$plot +
  theme(
    legend.position = c(1.0125,1.108),
    legend.justification = c(1,1),
    legend.background = element_blank(),
    legend.key = element_blank()
  )

maleCurve$table <- maleCurve$table +
  labs(title="Number at risk") +
  theme(plot.title = element_text(size=21,face="bold",hjust=0))

maleCurve_riskTable <- grid.arrange(
  maleCurve$plot,
  maleCurve$table,
  ncol=1,
  heights=c(3,1)
)

ggsave(
  "KM_Male_survival_Nature3.pdf",
  maleCurve_riskTable,
  width=7,
  height=6
)

ggsave(
  "KM_Male_survival_Nature3.png",
  maleCurve_riskTable,
  width=7,
  height=6,
  dpi=600
)

# =========================
# FEMALE
# =========================

female_fit <- survfit(Surv(time, status) ~ diet, data = female_df)

femaleCurve <- ggsurvplot(
  female_fit,
  data = female_df,
  
  title = "Female Survival Curve",
  conf.int = FALSE,
  pval = TRUE,
  risk.table = TRUE,
  surv.scale = "percent",
  break.time.by = 3,
  
  xlab = "Months",
  ylab = "Survival probability (%)",
  palette = c("Control"="#000000","Inulin"="#009A44"),
  surv.median.line = "hv",
  
  font.main = c(21,"bold"),
  font.x = c(14),
  font.y = c(14),
  font.tickslab = c(12),
  
  risk.table.fontsize = 5,
  risk.table.height = 0.25,
  
  pval.size = 5,
  
  legend.title = "",
  legend.labs = c("Control","Inulin"),
  legend = "right",
  font.legend = 14
)

femaleCurve$plot <- femaleCurve$plot +
  theme(
    legend.position = c(1.0125,1.108),
    legend.justification = c(1,1),
    legend.background = element_blank(),
    legend.key = element_blank()
  )

femaleCurve$table <- femaleCurve$table +
  labs(title="Number at risk") +
  theme(plot.title = element_text(size=21,face="bold",hjust=0))

femaleCurve_riskTable <- grid.arrange(
  femaleCurve$plot,
  femaleCurve$table,
  ncol=1,
  heights=c(3,1)
)

ggsave(
  "KM_Female_survival_Nature3.pdf",
  femaleCurve_riskTable,
  width=7,
  height=6
)

ggsave(
  "KM_Female_survival_Nature3.png",
  femaleCurve_riskTable,
  width=7,
  height=6,
  dpi=600
)

# Display plots
maleCurve_riskTable
femaleCurve_riskTable





