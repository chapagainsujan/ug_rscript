# Normalize the data
transformation2 <- bestNormalize(SD$NR)
transformation2
normalized_values <- transformation2$x.t
summary(normalized_values)

SD$NNR <- normalized_values
shapiro.test(SD$NNR)

SD$log_RL <- log(SD$RL)  # Using natural log (ln)
shapiro.test(SD$log_RL)
plotNormalHistogram(SD$log_RL)
ggpubr::ggqqplot(SD$log_RL)
datamodel <- lm(Data$FW~ Data$Landraces+Data$REP)
anova_model <- aov(datamodel)