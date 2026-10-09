
#### Normality, ANOVA, and LSD ####

library(rcompanion)
library(ggpubr)
library(ggplot2)
library(agricolae)
library(rstatix)
library(gvlma)
library(plotrix)
library(dplyr)

#### Normality test for the original data ####
shapiro.test(SD$RL)
plotNormalHistogram(SD$RL)
ggpubr::ggqqplot(SD$RL)

#### ANOVA model for CRD (without REP) ####
datamodel1 <- lm(RL ~ GEN + TRT, data = SD)

#### Test normality of model residuals ####
ggpubr::ggqqplot(residuals(datamodel1))
shapiro.test(residuals(datamodel1))

#### Homogeneity of variance ####
SD %>% levene_test(RL ~ GEN)
SD %>% levene_test(RL ~ TRT)

#### Additional ANOVA assumption test ####
gvlma_result1 <- gvlma(datamodel1)
summary(gvlma_result1)

#### Log transformation (if required) ####
SD$log_RL <- log(SD$RL)

datamodel_log <- lm(log_RL ~ GEN + TRT, data = SD)

shapiro.test(residuals(datamodel_log))
ggpubr::ggqqplot(residuals(datamodel_log))

#### ANOVA for CRD ####
anova_model <- aov(RL ~ GEN + TRT + GEN:TRT, data = SD)
summary(anova_model)

#### LSD test for GEN ####
LSD_GEN <- LSD.test(anova_model, "GEN",
                    p.adj = "none", console = TRUE)
LSD_GEN

#### LSD test for TRT (moisture treatment) ####
LSD_TRT <- LSD.test(anova_model, "TRT",
                    p.adj = "none", console = TRUE)
LSD_TRT

#### LSD test for GEN × TRT interaction ####
LSD_INTERACTION <- LSD.test(anova_model,
                            c("GEN", "TRT"),
                            p.adj = "none",
                            console = TRUE)
LSD_INTERACTION
