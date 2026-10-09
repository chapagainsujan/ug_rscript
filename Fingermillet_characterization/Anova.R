install.packages(tidyverse)

ggboxplot(FM$TSW)
shapiro.test(FM$TSW)
plotNormalHistogram(FM$TSW)
ggpubr::ggqqplot(FM$TSW)


####### Testing for Anova assumption ######
datamodel <- lm(newTSW ~ Landraces,data=FM)
ggqqplot(residuals(datamodel))
shapiro.test(residuals(datamodel))
###Homogenity of variance#####
FM %>% levene_test(TSW~Landraces)

####Test for anova assumption###
gvlma_result <- gvlma(datamodel)
summary(gvlma_result)

plotNormalHistogram(FM$TSW)
####Normalize data ######
bestNormalize(FM$TSW)
 #####Transformation####
newTSW<- sqrt_x(FM$TSW+0.5)



####### Testing for Anova assumption ######
datamodel1 <- lm(newTSW$x.t ~ Landraces,data=FM)
ggqqplot(residuals(datamodel1))
shapiro.test(residuals(datamodel1))
###Homogenity of variance#####
FM %>% levene_test(newTSW$x.t~Landraces)

####Test for anova assumption###
gvlma_result <- gvlma(datamodel1)
summary(gvlma_result)

plotNormalHistogram(FM$TSW)
####Normalize data ######
bestNormalize(FM$TSW)
#####Transformation####
newTSW<- sqrt_x(FM$TSW+0.5)
