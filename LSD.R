shapiro.test(Data$PH)
plotNormalHistogram(Data$PH)
ggpubr::ggqqplot(Data$PH)

####### Testing for Anova assumption ######
datamodel1 <- lm(Data$PH ~ Data$Landraces+Data$REP)
ggqqplot(residuals(datamodel1))
shapiro.test(residuals(datamodel1))

###Homogenity of variance#####
Data %>% levene_test(Data$PH ~ Landraces)

####Test for anova assumption###
gvlma_result1 <- gvlma(datamodel1)
summary(gvlma_result1)
datamodel <- lm(Data$PH~ Data$Landraces+Data$REP)
anova_model <- aov(datamodel)
summary(anova_model)
LSD_MODEL<-LSD.test (Data$PH, Data$Landraces,36, 58.36 )
LSD_MODEL
std.error(Data$PH)



####ET####
shapiro.test(Data$ET)
plotNormalHistogram(Data$ET)
ggpubr::ggqqplot(Data$ET)

####### Testing for Anova assumption ######
datamodel1 <- lm(Data$PH ~ Data$Landraces+Data$REP)
ggqqplot(residuals(datamodel1))
shapiro.test(residuals(datamodel1))

###Homogenity of variance#####
Data %>% levene_test(Data$PH ~ Landraces)

####Test for anova assumption###
gvlma_result1 <- gvlma(datamodel1)
summary(gvlma_result1)
datamodel <- lm(Data$ET~ Data$Landraces+Data$REP)
anova_model <- aov(datamodel)
summary(anova_model)
LSD_MODEL<-LSD.test (Data$ET, Data$Landraces,36, 0.4800 )
LSD_MODEL
std.error(Data$ET)


####FLA#####
shapiro.test(Data$FLA)
plotNormalHistogram(Data$FLA)
ggpubr::ggqqplot(Data$FLA)

####### Testing for Anova assumption ######
datamodel1 <- lm(Data$PH ~ Data$Landraces+Data$REP)
ggqqplot(residuals(datamodel1))
shapiro.test(residuals(datamodel1))

###Homogenity of variance#####
Data %>% levene_test(Data$PH ~ Landraces)

####Test for anova assumption###
gvlma_result1 <- gvlma(datamodel1)
summary(gvlma_result1)
datamodel <- lm(Data$FLA~ Data$Landraces+Data$REP)
anova_model <- aov(datamodel)
summary(anova_model)
LSD_MODEL<-LSD.test (Data$FLA, Data$Landraces,36, 10.06 )
LSD_MODEL
std.error(Data$FLA)

#####FW####
shapiro.test(Data$FW)
plotNormalHistogram(Data$FW)
ggpubr::ggqqplot(Data$FW)

####### Testing for Anova assumption ######
datamodel1 <- lm(Data$PH ~ Data$Landraces+Data$REP)
ggqqplot(residuals(datamodel1))
shapiro.test(residuals(datamodel1))

###Homogenity of variance#####
Data %>% levene_test(Data$PH ~ Landraces)

####Test for anova assumption###
gvlma_result1 <- gvlma(datamodel1)
summary(gvlma_result1)
datamodel <- lm(Data$FW~ Data$Landraces+Data$REP)
anova_model <- aov(datamodel)
summary(anova_model)
LSD_MODEL<-LSD.test (Data$FW, Data$Landraces,36, 0.006419 )
LSD_MODEL
std.error(Data$FW)

#####NF#####


shapiro.test(Data$NF)
plotNormalHistogram(Data$NF)
ggpubr::ggqqplot(Data$NF)

####### Testing for Anova assumption ######
datamodel1 <- lm(Data$NF ~ Data$Landraces+Data$REP)
ggqqplot(residuals(datamodel1))
shapiro.test(residuals(datamodel1))

###Homogenity of variance#####
Data %>% levene_test(Data$PH ~ Landraces)

####Test for anova assumption###
gvlma_result1 <- gvlma(datamodel1)
summary(gvlma_result1)
datamodel <- lm(Data$NF~ Data$Landraces+Data$REP)
anova_model <- aov(datamodel)
summary(anova_model)
LSD_MODEL<-LSD.test (Data$NF, Data$Landraces,36, 1.091 )
LSD_MODEL
std.error(Data$NF)


#####EHW#####


shapiro.test(Data$EHW)
plotNormalHistogram(Data$EHW)
ggpubr::ggqqplot(Data$EHW)

####### Testing for Anova assumption ######
datamodel1 <- lm(Data$EHW ~ Data$Landraces+Data$REP)
ggqqplot(residuals(datamodel1))
shapiro.test(residuals(datamodel1))

###Homogenity of variance#####
Data %>% levene_test(Data$EHW ~ Landraces)

####Test for anova assumption###
gvlma_result1 <- gvlma(datamodel1)
summary(gvlma_result1)
datamodel <- lm(Data$EHW ~ Data$Landraces+Data$REP)
anova_model <- aov(datamodel)
summary(anova_model)
LSD_MODEL<-LSD.test (Data$EHW, Data$Landraces,36, 1.106 )
LSD_MODEL
library(plotrix)
std.error(Data$EHW)




#####YPH#####


shapiro.test(Data$YPH)
plotNormalHistogram(Data$YPH)
ggpubr::ggqqplot(Data$YPH)

####### Testing for Anova assumption ######
datamodel1 <- lm(Data$YPH ~ Data$Landraces+Data$REP)
ggqqplot(residuals(datamodel1))
shapiro.test(residuals(datamodel1))

###Homogenity of variance#####
Data %>% levene_test(Data$YPH ~ Landraces)

####Test for anova assumption###
gvlma_result1 <- gvlma(datamodel1)
summary(gvlma_result1)
datamodel <- lm(Data$YPH ~ Data$Landraces+Data$REP)
anova_model <- aov(datamodel)
summary(anova_model)
LSD_MODEL<-LSD.test (Data$YPH, Data$Landraces,36, 1.1491 )
LSD_MODEL
library(plotrix)
std.error(Data$YPH)


####Harvst index#####


shapiro.test(Data$HI)

plotNormalHistogram(Data$HI)
ggpubr::ggqqplot(Data$HI)

####### Testing for Anova assumption ######
datamodel1 <- lm(Data$YPH ~ Data$Landraces+Data$REP)
ggqqplot(residuals(datamodel1))
shapiro.test(residuals(datamodel1))

###Homogenity of variance#####
Data %>% levene_test(Data$YPH ~ Landraces)

####Test for anova assumption###
gvlma_result1 <- gvlma(datamodel1)
summary(gvlma_result1)
datamodel <- lm(Data$HI ~ Data$Landraces+Data$REP)
anova_model <- aov(datamodel)
summary(anova_model)
LSD_MODEL<-LSD.test (Data$HI, Data$Landraces,36, 34.00 )
LSD_MODEL
library(plotrix)
std.error(Data$HI)

####Days to 50% Flowering#####
shapiro.test(Data$`Days 50% Flowering`)
plotNormalHistogram(Data$`Days 50% Flowering`)
ggpubr::ggqqplot(Data$`Days 50% Flowering`)
logflow <-log_x(Data$`Days 50% Flowering`)
logflow
Final <- log(Data$`Days 50% Flowering`)
Final 

shapiro.test(Final)
plotNormalHistogram(Final)
####### Testing for Anova assumption ######
datamodel1 <- lm(Data$`Days 50% Flowering` ~ Data$Landraces+Data$REP)
ggqqplot(residuals(datamodel1))
shapiro.test(residuals(datamodel1))

###Homogenity of variance#####
Data %>% levene_test(Data$`Days 50% Flowering` ~ Landraces)

####Test for anova assumption###
gvlma_result1 <- gvlma(datamodel1)
summary(gvlma_result1)


# Normalize the data
transformation2 <- bestNormalize(Data$`Days 50% Flowering`)
transformation2
normalized_values <- transformation2$x.t
summary(normalized_values)
datamodel <- lm(Data$FW~ Data$Landraces+Data$REP)
anova_model <- aov(datamodel)
summary(anova_model)
LSD_MODEL<-LSD.test (Data$FW, Data$Landraces,36, 0.006419 )
LSD_MODEL
std.error(Data$FW)

