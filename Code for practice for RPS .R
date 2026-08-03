library(readxl)
attach(Data_for_practice)
View(Data_for_practice)
str(Data_for_practice)
###normality testing###
library(ggplot2)
library(ggpubr)
shapiro.test(Data_for_practice$ASI)
ggpubr::ggqqplot(Data name$variablename)
hist(dataname$variable name)

### data transformation###
##Log transformation
A=log(banana$`Final Tss`)
A
shapiro.test(A)
#### ANOVA TEST####
a=aov(ASI~trt+rep,data=Data_for_practice)
summary(a)
### LSD test###
dmrt=duncan.test(a,"trt", alpha=0.05, main = Data_for_practice)
dmrt

### TWO factor ANOVA####
ab=aov(banana$`Initial TSs`~rep+packa+chemical+chemical:Packaging, data=banana)
summary(ab)
#### LSD test####
dmrt=duncan.test(A1,"seedtrt", alpha=0.05, main=management_working_sheet2)
dmrt
dmrt=duncan.test(A1,"Fungi", alpha=0.05, main=management_working_sheet2)
dmrt
dmrt=duncan.test(A1,c("seedtrt", "Funfi"))
dmrt

#### Variability study####
data(vardata)
pheno.corr(Data_for_practice[3:6],Data_for_practice$trt,Data_for_practice$rep)

data
gen.var(Data_for_practice[3:6],Data_for_practice$trt,Data_for_practice$rep)

### correlation study ###
library(metan)
library(ggplot2)
AB=corr_coef(banana,4:9)
AB
plot(AB)

### Alpha lattic design###
model.ad<- aov( AD~ Rep + Entry + Rep:Bloc, data=data)
summary(model.ad) 
# display Type I ANOVA table
###Coefficeint of variation
sd(data$ AD, na.rm=TRUE)/ 
  mean(data$ AD, na.rm=TRUE)*100
require(agricolae)
LSD.test(AD, Entry, 16, 460.7,console=TRUE) 
###duncan.test(AD,Entry, 16, 460.7, alpha = 0.05, group=TRUE, main = NULL,console=TRUE)
###16=error df freedom###
###460.7 EMSS###