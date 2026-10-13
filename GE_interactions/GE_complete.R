Syntax 

require(metan)
require(ggplot2)
require(GGEBiplots)
library(metan)
attach(Data_Rice_Copy_3_)
gge_model <- gge(Data_Rice_Copy_3_, ENV, GEN,GY)
predict(gge_model)


#Biplot type 1

b <- plot(gge_model,
             col.gen = "orange2",
             size.text.env = 2)
arrange_ggplot(b)

#Biplot type 2: Mean performance vs. stability
gge_model <- gge(modified, ENV, GEN, GY, svp = "genotype")

b <- plot(gge_model,
             type = 2,
             col.gen = "black",
             col.env = "brown",
             axis_expand = 0.5)
x=arrange_ggplot(b)
x+theme_classic()

#Biplot type 3: Which-won-where
gge_model <- gge(modified, ENV, GEN, GY, svp = "symmetrical")

b <- plot(gge_model,
             type = 3,
             size.shape.win = 0.5,
             large_label = 0.5,
             col.gen = "black",
             col.env = "brown",
             annotation = FALSE,
             title = FALSE)
z=arrange_ggplot(b)
z+theme_classic()

#Biplot type 6: Ranking environments
gge_model <-  gge(modified, ENV, GEN, GY, svp= "environment")

b <- plot(gge_model,
             type = 6,
             col.gen = "black",
             col.env = "brown",
             size.text.env = 5,
             axis_expand = 1.5)
r=arrange_ggplot(b)
r+theme_classic()

#Biplot type 8: Ranking genotypes
gge_model <- gge(modified, ENV, Gen, Grain yield, svp = "genotype")

b <- plot(gge_model,
             type = 8,
             col.gen = "black",
             col.env = "brown",
             size.text.gen = 4)
p=arrange_ggplot(b)
p+theme_classic()
