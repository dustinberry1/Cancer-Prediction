breast_cancer_data <- read.csv("~/Maryville/R Programming/Week8/breast_cancer_data.csv")
  View(breast_cancer_data)

#load ggplot2
library(ggplot2)

#load pscl
library(pscl)

#test if diagnosis variable is a factor (strings converted to factors during import)
is.factor(breast_cancer_data$diagnosis)

#2 create the user defined function
BoxPlotPredictorOnTarget <- function(target, predictor) {
  ggplot(data=breast_cancer_data, aes_string(x=target, y=predictor)) +
    geom_boxplot(aes_string(col= target ), notch = TRUE)
}

#call the user defined function for each 
BoxPlotPredictorOnTarget("diagnosis","area_mean")
BoxPlotPredictorOnTarget("diagnosis","area_se")
BoxPlotPredictorOnTarget("diagnosis","texture_mean")

#3a forecast diagnosis based on area_mean
glm.a <- glm(diagnosis ~ area_mean, family = binomial, data = breast_cancer_data)
summary(glm.a)

#3b forecast diagnosis based on area_mean and area_se
glm.b <- glm(diagnosis ~ area_mean + area_se, family = binomial, data = breast_cancer_data)
summary(glm.b)

#3c forecast diagnosis based on area_mean, area_se, and texture_mean
glm.c <- glm(diagnosis ~ area_mean + area_se + texture_mean, family = binomial, data = breast_cancer_data)
summary(glm.c)

#3d forecast diagnosis based on area_mean, area_se, texture_mean, and concavity_worst
glm.d <- glm(diagnosis ~ area_mean + area_se + texture_mean + concavity_worst, family = binomial, data = breast_cancer_data)
summary(glm.d)

#3e forecast diagnosis based on area_mean, area_se, texture_mean, concavity_worst, and concavity_mean
glm.e <- glm(diagnosis ~ area_mean + area_se + texture_mean + concavity_worst + concavity_mean, family = binomial, data = breast_cancer_data)
summary(glm.e)

#compile McFadden/pseudo R squared for analysis
pR2(glm.a)
pR2(glm.b)
pR2(glm.c)
pR2(glm.d)
pR2(glm.e)

