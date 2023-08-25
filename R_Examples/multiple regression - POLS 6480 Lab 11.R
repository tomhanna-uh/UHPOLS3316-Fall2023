#Clear the environment
rm(list=ls())

#Set Working Directory
setwd("C:/R Studio Files/Teaching/POLS6480-Fall2020-UH-lab/Lab 11")

UN <- read.csv("UN-data.csv")
simple <- lm(Fert ~ as.numeric(Cont), data = UN)
simple$coefficients
UN$Contracept <- as.numeric(as.character(UN$Cont))
correct <- lm(Fert ~ Contracept, data = UN)
correct$coefficients
predict(correct, data.frame(Contracept = c(13, 74, 84)))
summary(correct)

cor(UN$FemEc, UN$Contracept , use="complete.obs")
cor(UN$GDP, UN$FemEc , use="complete.obs")
cor(UN$GDP, UN$Contracept , use="complete.obs")
pairs(~ GDP + FemEc + Contracept, data = UN, pch=19)

labor.model <- lm(Fert ~ FemEc, data = UN)
summary(labor.model)
predict(labor.model, data.frame(FemEc = mean(UN$FemEc)))

multi.model <- lm(Fert ~ FemEc + Contracept, data=UN)
summary(multi.model)

med.model <- lm(Contracept ~ FemEc, data = UN)
summary(med.model)
postest <- UN[UN$Cont != "..", ]
postest$Indep <- med.model$residuals
alt.model <- lm(Fert ~ FemEc + Indep, data=postest)
summary(alt.model)

FemEc = c(51,80,51,80)
Contracept = c(56,56,77.5,77.5)
twobytwo = data.frame(cbind(FemEc, Contracept))
predict(multi.model, newdata = twobytwo)
