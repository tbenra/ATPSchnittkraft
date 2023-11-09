#install.packages("tidyverse")

library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(readxl)

library(here)

Messungen <- read_excel("Gesamt.xlsx", sheet = "Gesamt")

plot(Messungen$`Vorschub`, Messungen$Schnitt, col="blue",lty=1 ,xlab = "Vorschub[mm/u]", ylab = "Druck [N/mm^2]", pch ="X", ylim = c(-50, 300))
abline(lm(Messungen$Schnitt ~ Messungen$Vorschub), col="blue")
arrows(x0 = Messungen$Vorschub, y0 = Messungen$Schnitt, x1 = Messungen$Vorschub, y1= Messungen$Schnitt+Messungen$SigS, code=3, angle = 90, length = 0.05)
arrows(x0 = Messungen$Vorschub, y0 = Messungen$Schnitt, x1 = Messungen$Vorschub, y1= Messungen$Schnitt-Messungen$SigS, code=3, angle = 90, length = 0.05)

points(Messungen$`Vorschub`, Messungen$Vorschubk, col="red", pch ="x")
abline(lm(Messungen$Vorschubk ~ Messungen$Vorschub), col="red")
arrows(x0 = Messungen$Vorschub, y0 = Messungen$Vorschubk, x1 = Messungen$Vorschub, y1= Messungen$Vorschubk+Messungen$SigV, code=3, angle = 90, length = 0.05)
arrows(x0 = Messungen$Vorschub, y0 = Messungen$Vorschubk, x1 = Messungen$Vorschub, y1= Messungen$Vorschubk-Messungen$SigV, code=3, angle = 90, length = 0.05)




points(Messungen$Vorschub, Messungen$Passiv, col="green", pch ="x")
abline(lm(Messungen$Passiv ~ Messungen$Vorschub), col="green")
arrows(x0 = Messungen$Vorschub, y0 = Messungen$Passiv, x1 = Messungen$Vorschub, y1= Messungen$Passiv+Messungen$SigP, code=3, angle = 90, length = 0.05)
arrows(x0 = Messungen$Vorschub, y0 = Messungen$Passiv, x1 = Messungen$Vorschub, y1= Messungen$Passiv-Messungen$SigP, code=3, angle = 90, length = 0.05)

summary(lm(Messungen$`Vorschub`~ Messungen$Schnitt))
summary(lm(Messungen$`Vorschub`~ Messungen$Vorschubk))
summary(lm(Messungen$Vorschub~ Messungen$Passiv))