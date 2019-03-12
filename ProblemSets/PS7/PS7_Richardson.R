library(mice)
library(stargazer)
library(tidyverse)

wages <- read_csv("wages.csv")
View(wages)
wages1 <- wages %>% drop_na(hgc,tenure)
View(wages1)
stargazer(as.data.frame(wages1))

listwise <- as.data.frame(na.omit(wages1))
View(listwise)

listwise.reg <- lm(logwage ~ hgc + college + tenure + I(tenure^2) +age +married, wages1)
stargazer(listwise.reg)

wages.mean<-wages1

wages.mean$logwage[is.na(wages.mean$logwage)]<- mean(wages.mean$logwage,na.rm = TRUE)
View(wages.mean)
meanreg<- lm(logwage ~ hgc + college + tenure + tenure^2 +age +married,wages.mean)
summary(meanreg)

wages1$logwage_pred_imp<-wagedata$logwage
test<-lm(logwage ~ hgc + college +tenure + I(tenure^2) +age +married, data = wages1, na.action=na.exclude)
wages1$preds <-NA
wages1$preds[!is.na(wages1$hgc) & !is.na(wages1$tenure)] <- predict(test, wages1, na.action=na.exclude)
wages1$logwage_pred_imp[is.na(wages1$logwage)] <- wages1$preds[is.na(wages1$logwage)]

parms.regression.imp<-lm(logwage_pred_imp ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages1, na.action = na.omit)
print(summary(parms.regression.imp))

stargazer(listwise.reg, meanreg, parms.regression.imp)


fit2 = with(mean.imp, lm(logwage ~ hgc + college + tenure + tenure^2 +age +married))
View(fit2)
round(summary(pool(fit2)),2)
stargazer(as.data.frame(fit2))
