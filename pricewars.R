library(dplyr)
library(magrittr)
library(xlsx)
library(readr)
library(lattice)
library(ggplot2)
library(car)
sm <- supermarket
sm <- sm %>% mutate(Pricediff = coles - woolworths)
sm %>% group_by(`product type`) %>% summarise(Min = min(woolworths,na.rm = TRUE),
                 Q1 = quantile(woolworths, probs = .25, na.rm = TRUE),
                 Median = median(woolworths, na.rm = TRUE),
                 Q3 = quantile(woolworths, probs = .75, na.rm = TRUE),
                 Mean = mean(woolworths, na.rm = TRUE),
                 SD = sd(woolworths, na.rm = TRUE),
                 n = n(),
                 Missing = sum(is.na(woolworths)))
sm %>% group_by(`product type`) %>% summarise(Min = min(coles, na.rm = TRUE),
                 Q1 = quantile(coles, probs = .25, na.rm = TRUE),
                 Median = median(coles, na.rm = TRUE),
                 Q3 = quantile(coles, probs = .75, na.rm = TRUE),
                 Mean = mean(coles, na.rm = TRUE),
                 SD = sd(coles, na.rm = TRUE),
                 n = n(),
                 Missing = sum(is.na(coles)))
sm %>% group_by(`product type`) %>% summarise(Min = min(Pricediff, na.rm = TRUE),
                                              Q1 = quantile(Pricediff, probs = .25, na.rm = TRUE),
                                              Median = median(Pricediff, na.rm = TRUE),
                                              Q3 = quantile(Pricediff, probs = .75, na.rm = TRUE),
                                              Mean = mean(Pricediff, na.rm = TRUE),
                                              Sd = sd(Pricediff, na.rm = TRUE),
                                              n = n(),
                                              Missing = sum(is.na(Pricediff)))
sm %>% boxplot(Pricediff ~ `product type`, data = ., ylab = "Difference between Coles and woolworths", xlab = "Type of Product", col = "red", main = "Difference of Prices by types")
grid()
sm %>% group_by %>% summarise(Min = min(Pricediff, na.rm = TRUE),
                              Q1 = quantile(Pricediff, probs = .25, na.rm = TRUE),
                              Median = median(Pricediff, na.rm = TRUE),
                              Q3 = quantile(Pricediff, probs = .75, na.rm = TRUE),
                              Mean = mean(Pricediff, na.rm = TRUE),
                              SD = sd(Pricediff, na.rm = TRUE),
                              n = n(),
                              Missing = sum(is.na(Pricediff)))
sm$Pricediff %>% boxplot(., ylab = "difference", col = "blue", main = "difference between the price of coles and woolworths", ylim = c(-5,5))
grid()
plot(Pricediff ~ 'product name', data = sm, ylab = "difference", xlab = "products", main = "price vs pro")

qqPlot(sm$woolworths, dist = "norm", main = "qq plot for woolworths prices", ylim = c(0, 10))
qqPlot(sm$coles, dist = "norm", main = "qq plot for coles prices")
qqPlot(sm$Pricediff, dist = "norm", main = "qq plot for the difference of prices", ylim = c(0,2))
t.test(sm$coles, sm$woolworths, var.equal = FALSE, alternative = "less")
t.test(sm$coles, sm$woolworths, var.equal =  TRUE, alternative ="less")
t.test(sm$coles, sm$woolworths, var.equal = FALSE, alternative = "two.sided")

freezer<-sm%>%filter(`product type` == "Freezer")
t.test(freezer$coles, freezer$woolworths, var.equal = FALSE, alternative = "two.sided")

food<-sm%>%filter(`product type` == "Food")
t.test(food$coles, food$woolworths, var.equal = FALSE, alternative = "two.sided")

pantry<-sm%>%filter(`product type` == "Pantry")
t.test(pantry$coles, pantry$woolworths, var.equal = FALSE, alternative = "two.sided")

household <- sm%>%filter(`product type` == "Household")
t.test(household$coles, household$woolworths, var.equal = FALSE, alternative = "two.sided")

baby <- sm%>%filter(`product type` == "Baby")
t.test(baby$coles, baby$woolworths, var.equal = FALSE, alternative = "two.sided")

vegetables<-sm%>%filter(`product type` == "Vegetables")
t.test(vegetables$coles, vegetables$woolworths, var.equal = FALSE, alternative = "two.sided")

paired_ttest<- t.test(sm$coles, sm$woolworths, paired = TRUE, alternative = "two.sided")
single_ttest<- t.test(sm$Pricediff, mu = 0, alternative = "two.sided")

paired_ttest
single_ttest
