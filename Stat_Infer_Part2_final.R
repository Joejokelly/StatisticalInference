library(ggplot2)
library(knitr)
library(datasets)

data(ToothGrowth)
str(ToothGrowth)
head(ToothGrowth, 4)
tail(ToothGrowth, 4)
summary(ToothGrowth)

suppl_mean = split(ToothGrowth$len, ToothGrowth$supp)
sapply(Suppl_mean, mean)
suppl_mean


##Basic Exploratory Analysis

ggplot(aes(x=supp, y=len), data=ToothGrowth) + geom_boxplot(aes(fill=supp))+ 
  xlab("Supplement Type") +ylab("Tooth length") 

unique(ToothGrowth$dose)

 ggplot(aes(x = factor(dose), y = len), data = ToothGrowth) + 
  geom_boxplot(aes(fill = factor(dose))) +
  ggtitle("Tooth length to Dosage")



 ggplot(aes(x=supp, y=len), data=ToothGrowth) + 
  geom_boxplot(aes(fill=supp)) + xlab("Supplements") + 
  ylab("Tooth Length") + facet_grid(~ dose) + 
  ggtitle("Tooth length by dosage for each supplement") 



t.test(len ~ supp, ToothGrowth[ToothGrowth$dose == .5, ])

t.test(len ~ supp, ToothGrowth[ToothGrowth$dose == 1, ])

t.test(len ~ supp, ToothGrowth[ToothGrowth$dose == 2, ])


