#Load packages

library('tidyverse')
library('wooldridge')
library('psych')

#Load in data

data('bwght')
??bwght

#Create a scatter plot for data in bwght

ggplot(data = bwght, aes(x = cigs, y = bwghtlbs)) +
  geom_point() + 
  labs(x = "Number of Cigarettes", y = "Birth weight(oz)")

#Get descriptive statistics on variables in bwght

DscStat <- select(bwght, faminc, bwghtlbs, motheduc, cigs)
describe(DscStat)

#Create regression using bwght and get summary results 

RegBwght <- lm(bwghtlbs ~ cigs + faminc + motheduc, data = bwght)
summary(RegBwght)

#Load the second dataset 

data('ceosal2')
??ceosal2

#Create a scatter plot w/ regression line for the data in ceosal2

ggplot(data = ceosal2, aes(x = lsales, y = lsalary)) +
  geom_point() +
  labs(x = "Log Firm Sales", y = "Log CEO Salary") + 
  geom_smooth(method = "lm", se = FALSE)

#Get descriptive statistics for ceosal2

DscStat2 <- select(ceosal2, salary, grad, sales, mktval)
describe(DscStat2)

#Create regression using ceosal2 and get summary results

RegCeosal2 <- lm(lsalary ~ lsales + lmktval + grad, data = ceosal2)
summary(RegCeosal2)
