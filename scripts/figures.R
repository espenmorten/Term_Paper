install.packages("haven") # Install package that can read stata files (.dta)
install.packages("estimatr")
install.packages("labelled")
install.packages("tidyverse")
install.packages("rddensity")
install.packages("rdrobust")
install.packages("modelsummary")
install.packages("gghighlight")
install.packages("rdd")

library(tidyverse)
library(estimatr)
library(haven)
library(labelled)
library(rddensity)
library(rdrobust)
library(modelsummary)
library(gghighlight)
library(rdd)

data <- read_dta("data/regdata0.dta")

x <- data$iwm94 # x is the running variable, access the specific column iwm94 with "$"
d <- data$i94 #d is the assignment status, access specific column i94 with $.
y <- data$hischshr1520f #y is the outcome, and we access the outcome for high school education for females ages 15-20.

complete_data <- data[complete.cases(x, d, y, data$ageshr19),]

#FIGURE 1

ggplot(complete_data, aes(x=vshr_islam1994, y=iwm94)) +
  geom_point(color=ifelse(abs(complete_data$vi1)<=0.02, "black", "azure4")) +
  xlim(0, 0.75) +
  labs(x="Islamic vote share", y="Islamic win margin") +
  geom_abline(intercept=-1, slope=2, color="black") +
  geom_abline(intercept=0, slope=0, color="azure3") +
  theme_classic()


#FIGURE 2

#Histogram

ggplot(complete_data, aes(x=vi1, y=after_stat(count/sum(count)))) +
  geom_histogram(binwidth = 0.02, fill = "azure4", color="black")+
  scale_y_continuous(labels=percent) +
  xlim(-1, 1) +
  labs(x="Islamic win margin", y="Percent") +
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_classic()

#Density test

mctest_data <- data[
  complete.cases(data$hischshr1520f, data$iwm94) &
    abs(data$iwm94)< 0.6, ] #The actual implementation only uses entries between -0.6 and 0.6 for the density test. 

density_data <- rdd_data(y=mctest_data$hischshr1520f, x=mctest_data$iwm94, cutpoint=0)
dens_test(density_data)

test_density <- dens_test(density_data)





