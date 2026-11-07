install.packages("haven") # Install package that can read stata files (.dta)
install.packages("estimatr")
install.packages("labelled")
install.packages("tidyverse")
install.packages("rddensity")
install.packages("rdrobust")
install.packages("modelsummary")
install.packages("gghighlight")
install.packages("rddtools")
install.packages("patchwork")

library(tidyverse)
library(estimatr)
library(haven)
library(labelled)
library(rddensity)
library(rdrobust)
library(modelsummary)
library(gghighlight)
library(rddtools)
library(patchwork)

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
abline(v=0, lty=2, col="black")
title(
  xlab="Islamic win margin 1994",
  ylab="Density"
)


#FIGURE 3

#All our covariates
covariates <- c(
  "vshr_islam1994",
  "ageshr60",
  "ageshr19",
  "lpop1994",
  "sexr", 
  "anyc", 
  "partycount",
  "shhs"
)

covariate_labels <- c(
  vshr_islam1994="Islamic vote share",
  ageshr60= "Age 60+",
  ageshr19= "Age 19",
  lpop1994= "Log population",
  sexr= "Gender ratio",
  anyc= "Any center municipality",
  partycount= "Number of parties",
  shhs= "Household size"
)

#Empty list to insert plots
plots <- list()

for (i in covariates) {

cov_data <- data[
  complete.cases(data[[i]], data$iwm94), ]
cov_data$bin <- trunc(cov_data$iwm94*25/2) #Global bins in the original code is 25

#Meyersson shows separate averages for each side
cov_data$side <- cov_data$iwm94 > 0

#Grouping our data
grouped_data <- group_by(cov_data, side, bin)

means <- summarise(grouped_data, average_margin=mean(iwm94), average_vote=mean(.data[[i]]), .groups = "drop")
means <- means[abs(means$average_margin) < 0.3, ] #Limit points to between -0.3 and 0.3

#Plot
p <- ggplot(means, aes(x=average_margin, y=average_vote)) +
  geom_point(size=3, color="#242424", size=0.5)+
  geom_smooth(data=subset(cov_data, abs(iwm94)<0.3),
  aes(x=iwm94, y=.data[[i]], group=side),
  method="loess",
  se=TRUE,
  color="black",
  linewidth=0.5)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  labs(
    x=NULL,
    y=covariate_labels[[i]]
  )+
  theme_classic()

  if (i=="partycount") {
    p <- p+ labs(
    x="Islamic win margin",
    y=covariate_labels[[i]]
  )
  }
  else if (i=="shhs") {
    p <- p+ labs(
      x="Islamic win margin",
      y=covariate_labels[[i]]
  )
  }
  else {
    p <- p+ labs(
    x=NULL,
    y=covariate_labels[[i]]
  )+
  theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank()
      )
  }

plots[[i]] <- p
}
#DONE UNIVERSAL FOR LOOP FUNCTION
#Put together the plots into one
wrap_plots(plots, ncol=2)


#FIGURE 4 (KOPI)
#All our covariates
covariates_4 <- c(
  "hischshr1520f",
  "hischshr1520m",
  "c90hischshr1520f",
  "c90hischshr1520m"
)

main_names <- c(
  hischshr1520f= "a) Women in 2000",
  hischshr1520m= "b) Men in 2000",
  c90hischshr1520f= "c) Women in 1900",
  c90hischshr1520m= "d) Men in 1900"
)

#Empty list to insert plots
plots_fig4 <- list()

for (i in covariates_4) {

cov_data4 <- data[
  complete.cases(data[[i]], data$iwm94), ]
cov_data4$bin <- trunc(cov_data4$iwm94*25) #Global bins in the original code is 25

#Meyersson shows separate averages for each side
cov_data4$side <- cov_data4$iwm94 > 0

#Grouping our data
grouped_data <- group_by(cov_data4, side, bin)

means <- summarise(grouped_data, average_margin=mean(iwm94), average_vote=mean(.data[[i]]), .groups = "drop")
means <- means[abs(means$average_margin) < 0.5, ] #Limit points to between -0.3 and 0.3

#Plot
p <- ggplot(means, aes(x=average_margin, y=average_vote)) +
  geom_point(size=3, color="#242424")+
  geom_smooth(data=subset(cov_data, abs(iwm94)<0.5),
  aes(x=iwm94, y=.data[[i]], group=side),
  method="loess",
  se=TRUE,
  color="black",
  linewidth=0.5)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  labs(
    y="Share of 15-20-year-olds with High School Degree",
    x="Islamic win margin 1994",
    title=main_names[[i]]
  )+
  theme_classic()+
  coord_cartesian(ylim=c(-0.05, 0.25))

plots_fig4[[i]] <- p
}
#DONE UNIVERSAL FOR LOOP FUNCTION
#Put together the plots into one
wrap_plots(plots_fig4, ncol=2)+
plot_layout(axis_titles="collect")