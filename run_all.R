library(haven)
library(tidyverse)
library(estimatr)
library(gt)
library(rddtools)
library(patchwork)
library(gtsummary)
library(dplyr)

df <- read_dta('regdata0.dta')
source('shared.R')

# Replicating figures
#source("scripts/figures.R")

# Replicating tables
# TABLE I
source("scripts/table1.R")
gtsave(table1, 'table1.html')

# TABLE II
source("scripts/table2.R")
gtsave(table2, 'table2.html')

# TABLE III
source("scripts/table3.R")
gtsave(table3, 'table3.html')

# TABLE VII
source("scripts/table7.R")
gtsave(table7, 'table7.html')
