library(haven)
library(tidyverse)
library(estimatr)
library(gt)
library(rddtools)
library(patchwork)

df <- read_dta("../regdata0.dta")
source('./shared.R')

# Replicating figures
source("scripts/figures.R")

# Build table III and save the result in the repo root
source("scripts/table3.R")
gtsave(table3, 'table3.html')



# Replicating tables
source("scripts/table1.R")
source("scripts/table2.R")
source("scripts/table4.R")
