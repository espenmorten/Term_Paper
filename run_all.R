library(haven)
library(tidyverse)
library(estimatr)
library(gt)

df <- read_dta("../regdata0.dta")
source('./shared.R')

# Build table III and save the result in the repo root
source('scripts/table3.R')
gtsave(table3, 'table3.html')