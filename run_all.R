library(haven)
library(tidyverse)
library(estimatr)
library(gt)
library(rddtools)
library(patchwork)
library(gtsummary)
library(dplyr)
library(rdrobust)

# Robust data ingestion
project_dir <- this.path::this.dir()
setwd(project_dir)

data_file <- file.path(project_dir, 'data', 'regdata0.dta')

if (!file.exists(data_file)) {
    stop('Could not find dataset at ', data_file, call = FALSE)
}

message('Reading data from ', data_file)
df <- read_dta(data_file)
source('shared.R')


output_dir <- file.path(project_dir, 'files_produced')
dir.create(output_dir)

# Replicating figures
# source("scripts/figures.R")


# Replicating tables
# TABLE I
# source("scripts/table1.R")
# gtsave(table1, file.path(output_dir, 'table1.html'))

# # TABLE II
source("scripts/table2.R")
gtsave(table2, file.path(output_dir, 'table2.html'))

# # TABLE III
# source("scripts/table3.R")
# gtsave(table3, file.path(output_dir, 'table3.html'))

# # TABLE VII
# source("scripts/table7.R")
# gtsave(table7, file.path(output_dir, 'table7.html'))

# Extensions
source("scripts/extension.R")
gtsave(extension1_table, file.path(output_dir, "extension1.html"))
# gtsave(extension2_table, file.path(output_dir, "extension2.html"))