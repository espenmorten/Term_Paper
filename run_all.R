library(haven)
library(tidyverse)
library(estimatr)
library(gt)
library(rddtools)
library(patchwork)
library(gtsummary)
library(dplyr)
library(rdrobust)
library(scales)

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

# Replicating figures
source("scripts/figures.R")
ggsave(file.path(output_dir, 'figure1.png'), plot = fig1, width = 7, height = 5, dpi = 300)
ggsave(file.path(output_dir, 'figure2a_histogram.png'), plot = fig2a, width = 7, height = 5, dpi = 300)
save_density_plot(file.path(output_dir, 'figure2b.png'))
ggsave(file.path(output_dir, 'figure3.png'), plot = fig3, width = 10, height = 12, dpi = 300)
ggsave(file.path(output_dir, 'figure4.png'), plot = fig4, width = 10, height = 8, dpi = 300)

# Replicating tables
# TABLE I
source("scripts/table1.R")
gtsave(table1, file.path(output_dir, 'table1.html'))

# # TABLE II
source("scripts/table2.R")
gtsave(table2, file.path(output_dir, 'table2.html'))

# # TABLE III
source("scripts/table3.R")
gtsave(table3, file.path(output_dir, 'table3.html'))

# # TABLE VII
source("scripts/table7.R")
gtsave(table7, file.path(output_dir, 'table7.html'))

# Extensions
source("scripts/extension1.R")
gtsave(extension1_table, file.path(output_dir, "extension1.html"))

source("scripts/extension2.R")
gtsave(extension2_table, file.path(output_dir, "extension2.html"))