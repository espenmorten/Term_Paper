# TECH 6 Term paper
# Replication of Meyersson - ISLAMIC RULE AND THE EMPOWERMENT OF THE POOR AND PIOUS (2014)

This repository replicates tables 1, 2, 3 and 7 and figures 1-4 from Erik Meyersson's paper from 2014, called Islamic rule and the empowerment of the poor and pious, where he investigates whether electing an Islamic mayor in Turkey's 1994 municipal elections affected women's education. It uses a regression discontinuity design where municipalities where the Islamic candidate barely won are compared with the municipalities where the Islamic candidate barely lost. 

We also include 2 extensions:
1. **High school completion by age group in 2011** 
2. **Population growth and age composition from 1990 to 2000**

## Repository structure
<pre>
Term_Paper
├── README.md
├── run_all.R # Run this file to reproduce all figures, tables and extensions.
├── shared.R # Shared variables, data processing, functions and formatting rules
├── data/
│   └── regdata0.dta # Input data
├── scripts/
│   ├── figures.R # Figure 1, 2a, 2b, 3 and 4
│   ├── table1.R # Summary statistics
│   ├── table2.R # Islamic rule and high school completion
│   ├── table3.R # Alternative RD specifications
│   ├── table7.R # RD Treatment effects of islamic rule on education in 2011
│   ├── extension1.R # High school completion by age group in 2011
│   ├── extension2.R # Population growth and age composition from 1990 to 2000
└── files_produced/ # Output folder where tables, figures and extension output will end up.
</pre>

## Setup
1. Download and extract the repository ZIP, or fork this repository
2. Make sure 'regdata0.dta' is in the 'data/' folder
3. Open 'Term_Paper.Rproj' in RStudio
4. Run '00_setup.R' to install required packages.
5. Run 'run_all.R' to regenerate all tables, figures and extensions.

