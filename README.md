# TECH 6 Term paper
# Replication of Meyersson - ISLAMIC RULE AND THE EMPOWERMENT OF THE POOR AND PIOUS (2014)

This repository contains the code for replication of selected figures and tables from Meyersson's paper from 2014, called Islamic rule and the empowerment of the poor and pious, where he investigates whether electing an Islamic mayor in Turkey's 1994 municipal elections affected women's education. It uses a regression discontinuity design where municipalities where the Islamic candidate barely won are compared with the municipalities where the Islamic candidate barely lost. 

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
│   ├── extension1.R # Modern RD estimates for women's high school completion in 2000
│   ├── extension2.R # 2011 outcome availability and sensitivity to sample selection
└── files_produced/ # Output folder where tables, figures and extension output will end up.
</pre>

## Setup
1. Download or fork this repository
2. Extract the .zip file.
3. Open R studio
4. Click File -> Open Project...
5. Find the Term_Paper.proj and open
6. Run 00_setup.
7. Run run_all.R

