packages <-c(
        'haven', # Read .dta files
        'tidyverse', # Prepare data
        'gt', # Create and format tables
        'gtsummary', # Create and format tables
        'estimatr', # Run regressions
        'rddtools', # Density test and other regression diagnostics
        'patchwork', # Combine plots into one figure
        'rdrobust', # RD estimation for the extensions
        'this.path', # Find the project folder
        'scales', # Format plots
        'sandwich', # Robust standard errors
        'lmtest', # Test regression models
        'AER', # Econometrics methods
        'Formula' # Handle more complex regression formulas
    ) 

installed <- rownames(installed.packages()) # Lists already installed packages
missing <- packages[!packages %in% installed] # Lists missing packages

# Only install missing packages
if (length(missing) > 0) { 
    install.packages(missing, repos = 'https://cloud.r-project.org')
}

# Meyersson uses the Imbens & Kalyanaraman bandwidth algorithm, 
# which is available as a package in the archived rdd:
if (!requireNamespace('rdd', quietly = TRUE)) {
    install.packages(
        'https://cran.r-project.org/src/contrib/Archive/rdd/rdd_0.57.tar.gz',
        repos = NULL,
        type = 'source'
    )
}

message('Package setup finished. You can now run run_all.R')

