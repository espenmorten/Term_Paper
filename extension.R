# ============================================================
# Extension 1: Modern RD estimation
# TECH6 - Data Science and Econometrics
# ============================================================

library(haven) # Package that can read Stata (.dta) files
library(rdrobust) # Modern RD estimation package
library(dplyr) # Consistent data manipulation
library(gt) # Table formatting library

# Data preparation
df <- read_dta("data/regdata0.dta")

# Keeping only non-missing observations of 
# treatment, running variable, province and outcome
z <- subset(df, !is.na(i94) &
  !is.na(iwm94) & !is.na(hischshr1520f) &
  !is.na(pcode))

# Province dummies (Reusing table 2 code)
province_dummies <- names(select(df, starts_with('pd_')))
# Control variables (Reusing table 2 code)
controls <- c(
    'vshr_islam1994',
    'partycount',
    'lpop1994',
    'ageshr19',
    'ageshr60',
    'sexr','shhs',
    'merkezi',
    'merkezp',
    'subbuyuk',
    'buyuk',
    province_dummies
    )

# Keeping only non-missing control variable observations
z_control <- z[complete.cases(z[, controls]), ]
covariates <- z_control[, controls]

# Defining running variable and outcome
x <- z_control$iwm94
y <- z_control$hischshr1520f 

# Province codes for clustering standard errors in rdrobust()
cluster <- as.integer(factor(z_control$pcode))

# Running the modern rd with control variables and clustering
controls_rd <- rdrobust(y = y, x = x, covs = covariates, cluster = cluster)

# Getting the optimal bandwidths
controls_rd$bws
h_opt  <- controls_rd$bws["h", "left"]             # The chosen bandwidth
b_opt <- controls_rd$bws["b", "left"]              # Bias bandwidth

# Modern RD with half and double h and b
half_rd <- rdrobust(y, x, c = 0, h = h_opt/2, b=b_opt/2, covs=covariates, cluster=cluster)
double_rd <- rdrobust(y, x, c = 0, h = h_opt*2, b=b_opt*2, covs=covariates, cluster=cluster)

# Display results
summary(controls_rd) # Optimal bandwidth
summary(half_rd) # Half bandwidth
summary(double_rd) # Double bandwidth



## TABLE CREATION

# Getting results from all 3 RDs
fit_column <- function(model) {

    c(
        sprintf('%.3f', model$coef["Conventional", 1]),
        sprintf('%.3f', model$bws["h", "left"]),
        sprintf('%.3f', model$ci["Robust", 1]),
        sprintf('%.3f', model$ci["Robust", 2]),
        as.character(sum(model$N_h))
    )
}

result_labels <- c(
    'RD effect', 'Bandwidth (h)', 'Lower 95% CI', "Upper 95% CI", 'Observations within h'
)

results <- data.frame(label = result_labels)

# Adding our results from the three RD models
results$modern_rd <- fit_column(controls_rd)
results$half_rd <- fit_column(half_rd)
results$double_rd <- fit_column(double_rd)

# Formatting the table
extension1_table <- results |>
    gt() |>

    # Column labels
    cols_label(
        label = md('Result'),
        modern_rd = md("Modern RD"),
        half_rd = md("Half bandwidth"),
        double_rd = md("Double bandwidth")
    ) |>

    tab_header(title = "Extension 1", subtitle = "Results of modern RD estimation")|>
    tab_source_note("Confidence Intervals are robust and bias-corrected, while point estimates are conventional. Standard errors clustered by province.")
print(extension1_table)

# ============================================================
# Extension 2: Missing data and sample selection
# ============================================================



