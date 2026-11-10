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

# Checking quantities of existing and non-existing observations in 2011-data
# Non-existing observations
sum(is.na(df$ahischshr15_19f))

#Existing observations
sum(!is.na(df$ahischshr15_19f))

# Checking for 2011 data only for observations that would be part of our analysis
z <- subset(df, !is.na(i94) & !is.na(iwm94) & !is.na(pcode))

# Non-existing observations
sum(is.na(z$ahischshr15_19f))

# Existing observations
sum(!is.na(z$ahischshr15_19f))

# Creating a variable for whether observation exists in 2011
z$has_2011_data <- !is.na(z$ahischshr15_19f)

x <- z$iwm94
y <- as.numeric(z$has_2011_data)
cluster <- as.integer(factor(z$pcode))

selection_rd <- rdrobust(y = y, x = x, c=0, cluster = cluster)
summary(selection_rd)

# Observations with at least 2000 data
all_data <- subset(z, !is.na(hischshr1520f))

# Observations that contain both 2000 and 2011 data
has_2011 <- subset(all_data, has_2011_data == TRUE)

all_rd <- rdrobust(
    y = all_data$hischshr1520f,
    x = all_data$iwm94,
    cluster = as.integer(factor(all_data$pcode))
)
summary(all_rd)

h_opt = all_rd$bws["h", "left"]
b_opt = all_rd$bws["b", "left"]

has2011_rd <- rdrobust(
    y = has_2011$hischshr1520f,
    x = has_2011$iwm94,
    h = h_opt,
    b = b_opt,
    cluster = as.integer(factor(has_2011$pcode))
)
summary(has2011_rd)

##TABLE

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

results2 <- data.frame(label = result_labels)

# Adding our results from the three RD models
results2$all_rd <- fit_column(all_rd)
results2$has2011_rd <- fit_column(has2011_rd)

# Formatting the table
extension2_table <- results2 |>
    gt() |>

    # Column labels
    cols_label(
        label = md("Result"),
        all_rd = md("2000-data available"),
        has2011_rd = md("2000 and 2011-data available")
    ) |>

    tab_header(title = "Extension 2", subtitle = "Female education (2000) by sample availability")|>
    tab_source_note("Confidence Intervals are robust and bias-corrected, while point estimates are conventional. Standard errors clustered by province. Both models use the same h and b.")
print(extension2_table)

rdplot(
    y=y,
    x=x,
    c=0,
    p=1,
    x.lim = c(-0.3, 0.3),
    x.label = "Islamic win margin 1994",
    y.label = "Share with available data for 2011",
    title = "Availability of 2011-data on female education"
    )

