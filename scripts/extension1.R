# ============================================================
# Extension 1: Modern RD estimation
# TECH6 - Data Science and Econometrics
# ============================================================


extension_rd_rows <- c( # The names of the rows in our table
    'Conventional RD estimate',
    'Conventional 95% CI',
    'Bias-corrected RD estimate',
    'Robust bias-corrected 95% CI',
    'Estimation bandwidth (h)',
    'Bias bandwidth (b)',
    'Municipalities within h'
)

extension_rd_cells <- function(fit) {
    conventional_ci <- fit$ci['Conventional', ] # Store the conventional confidence interval
    robust_ci <- fit$ci['Robust', ] # Store the robust confidence interval

    c(
        sprintf('%.3f', fit$coef['Conventional', 1]), # Format the conventional estimate
        sprintf('[%.3f, %.3f]', conventional_ci[1], conventional_ci[2]), # Format the 95% confidence interval
        sprintf('%.3f', fit$coef['Bias-Corrected', 1]), # Format the bias-corrected estimate
        sprintf('[%.3f, %.3f]', robust_ci[1], robust_ci[2]), # Format the bias-corrected 95% confidence interval
        sprintf('%.3f', fit$bws['h', 'left']), # Format the estimation bandwidth
        sprintf('%.3f', fit$bws['b', 'left']), # Format the bias bandwidth
        as.character(sum(fit$N_h)) # Add number of observations
    )
}

# ================================================================
# EXTENSION 1
# ================================================================
# RD Robust will get this full sample, it will then be able to determine its own b and h
extension_2000_outcome <- 'hischshr1520f' 
extension1_full_sample <- make_sample(education_sample, extension_2000_outcome, h = Inf) # Recreating the full table 2 sample
covariates <- select(extension1_full_sample, all_of(regression_controls))

# The original fit (found in table 2 column 4) used the IK bandwidth
table2_h <- ik_h(extension1_full_sample) # Find the bandwidth based on the full sample
table2_sample <- filter(extension1_full_sample, abs(x)<=table2_h)
extension1_formula <- make_formula('Linear', regression_controls) # Linear model with regression controls
table2_fit <- fit_clustered(extension1_formula, table2_sample)
table2_result <- summary(table2_fit)$coefficients['d', ]

modern_fit <- fit_extension_rd(extension1_full_sample, covariates = covariates, kernel = 'triangular')
modern_h <- modern_fit$bws['h', 'left']
modern_b <- modern_fit$bws['b', 'left']

extension1_data <- data.frame(
    label = extension_rd_rows, 
    table2 = c(
        sprintf('%.3f', table2_result['Estimate']), # Point estimate
        sprintf('[%.3f, %.3f]', table2_result['CI Lower'], table2_result['CI Upper']), # 95% Confidence interval
        '-', # None
        '-', # None
        sprintf('%.3f', table2_h), # Bandwidth
        '-', # None
        as.character(nrow(table2_sample)) # Number of observations
    ),
    modern = extension_rd_cells(modern_fit)
)

extension_multipliers <- c(0.5, 2)

for (column in seq_along(extension_multipliers)) { #2 rows
    multiplier <- extension_multipliers[column] # Choose one multipler at a time, either 0.5 or 2
    column_name <- paste0('sensitivity', column) # Save this as column sensitivity{multiplier}

    sensitivity_fit <- fit_extension_rd(
        extension1_full_sample, 
        covariates = covariates,
        h = modern_h * multiplier, 
        b = modern_b * multiplier,
        kernel = 'triangular'
    )
    extension1_data[[column_name]] <- extension_rd_cells(sensitivity_fit) # Fill in the column for that fit
}

# Formatting
extension1_table <- extension1_data |>
    gt(rowname_col = 'label') |>
    tab_header(title = 'Extension 1', subtitle = 'Modern RD estimation of female high school completion in 2000') |>
    cols_label(
        table2 = 'Table II (4)',
        modern = 'Automatic h and b',
        sensitivity1 = 'Half modern h and b', 
        sensitivity2 = 'Double modern h and b'
    )