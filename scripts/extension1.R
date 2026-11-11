# ============================================================
# Extension 1: Modern RD estimation
# TECH6 - Data Science and Econometrics
# ============================================================

fit_extension_rd <- function(data, covariates = NULL, h = NULL, b = NULL, kernel = 'triangular') {
    rdrobust(
        y = data$y, # outcome
        x = data$x, # running variable
        c = 0, # election cutoff
        p = 1, # fit a line on each side of the cutoff to estimate the jump
        q = 2, # fit a quadratic on each side of the cutoff to estimate the bias
        h = h, # estimation bandwidth
        b = b, # bias bandwidth
        kernel = kernel, # Determine the weights within the bandwidth. Triangular gives more weight to observations close to the cutoff, uniform gives equal weight
        covs = covariates, # whether to include covariates
        cluster = data$pcode # cluster by provinence to allow correlation within provinces.
    )
}

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
extension_2000_outcome <- 'hischshr1520f' 
extension1_full_sample <- make_sample(education_sample, extension_2000_outcome, h = Inf) # Recreating the full table 2 sample
extension1_h <- ik_h(extension1_full_sample) # Find the bandwidth based on the full sample
extension1_bandwidths <- c(extension1_h, extension1_h/2, extension1_h*2) # Store the 3 bandwidths to compare
extension1_formula <- make_formula('Linear', regression_controls) # Linear model with regression controls

extension1_data <- data.frame(label = c('Table 2 RD estimate', extension_rd_rows))

for (column in seq_along(extension1_bandwidths)) { 
    h <- extension1_bandwidths[column]
    column_name <- paste0('col', column)
    model_sample <- make_sample(education_sample, extension_2000_outcome, h)

    table2_fit <- fit_clustered(extension1_formula, model_sample)
    table2_estimate <- coef(table2_fit)['d']

    covariates <- select(model_sample, all_of(regression_controls))
    modern_fit <- fit_extension_rd(model_sample, covariates, h = h, b = h, kernel = 'uniform')

    extension1_data[[column_name]] <- c(
        sprintf('%.3f', table2_estimate),
        extension_rd_cells(modern_fit)
    )
}

# Formatting
extension1_table <- extension1_data |>
    gt(rowname_col = 'label') |>
    tab_header(title = 'Extension 1', subtitle = 'Bias-corrected inference for female high school completion in 2000') |>
    cols_label(
        col1 = 'Table II (4) IK Bandwidth',
        col2 = 'Table II (5) Half bandwidth', 
        col3 = 'Table II (6) Double bandwidth'
    )