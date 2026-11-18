# ============================================================
# Extension 2: Population growth and age composition
# ============================================================

population_sample <- rd_base_sample |>
    drop_na(c90lpop, lpop, c90ageshr19, ageshr19) |>
    mutate(
        log_population_growth = lpop - c90lpop,
        youth_share_change = 100 * (ageshr19 - c90ageshr19)
    )

population_controls <- setdiff( # Control list for this extension
    regression_controls, c('ageshr19', 'ageshr60', 'sexr', 'shhs') # These four controls are recorded in 2000, so 
)

population_outcomes <- data.frame(
    outcome = c('log_population_growth', 'youth_share_change'),
    label = c('Log population growth (log points)', 'Change in share under 19 (percentage points)')
)

did_formula <- make_formula('None') # Build y ~ d
adjusted_did_formula <- make_formula('None', population_controls) # Adds controls

population_results <- data.frame() # The list we'll add the results to after each run

bandwidths <- c(0.25, 0.1, 0.5) # Main, narrower, wider bandwidths

for (row in seq_len(nrow(population_outcomes))) { 
    outcome <- population_outcomes$outcome[row] # Find and store outcome

    for (h in bandwidths) {
        model_sample <- make_sample(population_sample, outcome, h=h) # Create a sample based on the bandwidth in the sequence
        did_fit <- fit_clustered(did_formula, model_sample) # Fit plain DiD
        adjusted_did_fit <- fit_clustered(adjusted_did_formula, model_sample) # Fit adjusted DiD

        did_jump <- summary(did_fit)$coefficients['d', ] # Extract coefficients on d (plain did)
        adj_did_jump <- summary(adjusted_did_fit)$coefficients['d', ] # Extract coefficients on d (adj. did)
        jumps <- rbind(did_jump, adj_did_jump) # Bind the rows together

        outcome_results <- data.frame(
            outcome = population_outcomes$label[row], # Display label for model rows
            model = c('DiD', 'DiD with controls'), # Label the models
            bandwidth = h, 
            municipalities = nrow(model_sample), # Observations/number of municipalities
            estimate = jumps[, 'Estimate'], # Estimate
            ci_lower = jumps[, 'CI Lower'], # Lower confidence interval bound
            ci_upper = jumps[, 'CI Upper'], # Upper confidence interval bound
            p_value = jumps[, 'Pr(>|t|)'] # P value
        )
        population_results <- bind_rows(population_results, outcome_results)
    }
}

extension2_table <- population_results |>
    gt(groupname_col = 'outcome') |>
    tab_header(title = 'Extension 2: Population growth and age composition', subtitle = '1990 - 2000') |> 
    cols_label( # Printed column labels
        model = 'Model',
        bandwidth = 'Bandwidth',
        municipalities = 'Municipalities',
        estimate = 'DiD Estimate',
        ci_lower = '95% CI',
        p_value = 'P-value'
    ) |>
    fmt_number(columns = c(bandwidth, estimate, ci_lower, ci_upper, p_value), decimals = 3) |> # Round decimals
    cols_merge(columns = c(ci_lower, ci_upper), pattern = '[{1}, {2}]') |> # Format confidence intervals in [{lower}, {upper}]
    tab_source_note('')