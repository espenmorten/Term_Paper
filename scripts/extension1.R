# ============================================================
# Extension 1: Comparison of different age groups
# TECH6 - Data Science and Econometrics
# ============================================================
agegroups_2011_outcomes <- c('ahischshr20_24f', 'ahischshr25_29f', # 2011 high school completion share women
                            'ahischshr20_24m', 'ahischshr25_29m') # 2011 high school completion share men

# Prepare data
adult_sample <- rd_base_sample |> 
    drop_na(all_of(agegroups_2011_outcomes)) |> # keep only rows with all 4 outcomes recorded
    mutate( # adds 2 columns, 1 comparison per sex:
        women_20_vs_25 = ahischshr20_24f - ahischshr25_29f,
        men_20_vs_25 = ahischshr20_24m - ahischshr25_29m)   

women_20_24_sample <- make_sample(adult_sample, 'ahischshr20_24f', h = 1) # Keep full range (h = 1) to calculate optimal bandwidth using ik_h from shared.R
women_25_29_sample <- make_sample(adult_sample, 'ahischshr25_29f', h = 1)

adult_bandwidths <- c( # Calculating the 2 bandwidths. 
    'IK bandwidth from women aged 20-24' = ik_h(women_20_24_sample), # Bandwidth 1 is using the imbens & kalyanaraman algorithm on the 20-24 women sample
    'IK bandwidth from women aged 25-29' = ik_h(women_25_29_sample) # Bandwidth 2 is using the imbens & kalyanaraman algorithm on the 20-24 women sample
)

adult_comparisons <- data.frame(
    sex = c('Women', 'Men'),
    outcome = c('women_20_vs_25', 'men_20_vs_25')
)

adult_formula <- make_formula('Linear', regression_controls) # Using the local linear model with controls
adult_results <- list() # Start empty list for storing 

for (bandwidth_label in names(adult_bandwidths)) { # 2 loops
    h <- adult_bandwidths[[bandwidth_label]] # Look up bandwidth
    comparison_rows <- list() # start new list for this bandwidth

    for (row in seq_len(nrow(adult_comparisons))) {
        outcome <- adult_comparisons$outcome[row] # find the outcome from the current comparison row
        model_sample <- make_sample(adult_sample, outcome, h) # create the sample given outcome and bandwidth
        fit <- fit_clustered(adult_formula, model_sample) # fit with standard errors clustered by province
        jump <- summary(fit)$coefficients['d', ] # Extract the d row from the coefficients table

        comparison_rows[[row]] <- data.frame( # Build one row per sex and bandwidth combination
            window = bandwidth_label, 
            sex = adult_comparisons$sex[row],
            bandwidth = h,
            municipalities = nrow(model_sample),
            effect_pp = 100*jump[['Estimate']],
            ci_lower = 100*jump[['CI Lower']],
            ci_upper = 100*jump[['CI Upper']],
            p_value = jump[['Pr(>|t|)']]
        )
    }

    window_results <- bind_rows(comparison_rows) # Stack women' and men's result
    window_results$p_holm <- p.adjust(window_results$p_value, method = 'holm') # Apply adjusted p value to the comparisons
    adult_results[[bandwidth_label]] <- window_results # Save the 2 results under the bandwidth label
} 

extension1_results <- bind_rows(adult_results) # Stack both bandwidths into one table

extension1_table <- extension1_results |> # Formatting with gt
    gt(groupname_col = 'window') |>
    tab_header(title = 'Extension 1: High school completion share by age group in 2011', 
    subtitle = 'Difference in RD effects for ages 20-24 and 25-29') |>
    cols_label( # Printed column labels
        sex = 'Sex',
        bandwidth = 'Bandwidth',
        municipalities = 'Municipalities',
        effect_pp = 'Estimate (pp)',
        ci_lower = '95% CI',
        p_value = 'P-value',
        p_holm = 'Adjusted p-value (holm)'
    ) |>
    fmt_number(columns = c(bandwidth, effect_pp, ci_lower, ci_upper, p_value, p_holm), decimals = 3) |> # Round decimals
    cols_merge(columns = c(ci_lower, ci_upper), pattern = '[{1}, {2}]') |> # Format confidence intervals in [{lower}, {upper}]
    tab_source_note('Positive differences indicates a larger estimated effect for ages 20-24. 
    Confidence intervals use province-clustered standard errors. 
    Holm p-values adjust for the 2 age comparisons within each bandwidth')



