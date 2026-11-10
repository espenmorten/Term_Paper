h_school_women <- ik_h(make_sample(education_sample, 'hischshr1520f', h = Inf)) # Get the bandwidth based on the prepared sample where y is set to 'hischshr1520f' and using all eligible municipalities
h_school_men <- ik_h(make_sample(education_sample, 'hischshr1520m', h = Inf))
h_enrollment_women <- ik_h(make_sample(education_sample, 'studshr1530f', h = Inf))
h_enrollment_men <- ik_h(make_sample(education_sample, 'studshr1530m', h = Inf))

table2_settings <- data.frame( # Build a dataframe with the settings as names
    women_outcome = c(rep('hischshr1520f', 8), 'studshr1530f'), # Defines women who have completed high school, aged between 15 and 20 (outcome and age cohort)
    men_outcome = c(rep('hischshr1520m', 8), 'studshr1530m'), # Defines men who have completed high school, aged between 15 and 20, (outcome and age cohort)
    order = c('None', 'None', 'Linear', 'Linear', 'Linear', 'Linear', 'Quadratic', 'Cubic', 'Linear'), # Defines the control function from column 1 to 9
    h_women = c(1, 1, h_school_women, h_school_women, h_school_women/2, 
                h_school_women*2, h_school_women, h_school_women, h_enrollment_women), # Defines the bandwidth for women from column 1 to 9
    h_men = c(1, 1, h_school_men, h_school_men, h_school_men/2, h_school_men*2, 
                h_school_men, h_school_men, h_enrollment_men), # Defines the bandwidth for men from column 1 to 9
    with_controls = c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE) # Defines whether a control with covariates should be included
)

table2_data <- bind_rows( # Stack the panels vertically
    'Panel A: Women' = data.frame(label = c('Outcome mean', 'Islamic mayor in 1994', 'Bandwidth', 'R^2', 'Observations')), # five rows for women
    'Panel B: Men' = data.frame(label = c('Outcome mean', 'Islamic mayor in 1994', 'Bandwidth', 'R^2', 'Observations')), # five rows for men
    'Panel C: Test of coefficient equality' = data.frame(label = 'p-value'), # for the test of eqality
    .id = 'panel' # add column that records which panel each row belongs to
)
# Fit the 5 cells (5 cells for women and 5 cells for men later)
table2_cells <- function(data, formula, h) {
    fit <- fit_clustered(formula, data) # From shared.R
    c(
        sprintf('%.3f', mean(data$y)), # Formatting the mean outcome
        mayor_cell(fit), # Formatting the islamic mayor in 1994 cell
        sprintf('%.3f', h), # Formatting the bandwidth
        sprintf('%.2f', fit$r.squared), # Formatting the r squared
        as.character(nrow(data)) # Formatting the number of observations
    )
}

# To calculate the p values in panel c, we stack the 2 data samples and 
# check whether the mayor coefficient differs by gender
test_equality <- function(women_data, men_data, model_formula) { # Testing whether the jumps at 
    women_data$woman <- 1 # Create a column called 'woman' and set it to 1 for all rows in women_data
    men_data$woman <- 0 # Create a column called 'woman' and set it to 0 for all rows in men_data
    combined <- rbind(women_data, men_data) # Combine datasets

    pooled_formula <- update(model_formula, . ~woman * (.)) # 
    fit <- fit_clustered(pooled_formula, combined, se_type = 'CR0') # Fit the model

    result <- summary(fit)$coefficients['woman:d', ]
    effect_difference <- result['Estimate']

    # For CR0, we have to correct for the number of provinces
    provinces <- n_distinct(combined$pcode)
    corrected_se <- result['Std. Error'] * sqrt(provinces / (provinces-1))

    wald_statistic <- (effect_difference / corrected_se)^2
    pchisq(wald_statistic, df = 1, lower.tail = FALSE)
}

# Use each row of settings to build 1 column in table 2
for (column in seq_len(nrow(table2_settings))) { 
    setting <- table2_settings[column, ] # Read the matching settings row

    model_controls <- if (setting$with_controls) regression_controls else character(0) # Use regression controls if with_controls=TRUE
    formula <- make_formula(setting$order, model_controls) # Build the formula

    women_sample <- make_sample(education_sample, setting$women_outcome, setting$h_women) # Build the women's sample from education sample with corresponding outcome and bandwidth from the settings
    men_sample <- make_sample(education_sample, setting$men_outcome, setting$h_men) # Build the men's sample in the same way

    column_name <- paste0('col', column) # Name it col{column} - for example 4 turns to col4
    p_value <- test_equality(women_sample, men_sample, formula) # Get the p-value for H0 that the mayor effect is the same for women and men

    table2_data[[column_name]] <- c( # Combine the 5 cells for women, 5 cells for men and 1 p-value per column.
        table2_cells(women_sample, formula, setting$h_women), 
        table2_cells(men_sample, formula, setting$h_men),
        sprintf('%.3f', p_value)
    )
}

# 4. Formatting the table
table2 <- table2_data |>
    gt(groupname_col = 'panel')|>  
    tab_header(title = "TABLE II", subtitle = "Islamic Rule and High School Education") |>

    # Covariates
    cols_label(
        label = md('Covariates'),
        col1 = md('No  \n(1)'),
        col2 = md('Yes  \n(2)'),
        col3 = md('No  \n(3)'),
        col4 = md('Yes  \n(4)'),
        col5 = md('Yes  \n(5)'),
        col6 = md('Yes  \n(6)'),
        col7 = md('Yes  \n(7)'),
        col8 = md('Yes  \n(8)'),
        col9 = md('Yes  \n(9)')
    ) |>

    # Bandwidth
    tab_spanner('Bandwidth', columns = label, level = 1) |>
    tab_spanner('Global', columns = col1:col2, level = 1) |>
<<<<<<< HEAD
    tab_spanner('\u0125', columns = col3:col4, level = 1, id=1) |> # \u0125 renderes as h_hat in html files.
=======
    tab_spanner('\u0125', columns = col3:col4, level = 1, id=1) |>
>>>>>>> origin/main
    tab_spanner('\u0125/2', columns = col5, level = 1) |>
    tab_spanner('2\u0125', columns = col6, level = 1) |>
    tab_spanner('\u0125', columns = col7:col9, level = 1, id=2)|>

    # Control function
    tab_spanner('Control Function', columns = label, level = 2) |>
    tab_spanner('None', columns = col1:col2, level = 2) |>
    tab_spanner('Linear', columns = col3:col6, level = 2, id=3) |>
    tab_spanner('Quadratic', columns = col7, level = 2) |>
    tab_spanner('Cubic', columns = col8, level = 2) |>
    tab_spanner('Linear', columns = col9, level = 2, id=4) |>

    # Age Cohort
    tab_spanner('Age Cohort', columns = label, level = 3) |>
    tab_spanner('15-20', columns = col1:col8, level = 3) |>
    tab_spanner('15-30', columns = col9, level = 3) |>

    # Outcome
    tab_spanner('Outcome', columns = label, level = 4) |>
    tab_spanner('Completed high school in 2000', columns = col1:col8, level = 4) |>
    tab_spanner('Enrollment', columns = col9, level = 4) |>

<<<<<<< HEAD
    style_rd_table()
=======

    tab_header(title = "TABLE II", subtitle = "Islamic Rule and High School Education") |>
    tab_style(
        style = cell_text(whitespace = "pre-line"),
        locations = cells_body(columns = col1:col9)
    ) |>
    tab_source_note("Standard errors are clustered by province. ***, ** and * indicate the 1%, 5% and 10% levels.")

print(table2)

>>>>>>> origin/main
