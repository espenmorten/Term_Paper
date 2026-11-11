# Prepare data --> dropping missing values, grouping by mayor type
table1_sample <- df |>
    select(all_of(c(education_outcomes, 'i94', summary_covariates))) |> # Keep all the variables shown in table 1
    drop_na() |> # Keep the municipalities with complete values for these variables
    mutate( # Adds a column
        mayor = factor(i94, levels = c(1, 0), labels = c('Islamic', 'Secular')) # Mayor is created as categorical variable, i94=1 means mayor is islamic, 0 means secular
    )

# Calculate difference and standard errors for the fourth column
diff_se_p = function(data, variable, ...) { 
    if (variable == 'i94') {
        return(format_estimate(1, 0, p_value = NA_real_))
    } 
    fit <- lm(data[[variable]] ~ i94, data = data) # Fit the regression that compares the 2 group means
    result <- summary(fit)$coefficients['i94', ] # Gets the coefficient row for the treatment variable i94
    format_estimate(result['Estimate'], result['Std. Error'], result['Pr(>|t|)']) # Display the estimate, std error and any significance stars from p value
}
# Create the summary table
table1 <- table1_sample |>
    tbl_summary( # From the gtsummary package we can create a summary table
        by = mayor, # group by
        type = everything() ~ 'continuous',  # Summarize variables with numeric statistics, also the indicators
        statistic = everything() ~ '{mean}  \n({sd})', # Show the mean, then linebreak, then standard error
        digits = everything() ~ 3 # Use 3 digits
    ) |>
    add_overall() |> # Add a 'All' column, keeping all the municipalities in the sample
    add_stat(fns = everything() ~ diff_se_p) |> # Add the fourth column, fns is short for functions, calling diff_se_p for every variable

    # Adding the column headings
    modify_header(
        label='', # Take away default "Characteristic"
        stat_0 = '**All  \n(1)  \nMean  \n(S.D.)**',
        stat_1 = '**Islamic  \n(2)  \nMean  \n(S.D.)**', 
        stat_2 = '**Secular  \n(3)  \nMean  \n(S.D.)**',
        add_stat_1 = '**(2)-(3)  \n(4)  \nEst.  \n(S.E)**'
    ) |>

    # Adding variable group headers
    add_variable_group_header(header="Main outcome variables", variables = all_of(education_outcomes),) |> # Heading for the main outcome variables
    add_variable_group_header(header="Main explanatory variable", variables = i94) |> # Heading for the main explanatory variable
    add_variable_group_header(header="Covariates", variables = all_of(summary_covariates)) |> # Heading for the list of covariates

    remove_footnote_header() |>
    as_gt() |> # Convert to a gt table

    # Observations row
    rows_add(
        label = 'Observations',
        stat_0 = as.character(nrow(table1_sample)), # All column
        stat_1 = as.character(sum(table1_sample$i94==1)), # Islamic column
        stat_2 = as.character(sum(table1_sample$i94==0)), # Secular column
        add_stat_1 = as.character(nrow(table1_sample)) # Difference in means + S.E. column
    ) |> 
    tab_spanner(label='Mayor Type', columns = c(stat_0, stat_1, stat_2))  |> # Shared heading
    tab_spanner(label='Difference', columns = add_stat_1)  |> 
    tab_header(title = 'TABLE I', subtitle = 'SUMMARY STATISTICS') |> 
    tab_source_note(paste(
            'Columns 1-3 report means and standard deviations in parenthesis.', 
            'Column 4 reports differences of group means between columns 2 and 3 with standard errors in parenthesis.',
            significance_note
        )) |>

    fmt_markdown()
