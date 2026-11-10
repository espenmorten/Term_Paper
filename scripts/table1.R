install.packages("gtsummary")
# Importing the relevant packages
library(haven) # For importing .dta
library(tidyverse) # For preparing the data
library(gtsummary) # For formatting table and calculate the statistics

# Importing data
df <- read_dta('data/regdata0.dta')

outcomes <- c('hischshr1520f', 'hischshr1520m') # Share women/men aged 15-20 with high school education

# define covariates
covariates <- c(
    'vshr_islam1994', # Islamic vote share in 1994
    'partycount', # Number of parties receiving votes 1994
    'lpop1994', # Log population in 1994
    'ageshr19', # Population share below 19 in 2000a
    'ageshr60', # Population share above 60 in 2000
    'sexr', # Gender ratio in 2000
    'shhs', # Household size in 2000
    'merkezi', # District center
    'merkezp', # Province center
    'subbuyuk' # Sub-metro center
)

treatment_variable <- 'i94' # Islamic mayor in 1994

# Prepare data --> dropping missing values, grouping by mayor type
data1 <- df |> # Get the dataframe
    select(all_of(c(outcomes, covariates, treatment_variable))) |> # Keep all the columns that are defined in outcomes, covariates and treatment variable above
    drop_na() |> # Drop all of the rows that doesn't have values in at least 1 of the columns
    mutate( # Adds a column
        mayor = factor(i94, levels = c(1, 0), labels = c('Islamic', 'Secular')) # Mayor is created as categorical variable, i94=1 means mayor is islamic, 0 means secular
    )

significance_note <- '***, **, and * denote significance at the 1%, 5%, and 10% levels.' # Explaining the stars

format_estimate <- function(estimate, std_error, p_value) {
    stars <- case_when(
        p_value < 0.01 ~ '***',
        p_value < 0.05 ~ '**',
        p_value < 0.1 ~ '*',
        TRUE ~ ''
    )

    sprintf('%.3f%s  \n(%.3f)', estimate, stars, std_error)
}

# Calculate difference and standard errors for the fourth column
diff_se_p = function(data, variable, ...) { #Define function with input data and variable to be summarized. 
    model <- lm(formula = data[[variable]] ~ i94, data = data) # Linear model, fitting variable by relationship 
    result <- summary(model)$coefficients # Finding the coefficients
    difference <- result['i94', 'Estimate'] # Storing difference
    standard_error <- result['i94', 'Std. Error'] # Storing standard error
    p_value <- result['i94', 'Pr(>|t|)'] # Storing p-value

    stars <- # Assigning stars according to the 1, 5 and 10% significance levels 
        if (variable == 'i94') '' else
        if (p_value < 0.01)'***' else
        if (p_value < 0.05) '**' else
        if (p_value < 0.1) '*' else  ''

    sprintf(fmt = '%.3f%s  \n(%.3f)', difference, stars, standard_error) # Formatting the output nicely with 3 decimals and linebreak
}

# Create the summary table
table1 <- data1 |>
    tbl_summary( # From the gtsummary package
        by = mayor, 
        type = everything() ~ 'continuous', 
        statistic = everything() ~ '{mean}  \n({sd})',
        digits = everything() ~ 3
    ) |>
    add_overall() |>
    add_stat(fns = everything() ~ diff_se_p) |>

    # Adding the column headings
    modify_header(
        label='', # Take away default "Characteristic"
        stat_0 = '**All  \n(1)  \nMean  \n(S.D.)**',
        stat_1 = '**Islamic  \n(2)  \nMean  \n(S.D.)**', 
        stat_2 = '**Secular  \n(3)  \nMean  \n(S.D.)**',
        add_stat_1 = '**(2)-(3)  \n(4)  \nEst.  \n(S.E)**'
    ) |>

    # Adding variable group headers
    add_variable_group_header(header="Main outcome variables", variables = c(hischshr1520f, hischshr1520m),) |> # Heading for the main outcome variables
    add_variable_group_header(header="Main explanatory variable", variables = i94) |> # Heading for the main explanatory variable
    add_variable_group_header(header="Covariates", variables = vshr_islam1994:subbuyuk) |> # Heading for the list of covariates

    remove_footnote_header() |>
    as_gt() |> 

    gt::rows_add(
        label = 'Observations',
        stat_0 = as.character(nrow(data1)), # All column
        stat_1 = as.character(sum(data1$i94==1)), # Islamic column
        stat_2 = as.character(sum(data1$i94==0)), # Secular column
        add_stat_1 = as.character(nrow(data1)) # Difference in means + S.E. column
    ) |> 
    gt::tab_spanner(label='Mayor Type', columns = c(stat_0, stat_1, stat_2))  |>
    gt::tab_spanner(label='Difference', columns = add_stat_1)  |>
    gt::tab_header(title = 'TABLE I', subtitle = 'SUMMARY STATISTICS') |> 
    gt::tab_source_note(source_note = paste(
            'Columns 1-3 report means and standard deviations in parenthesis.', 
            'Column 4 reports differences of group means between columns 2 and 3 with standard errors in parenthesis.',
            '***, **, and * denote significance at the 1, 5 and 10 percent levels, respectively.'
        )) |>

    gt::fmt_markdown()

print(table1)
