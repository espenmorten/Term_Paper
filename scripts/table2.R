library(haven)
library(tidyverse)
library(estimatr)
library(gt)

df <- read_dta('data/regdata0.dta')

# DEFINING VARIABLES:
outcomes <- c('hischshr1520f', 'hischshr1520m') # Outcome variables
provinence_dummies <- names(select(df, starts_with('pd_'))) # Provinence dummies
controls <- c('vshr_islam1994','partycount','lpop1994','ageshr19','ageshr60','sexr','shhs','merkezi','merkezp','subbuyuk','buyuk',provinence_dummies) # Controls

# Prepare data
data2 <- df |>
    mutate(d = i94, x = iwm94, x2 = x^2, x3 = x^3) |>
    drop_na(all_of(outcomes), d, x, pcode, all_of(controls))

# DEFINING THE CONTROL FUNCTIONS
control_function <- c(
    None = 'd',
    Linear = 'd * x',
    Quadratic = 'd * (x + x2)',
    Cubic = 'd * (x + x2 + x3)'
)

# MODEL SETTINGS
model_settings <- data.frame(
    women_outcome = c(rep('hischshr1520f', 8), 'studshr1530f'),
    men_outcome = c(rep('hischshr1520m', 8), 'studshr1530m'),
    outcome_label = c(rep('High school completion', 8), 'Enrollment'),
    age_cohort = c(rep('15-20', 8), '15-30'),
    order = c('None', 'None', 'Linear', 'Linear', 'Linear', 'Linear', 'Quadratic', 'Cubic', 'Linear'),
    with_controls = c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    h_women = c(Inf, Inf, 0.24, 0.24, 0.12, 0.48, 0.24, 0.24, 0.205),
    h_men = c(Inf, Inf, 0.323, 0.323, 0.161, 0.646, 0.323, 0.323, 0.230)
)

make_sample <- function(outcome, bandwidth) {
    model_data <- data2
    model_data$y <- model_data[[outcome]]
    model_data |>
        filter(abs(x) < bandwidth, !is.na(y))
}

fit_column <- function(model_data, model_formula, bandwidth) {
    fit <- lm_robust(
        model_formula,
        data = model_data,
        clusters = pcode,
        se_type = 'stata'
    )

    result <- summary(fit)$coefficients['d', ]
    p_value <- result['Pr(>|t|)']

    stars <- ''
    if (p_value < 0.1) stars <- '*'
    if (p_value < 0.05) stars <- '**'
    if (p_value < 0.01) stars <- '***'

    if (is.infinite(bandwidth)) bandwidth <- 1

    c(
        sprintf('%.3f', mean(model_data$y)),
        sprintf('%.3f%s\n(%.3f)', result['Estimate'], stars, result['Std. Error']),
        sprintf('%.3f', bandwidth),
        sprintf('%.2f', fit$r.squared),
        as.character(nrow(model_data))
    )
}

test_equality <- function(women_data, men_data, model_formula) {
    women_data$woman <- 1
    men_data$woman <- 0
    combined <- rbind(women_data, men_data)

    pooled_formula <- update(model_formula, . ~woman * (.))

    fit <- lm_robust(
        pooled_formula,
        data = combined,
        clusters = pcode,
        se_type = 'stata'
    )

    summary(fit)$coefficients['woman:d', 'Pr(>|t|)']
}

result_labels <- c(
    'Outcome mean', 'Islamic mayor in 1994', 'Bandwidth', 'R^2', 'Observations'
)

women <- data.frame(label = result_labels)
men <- data.frame(label = result_labels)
panel_c <- data.frame(label = 'p-value')

for (column in seq_len(nrow(model_settings))) {
    model <- model_settings[column, ]

    terms <- control_function[[model$order]]
    if (model$with_controls) terms <- c(terms, controls)
    formula <- reformulate(terms, response = 'y')

    women_data <- make_sample(model$women_outcome, model$h_women)
    men_data <- make_sample(model$men_outcome, model$h_men)

    name <- paste0('col', column)
    women[[name]] <- fit_column(women_data, formula, model$h_women)
    men[[name]] <- fit_column(men_data, formula, model$h_men)
    panel_c[[name]] <- sprintf('%.3f', test_equality(women_data, men_data, formula))
}


# 4. Formatting the table
table2 <- bind_rows(
    "Panel A: Women" = women,
    "Panel B: Men" = men,
    'Panel C: Test of coefficient equality' = panel_c,
    .id = "panel"
) |>
    gt(groupname_col = 'panel') |>

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
    tab_spanner('\u0125', columns = col3:col4, level = 1, id=1) |>
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


    tab_header(title = "TABLE II", subtitle = "Islamic Rule and High School Education") |>
    tab_style(
        style = cell_text(whitespace = "pre-line"),
        locations = cells_body(columns = col1:col9)
    ) |>
    tab_source_note("Standard errors are clustered by province. ***, ** and * indicate the 1%, 5% and 10% levels.")

print(table2)

