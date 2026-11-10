table7_outcomes <- c('ahischshr15_19f', 'ahischshr15_19m', 'ahischshr15_29f', 'ahischshr15_29m', 'nevm15_19f', 'nevm15_19m')

# Prepare data
data7 <- rd_base_sample |>
    drop_na(all_of(table7_outcomes))

ols <- make_formula('None') # y ~ d
ols_controls <- make_formula('None', regression_controls) # y ~ d + regression_controls
local_linear <- make_formula('Linear', regression_controls) # y ~ d + x + d:x + regression controls

# Set up the 15 rows of table 7 before the loop fills in results
table7_data <- bind_rows(
    # Table A has 
    'Panel A: Global OLS' = data.frame(label =c(
        'Outcome mean', 
        'Model 1. Islamic mayor in 1994 (without controls)', 
        'Model 2. Islamic mayor in 1994 (with controls)', 'Bandwidth', 'Observations')),

    # Table B has 
    'Panel B: Conditional local linear RD' = data.frame(label = c(
        'Outcome mean at h', 
        'Model 1. Islamic mayor in 1994 (h)', 'Bandwidth', 'Observations', 
        'Model 2. Islamic mayor in 1994 (h/2)', 'Bandwidth', 'Observations',
        'Model 3. Islamic mayor in 1994 (2h)', 'Bandwidth', 'Observations')),
    .id = 'panel'
)

# Used to create 3 panel b cells per outcome and bandwidth
local_cells <- function(data, formula, h) {
    fit <- fit_clustered(formula, data) # fit the model, std errors clustered by province

    c(
        mayor_cell(fit), # format the estimate, its standard error and significance stars
        sprintf('%.3f', h), # display bandwidth to 3 decimals
        nrow(data) # count observations in sample
    )
}

for (column in seq_along(table7_outcomes)) { # For column 1 through 6
    outcome <- table7_outcomes[column] # choose outcome
    column_name <- paste0("col", column) # name it col{run}

    full_data <- make_sample(data7, outcome, 1) # make the full data with the outcome as y

    # Calculate the IK bandwidth for this outcome.
    h <- ik_h(full_data)

    data_h <- make_sample(data7, outcome, h) # Use chosen window
    data_half_h <- make_sample(data7, outcome, h/2) # Use half the window
    data_double_h <- make_sample(data7, outcome, 2*h) # Use double the window

    fit_ols <- fit_clustered(ols, full_data) # Fit OLS without controls
    fit_ols_controls <- fit_clustered(ols_controls, full_data) # Fit OLS with controls

    panel_a_cells <- c( # Fill panel a
        sprintf('%.3f', mean(full_data$y)), # mean outcome in the full sample
        mayor_cell(fit_ols), # format the estimate without controls
        mayor_cell(fit_ols_controls), # format the estimate with controls
        '1.00', # Display full bandwidth
        nrow(full_data) # Fill in number of observations in the full sample
    )

    panel_b_cells <- c(
        sprintf('%.3f', mean(data_h$y)), # mean outcome within h
        local_cells(data_h, local_linear, h), # estimate, bandwidth and count at h
        local_cells(data_half_h, local_linear, h/2), # estimate, bandwidth at half h
        local_cells(data_double_h, local_linear, 2*h) # estimate, bandwidth at double h
    )

    table7_data[[column_name]] <- c(panel_a_cells, panel_b_cells) # fill the entire column from top to bottom
}

# Format the two panels using gt
table7 <- table7_data |>
    gt(rowname_col = "label", groupname_col = "panel") |>
    tab_header(title = "TABLE VII", subtitle = "RD Treatment Effects of Islamic Rule on Education in 2011") |>
    tab_spanner("High school, ages 15–19", columns = col1:col2) |> # Shared header
    tab_spanner("High school, ages 15–29", columns = col3:col4) |> # Shared header
    tab_spanner("Unmarried, ages 15–19", columns = col5:col6) |> # Shared header
    cols_label(
        col1 = md("Women  \n(1)"),
        col2 = md("Men  \n(2)"),
        col3 = md("Women  \n(3)"),
        col4 = md("Men  \n(4)"),
        col5 = md("Women  \n(5)"),
        col6 = md("Men  \n(6)")
    ) |>
    style_rd_table(note = "")
