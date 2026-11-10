table7_outcomes <- c('ahischshr15_19f', 'ahischshr15_19m', 'ahischshr15_29f', 'ahischshr15_29m', 'nevm15_19f', 'nevm15_19m')

# Prepare data
data7 <- rd_base_sample |>
    drop_na(all_of(table7_outcomes))

ols <- make_formula('None')
ols_controls <- make_formula('None', regression_controls)
local_linear <- make_formula('Linear', regression_controls)

table7_data <- bind_rows(
    'Panel A: Global OLS' = data.frame(label =c('Outcome mean', 
    'Model 1. Islamic mayor in 1994 (without controls)', 
    'Model 2. Islamic mayor in 1994 (with controls)', 'Bandwidth', 'Observations')),
    'Panel B: Conditional local linear RD' = data.frame(label = c('Outcome mean at h', 
    'Model 1. Islamic mayor in 1994 (h)', 'Bandwidth', 'Observations', 
    'Model 2. Islamic mayor in 1994 (h/2)', 'Bandwidth', 'Observations',
    'Model 3. Islamic mayor in 1994 (2h)', 'Bandwidth', 'Observations')),
    .id = 'panel'
)

local_cells <- function(data, formula, h) {
    fit <- fit_clustered(formula, data)

    c(
        mayor_cell(fit),
        sprintf('%.3f', h),
        nrow(data)
    )
}

for (column in seq_along(table7_outcomes)) {
    outcome <- table7_outcomes[column]
    column_name <- paste0("col", column)

    full_data <- make_sample(data7, outcome, 1)

    # Calculate h for this outcome. Round only when displaying it.
    h <- ik_h(full_data)
    data_h <- make_sample(data7, outcome, h)
    data_half_h <- make_sample(data7, outcome, h/2)
    data_double_h <- make_sample(data7, outcome, 2*h)

    fit_ols <- fit_clustered(ols, full_data)
    fit_ols_controls <- fit_clustered(ols_controls, full_data)

    panel_a_cells <- c(
        sprintf('%.3f', mean(full_data$y)),
        mayor_cell(fit_ols),
        mayor_cell(fit_ols_controls),
        '1.00',
        nrow(full_data)
    )

    panel_b_cells <- c(
        sprintf('%.3f', mean(data_h$y)),
        local_cells(data_h, local_linear, h),
        local_cells(data_half_h, local_linear, h/2),
        local_cells(data_double_h, local_linear, 2*h)
    )

    table7_data[[column_name]] <- c(panel_a_cells, panel_b_cells)
}

# Format the two panels using gt, as in Table III
table7 <- table7_data |>
    gt(rowname_col = "label", groupname_col = "panel") |>
    tab_header(title = "TABLE VII", subtitle = "RD Treatment Effects of Islamic Rule on Education in 2011") |>
    tab_spanner("High school, ages 15–19", columns = col1:col2) |>
    tab_spanner("High school, ages 15–29", columns = col3:col4) |>
    tab_spanner("Unmarried, ages 15–19", columns = col5:col6) |>
    cols_label(
        col1 = md("Women  \n(1)"),
        col2 = md("Men  \n(2)"),
        col3 = md("Women  \n(3)"),
        col4 = md("Men  \n(4)"),
        col5 = md("Women  \n(5)"),
        col6 = md("Men  \n(6)")
    ) |>
    style_rd_table(note = "")
