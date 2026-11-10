table3_h <- c(1, 0.5, 0.25, 0.1, 0.05) # Custom bandwidth list for table 3
table3_orders <- names(control_functions)
table3_rows <- c(table3_orders, 'Observations')

table3_data <- bind_rows(
    'Panel A: Women' = data.frame(label = table3_rows),
    'Panel B: Men' = data.frame(label = table3_rows),
    .id = 'panel'
)

table3_cells <- function(data) {
    estimates <- character(length(table3_orders))

    for (row in seq_along(table3_orders)) {
        order <- table3_orders[row]
        formula <- make_formula(order, regression_controls)
        fit <- fit_clustered(formula, data)
        estimates[row] <- mayor_cell(fit)
    }
    c(estimates, as.character(nrow(data)))
}

for (column in seq_along(table3_h)) {
    h <- table3_h[column]
    column_name <- paste0('col', column)

    women_sample <- make_sample(
        education_sample, 'hischshr1520f', h, include_boundary = FALSE
    )
    men_sample <- make_sample(
        education_sample, 'hischshr1520m', h, include_boundary = FALSE
    )

    table3_data[[column_name]] <- c(
        table3_cells(women_sample),
        table3_cells(men_sample)
    )
}

table3 <- table3_data |>
    gt(rowname_col = 'label', groupname_col = 'panel') |>
    tab_header(title = 'TABLE III', subtitle = 'Alternative RD Specifications') |>
    tab_stubhead(label = 'Polynomial order') |>
    tab_spanner(label = 'Bandwidth', columns = col1:col5) |>
    cols_label(
        col1 = md('1  \n(1)'),
        col2 = md('0.5  \n(2)'),
        col3 = md('0.25  \n(3)'),
        col4 = md('0.1  \n(4)'),
        col5 = md('0.05  \n(5)')
    ) |>
    style_rd_table(
        paste(
            'The outcome is the share of 15-20 year olds who completed high school.',
            'All specifications include the Table II controls.'
        )
    )
