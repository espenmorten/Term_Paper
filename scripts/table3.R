# Prepare data
data3 <- education_sample |>
    mutate(d = i94, x = iwm94, x2 = x^2, x3 = x^3, x4 = x^4)

# Custom bandwidth list for table 3
h_list <- c(1, 0.5, 0.25, 0.1, 0.05)

make_sample <- function(outcome, bandwidth) {
    model_data <- data3
    model_data$y <- model_data[[outcome]]
}


make_table3_panel <- function(outcome) {
    panel <- tibble(label = c(names(control_function), 'Observations'))

    for (column in seq_along(h_list)) {
        bandwidth <- h_list[column]
        model_data <- data3 |> filter(abs(x) < bandwidth)
        model_data$y <- model_data[[outcome]]
        estimates <- character(length(control_function))

        for (row in seq_along(control_function)) {
            terms <- c(control_function[[row]], controls)
            formula <- reformulate(terms, response = 'y')

            fit <- fit_clustered(formula, model_data)
            estimates[row] <- mayor_cell(fit)
        }

        panel[[paste0('col', column)]] <- c(
            estimates, as.character(nrow(model_data))
        )
    }

    panel
}

women <- make_table3_panel('hischshr1520f')
men <- make_table3_panel('hischshr1520m')

# Format the two panels using gt.
table3 <- bind_rows(
    'Panel A: Women' = women,
    'Panel B: Men' = men,
    .id = 'panel'
) |>
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

print(table3)