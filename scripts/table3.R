table3_h <- c(1, 0.5, 0.25, 0.1, 0.05) # Custom bandwidth list for table 3
table3_orders <- names(control_functions) # Store the names of the control functions in table3_orders
table3_rows <- c(table3_orders, 'Observations') # define the row labels used in each panel

table3_data <- bind_rows( # stack vertically
    'Panel A: Women' = data.frame(label = table3_rows), # give panel a the rows defined above
    'Panel B: Men' = data.frame(label = table3_rows), # give panel b the rows defined above
    .id = 'panel' # record what panel each row belongs to
)

# Build the rows for one panel at one bandwidth
table3_cells <- function(data) {
    estimates <- character(length(table3_orders)) # Stores 5 empty text entries as a vector - this will be updated by the loop

    for (row in seq_along(table3_orders)) {
        order <- table3_orders[row] #Pick the control function for this row, for example Linear in row 2
        formula <- make_formula(order, regression_controls) # Build the formula with the regression controls
        fit <- fit_clustered(formula, data) # Fit the model with standard errors clustered by province
        estimates[row] <- mayor_cell(fit) # Store the estimate, std error and stars into the estimates vector at position [row]
    }
    c(estimates, as.character(nrow(data))) # Add observation count
}

# Build one table column for one bandwidth.
for (column in seq_along(table3_h)) {
    h <- table3_h[column] # pick the bandwidth for the column
    column_name <- paste0('col', column) # rename 

    # Create the 2 different samples, one for female, one for men.
    women_sample <- make_sample(
        education_sample, 'hischshr1520f', h, include_boundary = FALSE
    )
    men_sample <- make_sample(
        education_sample, 'hischshr1520m', h, include_boundary = FALSE
    )

    table3_data[[column_name]] <- c( # join the two panels results and save them in the current column
        table3_cells(women_sample), # 6 entries
        table3_cells(men_sample) # 6 entries
    )
}

table3 <- table3_data |>
    gt(rowname_col = 'label', groupname_col = 'panel') |>
    tab_header(title = 'TABLE III', subtitle = 'Alternative RD Specifications') |>
    tab_stubhead(label = 'Polynomial order') |>
    tab_spanner(label = 'Bandwidth', columns = col1:col5) |> # Bandwidth shared header
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
