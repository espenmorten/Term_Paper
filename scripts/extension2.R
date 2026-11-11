# ============================================================
# Extension 2: Missing data and sample selection
# ============================================================
extension_rd_rows <- c( # The names of the rows in our table
    'Conventional RD estimate',
    'Conventional 95% CI',
    'Bias-corrected RD estimate',
    'Robust bias-corrected 95% CI',
    'Estimation bandwidth (h)',
    'Bias bandwidth (b)',
    'Municipalities within h'
)

election_sample <- df |>
    drop_na(i94, iwm94, pcode) |>
    mutate(x = iwm94, has_2011_data = as.integer(!is.na(ahischshr15_19f))) # we add a column that is equal to 1 if the municipality has data in 2011, 0 otherwise

availability_sample <- make_sample(election_sample, 'has_2011_data', h = Inf) # the sample of municipalities 
availability_fit <- fit_extension_rd(availability_sample)

completion_2000_sample <- make_sample(election_sample, 'hischshr1520f', h = Inf)
both_sample <- completion_2000_sample |>
    filter(has_2011_data == 1)

completion_2000_fit <- fit_extension_rd(completion_2000_sample)
h <- completion_2000_fit$bws['h', 'left']
b <- completion_2000_fit$bws['b', 'left']

both_fit <- fit_extension_rd(both_sample, h = h, b = b)

extension2_data <- data.frame(
    label = extension_rd_rows,
    availability = extension_rd_cells(availability_fit),
    all_2000 = extension_rd_cells(completion_2000_fit),
    both_years = extension_rd_cells(both_fit)
)

extension2_table <- extension2_data |>
    gt(rowname_col = 'label') |>
    tab_header(title = 'Extension2', subtitle = '2011 availability and sample selection') |>
    tab_spanner('High school completion in 2000', columns = c(all_2000, both_years)) |>
    cols_label(
        availability = 'Jump in 2011 availability',
        all_2000 = '2000 outcome recorded',
        both_years = 'Both outcomes recorded'
    )

availability_plot_result <- rdplot(
    y = availability_sample$y,
    x = availability_sample$x,
    c = 0,
    p = 1,
    h = availability_fit$bws['h', ],
    kernel = 'triangular',
    x.lim = c(-0.3, 0.3),
    x.label = 'Islamic win margin in 1994',
    y.label = 'Share with 2011 outcome observed',
    title = 'Availability of 2011 female education data',
    hide = TRUE,
)

extension2_plot <- availability_plot_result$rdplot