# ---- Shared variables and analysis sample ----
education_outcomes <- c("hischshr1520f", "hischshr1520m")  # High-school completion for women and men aged 15–20.
# define covariates
covariates <- c(
    'vshr_islam1994', # Islamic vote share in 1994
    'partycount', # Number of parties receiving votes 1994
    'lpop1994', # Log population in 1994
    'ageshr19', # Population share below 19 in 2000
    'ageshr60', # Population share above 60 in 2000
    'sexr', # Gender ratio in 2000
    'shhs', # Household size in 2000
    'merkezi', # District center
    'merkezp', # Province center
    'subbuyuk' # Sub-metro center
)

province_dummies <- names(select(df, starts_with("pd_")))  # Find all province indicator columns.

controls <- c(covariates, "buyuk", province_dummies)  # Regressions also use metro status and province indicators.

rd_data <- df |>
    mutate(d = i94, x = iwm94, x2 = x^2, x3 = x^3, x4 = x^4)|>
    drop_na(d, x, pcode, all_of(controls))

#Tables 2 and 3 start from this data
education_sample <- rd_data |>
    drop_na(all_of(education_outcomes))

# DEFINING THE CONTROL FUNCTIONS
control_function <- c(
    None = 'd',
    Linear = 'd * x',
    Quadratic = 'd * (x + x2)',
    Cubic = 'd * (x + x2 + x3)',
    Quartic = 'd * (x + x2 + x3 + x4)'
)

ik_bandwidth <- function(data) {
    bandwidth <- rdd::IKbandwidth(data$x, data$y, cutpoint = 0, kernel = 'triangular')
}

make_sample <- function(data, outcome, h, include_boundary = TRUE) {
    model_data <- data
    model_data$y <- model_data[[outcome]]
    model_data <- drop_na(model_data, y)

    if (include_boundary) {
        model_data <- filter(model_data, abs(x) <= h)
    } else {
        model_data <- filter(model_data, abs(x) < h)
    }
    model_data
}

make_formula <- function(order, covariates = character(0)) {
    terms <- c(control_function[[order]], covariates)
    reformulate(terms, response = 'y')
}

# Formula used by table 2, 3 and 7 to fit a regression:
fit_clustered <- function(formula, data, se_type = 'stata') {
    lm_robust(
        formula,
        data = data,
        clusters = pcode,
        se_type = se_type
    )
}

# Display stars used by tables 2, 3, and 7
format_estimate <- function(estimate, standard_error, p_value = NA_real_) {
    stars <- case_when(
        p_value < 0.01 ~ '***',
        p_value < 0.05 ~ '**',
        p_value < 0.1 ~ '*',
        TRUE ~ ''
    )

    sprintf('%.3f%s  \n(%.3f)', estimate, stars, standard_error)
}

mayor_cell <- function(fit) {
    result <- summary(fit)$coefficients['d', ]
    format_estimate(result['Estimate'], result['Std. Error'], result['Pr(>|t|)'])
}
significance_note <- '***, ** and * indicate significance at the 1%, 5% and 10% levels.'

style_rd_table <- function(gt_table, note = NULL) {
    source_note <- c(
        note, 
        'Standard errors, clustered by provinces in parentheses, are reported in parentheses.',
        significance_note
    )
    
    gt_table |>
        tab_style(
            style = cell_text(whitespace = 'pre-line'),
            locations = cells_body(columns = starts_with('col'))
        ) |>
        tab_source_note(paste(source_note, collapse = ' '))
}