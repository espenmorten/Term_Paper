# =======================================================================================
# Shared variables, data processing, functions and formatting rules
# =======================================================================================
education_outcomes <- c("hischshr1520f", "hischshr1520m")  # High-school completion for women and men aged 15–20.

summary_covariates <- c( # Define covariates in table 1 (summary)
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

regression_controls <- c(summary_covariates, "buyuk", province_dummies)  # Regressions also use the summary covariates, metro status and province indicators as controls

rd_base_sample <- df |> 
    mutate(d = i94, x = iwm94, x2 = x^2, x3 = x^3, x4 = x^4)|> # Add column d (treatment), x (running variable)
    drop_na(d, x, pcode, all_of(regression_controls))

#Tables 2 and 3 start from this data
education_sample <- rd_base_sample |>
    drop_na(all_of(education_outcomes))

# Defining the control functions available
control_functions <- c(
    None = 'd', # The simple mean, jump at cutoff
    Linear = 'd * x', # Expands to d + x + d:x, where d:x is the interaction term and allows different slopes on each side of the cutoff
    Quadratic = 'd * (x + x2)', # Expands to d + x + x2 + d:x + d:x2, allows for different rates of 
    Cubic = 'd * (x + x2 + x3)',
    Quartic = 'd * (x + x2 + x3 + x4)'
)

# In the paper, Meyerson says that he uses the Imbens & Kalyanaraman algorithm from 2012 to determine
# optimal bandwidth. This algorithm is available as a function in an old version of rddtools
ik_h <- function(data) {
    h <- rdd::IKbandwidth(data$x, data$y, cutpoint = 0, kernel = 'triangular')
}

make_sample <- function(data, outcome, h, include_boundary = TRUE) {
    model_sample <- data
    model_sample$y <- model_sample[[outcome]] # Assigns the outcome variable to y, for example hischshr1520f
    model_sample <- drop_na(model_sample, y) 

    if (include_boundary) {
        model_sample <- filter(model_sample, abs(x) <= h) # We have 1 municipality at the vote margin (x) -1. This include_boundary function includes that when set to TRUE
    } else {
        model_sample <- filter(model_sample, abs(x) < h)
    }
    model_sample
}

# Builds the regression formula
make_formula <- function(order, controls = character(0)) {
    terms <- c(control_functions[[order]], controls) # Looks up and assigns the order of the model + any regression controls to terms.
    reformulate(terms, response = 'y') # Formulates the regression as a function, y on the LHS (for example y ~ d * (x + x2) would expand to y ~ d + x + x2 + d:x + d:x2, where d:{} is the interaction terms)
}

# Formula used by table 2, 3 and 7 to fit a regression:
fit_clustered <- function(formula, data, se_type = 'stata') {
    lm_robust(
        formula, # The formula made in make_formula
        data = data, # The data supplied, for example women_sample
        clusters = pcode, # Groups municipalities by province to deal with possible correlation of regression errors from municipalities within the same province.
                          # We're assuming independence across provinces. 
        se_type = se_type # Chooses the calculation for the standard error, with adjustment 
    )
}

format_estimate <- function(estimate, standard_error, p_value = NA_real_) {
    stars <- case_when( # Display stars used by all tables
        p_value < 0.01 ~ '***', # 1% significance level
        p_value < 0.05 ~ '**', # 5% significance level
        p_value < 0.1 ~ '*', # 10% significance level
        TRUE ~ '' # Empty strings at or above 10%
    )

    sprintf('%.3f%s  \n(%.3f)', estimate, stars, standard_error) # Formatting with 3 decimals and linebreak
}

mayor_cell <- function(fit) {
    result <- summary(fit)$coefficients['d', ] # Stores the summary in result
    format_estimate(result['Estimate'], result['Std. Error'], result['Pr(>|t|)']) # Calling the format_estimate function defined above with the coefficient columns defined
}
significance_note <- '***, ** and * indicate significance at the 1%, 5% and 10% levels.' # Note for many of the tables

style_rd_table <- function(gt_table, note = NULL) { #Standardize formatting
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