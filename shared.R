# ---- Shared variables and analysis sample ----
education <- c("hischshr1520f", "hischshr1520m")  # High-school completion for women and men aged 15–20.
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

# 
education_sample <- df |>  # Start Tables II and III from the same set of municipalities.
  drop_na(all_of(education), i94, iwm94, pcode, all_of(controls))  # Require both outcomes and all model inputs.

# DEFINING THE CONTROL FUNCTIONS
control_function <- c(
    None = 'd',
    Linear = 'd * x',
    Quadratic = 'd * (x + x2)',
    Cubic = 'd * (x + x2 + x3)',
    Quartic = 'd * (x + x2 + x3 + x4)'
)

# Formula used by table II, III and VII to fit a regression:
fit_clustered <- function(formula, data) {
    lm_robust(
        formula,
        data = data,
        clusters = pcode,
        se_type = 'stata'
    )
}

# Display stars used by tables II, III and VII:
mayor_cell <- function(fit) {
    result <- summary(fit)$coefficients['d', ]
    p_value <- result['Pr(>|t|)']

    stars <- case_when(
        p_value < 0.01 ~ '***',
        p_value < 0.05 ~ '**',
        p_value < 0.1 ~ '*',
        TRUE ~ ''
    )

    sprintf('%.3f%s\n(%.3f)', result['Estimate'], stars, result['Std. Error'])
}

style_rd_table <- function(gt_table, note) {
    gt_table |>
        tab_style(
            style = cell_text(whitespace = 'pre-line'),
            locations = cells_body(columns = starts_with('col'))
        ) |>
        tab_source_note(paste(
            note,
            'Standard errors clustered by province appears in parenthesis.',
            '***, **, and * indicate the significance at the 1%, 5% and 10% levels.'
        ))
}