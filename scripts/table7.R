library(haven)
library(tidyverse)
library(estimatr)
library(gt)

df = read_dta('../regdata0.dta')

outcomes <- c('ahischshr15_19f', 'ahischshr15_19m', 'ahischshr15_29f', 'ahischshr15_29m', 'nevm15_19f', 'nevm15_19m') 


# Controls
controls <- c(
    'vshr_islam1994', # Islamic vote share in 1994 
    'partycount', # Number of parties receiving votes 1994
    'lpop1994', # Log population in 1994 
    'ageshr19', # Population share below 19 in 2000
    'ageshr60', # Population share above 60 in 2000
    'sexr', # Gender ratio in 2000
    'shhs', # Household size in 2000 
    'merkezi', # District center
    'merkezp', # Province center
    'subbuyuk', # Sub-metro center, 
    'buyuk' # Metro center
)


# Prepare data
data7 <- df |>
    drop_na(i94, iwm94, pcode, all_of(c(controls, outcomes))) |>
    mutate(d = i94, x = iwm94)

ols <- "d"
ols_controls <- c("d", controls)
local_linear <- c("d", "x", "x:d", controls)

fit_cell <- function(model_data, outcome, terms) {
    formula <- reformulate(terms, response = outcome)
    # pcode groups municipalities by province_pre for the standard errors
    fit <- lm_robust(formula, data = model_data, clusters = pcode, se_type = "stata")
    result <- summary(fit)$coefficients["d", ]
    p_value <- result["Pr(>|t|)"]

    stars <- case_when(
        p_value < 0.01 ~ "***",
        p_value < 0.05 ~ "**",
        p_value < 0.10 ~ "*",
        TRUE ~ ""
    )

    sprintf("%.3f%s\n(%.3f)", result["Estimate"], stars, result["Std. Error"])
}

panel_a <- tibble(label = c(
    "Outcome mean",
    "Model 1. Islamic mayor in 1994 (without controls)",
    "Model 2. Islamic mayor in 1994 (with controls)",
    "Bandwidth",
    "Observations"
))

panel_b <- tibble(label = c(
    "Outcome mean at h",
    "Model 1. Islamic mayor in 1994 (h)",
    "Bandwidth",
    "Observations",
    "Model 2. Islamic mayor in 1994 (h/2)",
    "Bandwidth",
    "Observations",
    "Model 3. Islamic mayor in 1994 (2h)",
    "Bandwidth",
    "Observations"
))

for (j in seq_along(outcomes)) {
    outcome <- outcomes[j]
    column <- paste0("col", j)

    # Calculate h for this outcome. Round only when displaying it.
    h <- rdd::IKbandwidth(data7$x, data7[[outcome]], cutpoint = 0, kernel = "triangular")
    data_h <- data7 |> filter(abs(x) < h)
    data_half <- data7 |> filter(abs(x) < h / 2)
    data_double <- data7 |> filter(abs(x) < 2 * h)

    panel_a[[column]] <- c(
        sprintf("%.3f", mean(data7[[outcome]])),
        fit_cell(data7, outcome, ols),
        fit_cell(data7, outcome, ols_controls),
        "1.00",
        as.character(nrow(data7))
    )

    panel_b[[column]] <- c(
        sprintf("%.3f", mean(data_h[[outcome]])),
        fit_cell(data_h, outcome, local_linear),
        sprintf("%.3f", h),
        as.character(nrow(data_h)),
        fit_cell(data_half, outcome, local_linear),
        sprintf("%.3f", h / 2),
        as.character(nrow(data_half)),
        fit_cell(data_double, outcome, local_linear),
        sprintf("%.3f", 2 * h),
        as.character(nrow(data_double))
    )
}

# Format the two panels using gt, as in Table III
table7 <- bind_rows(
    "Panel A: Global OLS" = panel_a,
    "Panel B: Conditional local linear RD" = panel_b,
    .id = "panel"
) |>
    gt(rowname_col = "label", groupname_col = "panel") |>
    tab_header(
        title = "TABLE VII",
        subtitle = "RD Treatment Effects of Islamic Rule on Education in 2011"
    ) |>
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
    tab_style(
        style = cell_text(whitespace = "pre-line"),
        locations = cells_body(columns = col1:col6)
    ) |>
    tab_source_note(paste(
        "Standard errors clustered by province appear in parentheses.",
        "***, **, and * indicate significance at the 1%, 5%, and 10% levels."
    ))

print(table7)
