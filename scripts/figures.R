# ============================================================
# Replication of Figures from Meyersson (2014)
# TECH6 - Data Science and Econometrics
# ============================================================

# ============================================================
#Figure 1
# ============================================================

# Keep only non-missing values of Islamic vote share and Islamic win margin
fig1_data <- df[complete.cases(df$vshr_islam1994, df$iwm94), ]

fig1 <- ggplot(fig1_data, aes(x=vshr_islam1994, y=iwm94)) +
# Highlight points within +/- 2% of winning cutoff (close elections)
  geom_point(color=ifelse(abs(fig1_data$iwm94)<=0.02, "black", "azure4")) +
  xlim(0, 0.75) +
  labs(x="Islamic vote share", y="Islamic win margin") +
# Win margin = 2*Islamic vote share -1 (diagonal line)
  geom_abline(intercept=-1, slope=2, color="black") +
  geom_abline(intercept=0, slope=0, color="azure3") +
  theme_classic()


# ============================================================
#Figure 2
# ============================================================

# Histogram of the running variable

# Keep only non-missing values of Islamic win margin
fig2_data <- df[complete.cases(df$iwm94), ]

fig2a <- ggplot(fig2_data, aes(x=iwm94, y=after_stat(count/sum(count)))) +
# Display distribution of running variable with binwidth=0.02
  geom_histogram(binwidth = 0.02, fill = "azure4", color="black")+
  scale_y_continuous(labels=percent) +
  xlim(-1, 1) +
  labs(x="Islamic win margin", y="Percent") +
# Cutoff line at 0, the threshold for winning the election
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_classic()

# Density test

# Keep only non-missing values of the outcome variable and running variable within +/- 0.6
mctest_data <- df[
  complete.cases(df$hischshr1520f, df$iwm94) &
  abs(df$iwm94)< 0.6, ] 

# Organizing the data for the analysis with "rdd_data()", cutoff=0.
density_data <- rdd_data(y=mctest_data$hischshr1520f, x=mctest_data$iwm94, cutpoint=0)
# McCrary density test: Checking for discontinuity in the density of the running variable at the cutoff
save_density_plot <- function(filename) {
  png(filename, width = 1800, height = 1200, res = 220)

  density_test <- dens_test(density_data, plot = TRUE)

  abline(v=0, lty=2, col='black')

  title(xlab = 'Islamic win margin 1994', ylab = 'Density')

  dev.off()
}

# ============================================================
#Figure 3
# ============================================================

# List of covariates examined around the cutoff
covariates <- c(
  "vshr_islam1994",
  "ageshr60",
  "ageshr19",
  "lpop1994",
  "sexr", 
  "anyc", 
  "partycount",
  "shhs"
)

# Giving each covariate a label for the y-axes in the plots
covariate_labels <- c(
  vshr_islam1994="Islamic vote share",
  ageshr60= "Age 60+",
  ageshr19= "Age 19",
  lpop1994= "Log population",
  sexr= "Gender ratio",
  anyc= "Any center municipality",
  partycount= "Number of parties",
  shhs= "Household size"
)

# Empty list to insert plots
plots <- list()

# Creating one plot per covariate
for (i in covariates) {

# Keep only non-missing values of each covariate and the running variable
cov_data <- df[
  complete.cases(df[[i]], df$iwm94), ]

# Creating bins with trunc(), truncating the win margin toward zero
cov_data$bin <- trunc(cov_data$iwm94*25/2)

# Separate values below and above cutoff
cov_data$side <- cov_data$iwm94 > 0

# Finding mean election margin and covariate value inside each bin
grouped_data <- group_by(cov_data, side, bin)

means <- summarise(grouped_data, average_margin=mean(iwm94), average_vote=mean(.data[[i]]), .groups = "drop")
means <- means[abs(means$average_margin) < 0.3, ] #Limit points to between -0.3 and 0.3

#Plotting function
p <- ggplot(means, aes(x=average_margin, y=average_vote)) +
  geom_point(size=3, color="#242424")+
# Fitting LOESS curve + confidence intervals on both cutoff sides
# Note: Meyersson uses local linear smoothing with bandwidth 0.06, not LOESS
  geom_smooth(data=subset(cov_data, abs(iwm94)<0.3),
  aes(x=iwm94, y=.data[[i]], group=side),
  method="loess",
  se=TRUE,
  color="black",
  linewidth=0.5)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  labs(
    x=NULL,
    y=covariate_labels[[i]]
  )+
  theme_classic()
# As in Meyersson (2014), we put x-axis labels only in the bottom row
  if (i=="partycount") {
    p <- p+ labs(
    x="Islamic win margin",
    y=covariate_labels[[i]]
  )
  }
  else if (i=="shhs") {
    p <- p+ labs(
      x="Islamic win margin",
      y=covariate_labels[[i]]
  )
  }
  else {
    p <- p+ labs(
    x=NULL,
    y=covariate_labels[[i]]
  )+
  theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank()
      )
  }

plots[[i]] <- p
}

# Combining all eight covariate plots into one
fig3 <- wrap_plots(plots, ncol=2)


# ============================================================
#Figure 4
# ============================================================

# Outcomes by gender and year 1990 vs 2000
outcomes_4 <- c(
  "hischshr1520f",
  "hischshr1520m",
  "c90hischshr1520f",
  "c90hischshr1520m"
)

# Titles for plot of each outcome
main_names <- c(
  hischshr1520f= "a) Women in 2000",
  hischshr1520m= "b) Men in 2000",
  c90hischshr1520f= "c) Women in 1990",
  c90hischshr1520m= "d) Men in 1990"
)

# Empty list to insert plots
plots_fig4 <- list()

for (i in outcomes_4) {

# Keep only non-missing values of outcome and the running variable
outcome_data4 <- df[
  complete.cases(df[[i]], df$iwm94), ]

# Creating bins with trunc(), truncating the win margin toward zero
outcome_data4$bin <- trunc(outcome_data4$iwm94*25)

# Separate values below and above cutoff
outcome_data4$side <- outcome_data4$iwm94 > 0

# Finding mean election margin and outcome inside each bin
grouped_data <- group_by(outcome_data4, side, bin)

means <- summarise(grouped_data, average_margin=mean(iwm94), average_outcome=mean(.data[[i]]), .groups = "drop")
means <- means[abs(means$average_margin) < 0.5, ] #Limit points to between -0.5 and 0.5

# Plotting function
p <- ggplot(means, aes(x=average_margin, y=average_outcome)) +
  geom_point(size=3, color="#242424")+
# Fitting LOESS curve + confidence intervals on both cutoff sides
# Note: Meyersson uses local linear smoothing, not LOESS
  geom_smooth(data=subset(outcome_data4, abs(iwm94)<0.5),
  aes(x=iwm94, y=.data[[i]], group=side),
  method="loess",
  se=TRUE,
  color="black",
  linewidth=0.5)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  labs(
    y="Share of 15-20-year-olds with High School Degree",
    x="Islamic win margin 1994",
    title=main_names[[i]]
  )+
  theme_classic()+
# Define the y-axis range for our graphs
  coord_cartesian(ylim=c(-0.05, 0.25))

plots_fig4[[i]] <- p
}

# Combine all outcome plots
fig4 <- wrap_plots(plots_fig4, ncol=2)+
# Create shared axis titles
plot_layout(axis_titles="collect")