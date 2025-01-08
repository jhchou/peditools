# Exploratory visualization: generate growth charts with arbitrary percentile lines, and plot patient data points
# - use lmsdata available ages rather than interpolating

library(ggplot2)

load("R/sysdata.rda") # needs lmsdata from sysdata.rda

# Extract one specific chart's data for age / L / M / S
# lms <- lmsdata[(lmsdata$chart == 'fenton_2013') & (lmsdata$measure == 'weight') & (lmsdata$gender == 'm'), ]
# lms <- lmsdata[(lmsdata$chart == 'brooks_gmfcs_5_tf') & (lmsdata$measure == 'weight') & (lmsdata$gender == 'f'), ]
lms <- lmsdata[(lmsdata$chart == 'cappa_2024') & (lmsdata$measure == 'weight') & (lmsdata$gender == 'm'), ]

lms <- lms[order(lms$age), ] # for safety and robustness, ensure dataframe sorted by age


# Select percentile lines to generate
percentilelist <- c(10, 50, 90)

# Iteratively build a wide dataframe of predicted measurements, starting with one common age source vector, and one column for each Z value
wide <- lms$age
for (z in qnorm(percentilelist / 100.0)) { # convert the list of percentiles into a vector of Z scores
    wide <- cbind(wide, peditools::z_lms_to_x(z, lms$L, lms$M, lms$S)) # iteratively add on columns of measurement predictions at each Z score
}
wide <- as.data.frame(wide) # convert matrix to dataframe
names(wide) <- c("age", percentilelist)

# Convert dataframe from wide to long, with three columns: age, percentile, measure
long <- tidyr::gather(wide, key = 'percentile', value = 'measure', -age)

# Sample data for patient data points: needs columns age and measure
patient <- data.frame( # sample patient: GA and weight in grams
  age = c(27.14285714, 27.85714286, 28.85714286, 29.85714286, 30.85714286, 31.85714286, 32.85714286, 35, 36, 37),
  measure = c(990, 930, 1000, 1150, 1310, 1600, 1870, 2310, 2570, 2835)
)

# Plot chart with percentile curves and points
# - incidentally, lms$measure[] should contain a single unique measure type as factor, so could take 
g <- ggplot(mapping = aes(age, measure)) +
  geom_line(data = long, aes(color = reorder(percentile, sort(as.numeric(percentile), decreasing = TRUE)))) +
  scale_color_discrete(name ="Percentiles") +
  # Put bottom-right corner of legend box NEAR bottom-right corner of graph
  # geom_point(data = patient) +
  coord_cartesian(ylim = c(0, 70)) +
  theme(legend.justification = c(1, 0), legend.position.inside = c(0.95, 0.05))
g
