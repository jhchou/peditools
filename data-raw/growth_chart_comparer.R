# Exploratory visualization: compare different growth charts at set percentiles
# - use lmsdata available ages rather than interpolating

library(tidyverse)
library(peditools)

load("R/sysdata.rda") # needs lmsdata from sysdata.rda

# Select percentile lines to generate
percentilelist <- c(50)

charts <- c('cdc_2000_infant', 'who_2006_infant', 'who_expanded')

df <- data.frame()
for (chart in charts) {
  lms <- lmsdata[(lmsdata$chart == chart) & (lmsdata$measure == 'weight') & (lmsdata$gender == 'f'), ]
  # Iteratively build a wide dataframe of predicted measurements, starting with one common age source vector, and one column for each Z value
  wide <- lms$age
  # wide <- lms[, c('chart', 'age')] # can't do this because using a MATRIX at first
  for (z in qnorm(percentilelist / 100.0)) { # convert the list of percentiles into a vector of Z scores
      wide <- cbind(wide, peditools::z_lms_to_x(z, lms$L, lms$M, lms$S)) # iteratively add on columns of measurement predictions at each Z score
  }
  wide <- as.data.frame(wide) # convert matrix to dataframe
  names(wide) <- c("age", percentilelist)
  long <- tidyr::gather(wide, key = 'percentile', value = 'measure', -age) # Convert dataframe from wide to long, with three columns: age, chart, percentile, measure
  long$chart <- chart
  df <- bind_rows(df, long)
}

df <- df %>%
  mutate(
    measure = ifelse(chart == 'fenton_2003', measure * 1000, measure),
    age = ifelse(chart == 'who_expanded', age / (365.25/12), age)
  )


# Sample data for patient data points: needs columns age and measure
# patient <- data.frame( # sample patient: GA and weight in grams
#   age = c(27.14285714, 27.85714286, 28.85714286, 29.85714286, 30.85714286, 31.85714286, 32.85714286, 35, 36, 37),
#   measure = c(990, 930, 1000, 1150, 1310, 1600, 1870, 2310, 2570, 2835)
# )
# geom_point(data = patient)

# Plot chart with percentile curves and points
g <- ggplot(mapping = aes(age, measure)) +
  # geom_line(data = df, aes(linetype = chart, color = reorder(percentile, sort(as.numeric(percentile), decreasing = TRUE)))) +
  geom_line(data = df, aes(linetype = chart, color = chart)) +
  scale_color_discrete(name ="Percentiles") +
  # Put bottom-right corner of legend box NEAR bottom-right corner of graph
  # theme(legend.justification = c(1, 0), legend.position = c(0.95, 0.05)) +
  theme_bw()
g
# plotly::ggplotly(g)
