# Exploratory visualization: compare different growth charts at set percentiles
# - use lmsdata available ages rather than interpolating

library(tidyverse)
library(peditools)

# load("R/sysdata.rda") # needs lmsdata from sysdata.rda

add_lmsdata(read.csv("https://opendataarchive.github.io/raw-data/fenton2013_lmsdata.csv"))
add_lmsdata(read.csv("https://opendataarchive.github.io/raw-data/fenton2025_lmsdata.csv"))
lmsdata <- get_lmsdata()

# Select percentile lines to generate
# percentilelist <- c(3, 10, 50, 90, 97)
# percentilelist <- c(3, 50, 97)
percentilelist <- c(3, 10, 50, 90, 97)

# charts <- c('cdc_2000_infant', 'who_2006_infant', 'who_expanded')
# charts <- c('fenton_2013', 'fenton_2025', 'who_expanded')
# charts <- c('fenton_2013', 'fenton_2025', 'who_2006_infant', 'who_expanded')
# charts <- c('fenton_2003', 'olsen_2010', 'fenton_2013', 'fenton_2025', 'who_2006_infant', 'who_expanded')
charts <- c('fenton_2025', 'who_2006_infant', 'who_expanded')

df <- data.frame()
for (chart in charts) {
  lms <- lmsdata[(lmsdata$chart == chart) & (lmsdata$measure == 'weight') & (lmsdata$gender == 'm'), ]
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
    
    measure = ifelse(chart == 'who_expanded', measure * 1000, measure),
    # age = ifelse(chart == 'who_expanded', 40 + age/7, age), # For Fenton 2013, add 40 to harmonize
    age = ifelse(chart == 'who_expanded', 40.5 + age/7, age), # For Fenton 2025, add 40.5 to harmonize -- CHANGED so that "term" means 40.5 instead of 40
    
    measure = ifelse(chart == 'who_2006_infant', measure * 1000, measure),
    # age = ifelse(chart == 'who_2006_infant', 40 + (age*30.5)/7, age) # For Fenton 2013, add 40 to harmonize
    age = ifelse(chart == 'who_2006_infant', 40.5 + (age*30.5)/7, age) # For Fenton 2025, add 40.5 to harmonize -- CHANGED so that "term" means 40.5 instead of 40
  )

df <- df |> filter(age <= 50)

# Sample data for patient data points: needs columns age and measure
# patient <- data.frame( # sample patient: GA and weight in grams
#   age = c(27.14285714, 27.85714286, 28.85714286, 29.85714286, 30.85714286, 31.85714286, 32.85714286, 35, 36, 37),
#   measure = c(990, 930, 1000, 1150, 1310, 1600, 1870, 2310, 2570, 2835)
# )
# geom_point(data = patient)

# Plot chart with percentile curves and points
g <- ggplot(mapping = aes(age, measure)) +
  geom_line(data = df, aes(linetype = chart, color = reorder(percentile, sort(as.numeric(percentile), decreasing = TRUE)))) +
  # geom_line(data = df, aes(linetype = chart, color = chart)) +
  scale_color_discrete(name ="Percentiles") +
  # Put bottom-right corner of legend box NEAR bottom-right corner of graph
  # theme(legend.justification = c(1, 0), legend.position = c(0.95, 0.05)) +
  theme_bw() +
  labs(
    title = "Comparison of Weight-for-Age Growth Charts (10/50/90 percentiles)",
    x = "Post-menstrual Age (weeks)",
    y = "Weight (grams)"
  )
# g
plotly::ggplotly(g)
