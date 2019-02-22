context("LMS Growth functions")

test_that("x_to_z is vectorized", {

  # > lmsdata %>% filter(chart == 'cdc_2000_infant', age %in% c(10.5, 11.5, 12.5), measure == 'weight')
  #             chart  age age_units gender measure measure_units           L         M         S
  # 1 cdc_2000_infant 10.5    months      m  weight            kg -0.17429685  9.835308 0.1113545
  # 2 cdc_2000_infant 11.5    months      m  weight            kg -0.17971890 10.161536 0.1106764
  # 3 cdc_2000_infant 12.5    months      m  weight            kg -0.17925400 10.458854 0.1101186
  # 4 cdc_2000_infant 10.5    months      f  weight            kg -0.00968493  9.043262 0.1086608
  # 5 cdc_2000_infant 11.5    months      f  weight            kg -0.08525800  9.366594 0.1076562
  # 6 cdc_2000_infant 12.5    months      f  weight            kg -0.15640945  9.666089 0.1068345

  # gender <- c('m','m','m','f','f','f') # correct order
  # gender <- c('f','f','f','m','m','m') # swap genders
  # gender <- 'f'                        # all female
  #
  # x_to_z(x, age, gender, chart = 'cdc_2000_infant', measure = 'weight') %>% round(4)             # show Z
  # x_to_z(x, age, gender, chart = 'cdc_2000_infant', measure = 'weight') %>% pnorm() %>% round(3) # show percentile
  
  x      <- c(9.835308, 10.161536, 10.458854, 9.043262, 9.366594, 9.666089) # 50%ile
  age    <- c(    10.5,      11.5,      12.5,     10.5,     11.5,     12.5)
  gender <- c(     'm',       'm',       'm',      'f',      'f',      'f')
  expect_equal(round(x_to_z(x, age, gender, chart = 'cdc_2000_infant', measure = 'weight'), 4), rep(0, length(x)))

  x      <- c(8.542195484, 8.833485623, 9.098245709, 7.868436369, 8.166068895, 8.441459662) # 10%ile
  expect_equal(round(pnorm(x_to_z(x, age, gender, chart = 'cdc_2000_infant', measure = 'weight')), 4), rep(0.1, length(x)))
})
