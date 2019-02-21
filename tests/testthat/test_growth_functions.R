context("LMS Growth functions")

test_that("x_to_z is vectorized", {

  # > lmsData %>% filter(Chart == 'Fenton2013', Age %in% c(37, 38, 39), Measure == 'Wt')
  #        Chart Age AgeUnits Gender Measure MeasureUnits       L        M       S
  # 1 Fenton2013  37    weeks      M      Wt            g 0.74907 2946.661 0.15083
  # 2 Fenton2013  38    weeks      M      Wt            g 0.66489 3155.875 0.14297
  # 3 Fenton2013  39    weeks      M      Wt            g 0.59196 3360.478 0.13657
  # 4 Fenton2013  37    weeks      F      Wt            g 0.34943 2835.085 0.16132
  # 5 Fenton2013  38    weeks      F      Wt            g 0.26790 3049.668 0.15490
  # 6 Fenton2013  39    weeks      F      Wt            g 0.20947 3239.306 0.15011
  
  # x <- c(2946.661, 3155.875, 3360.478, 2835.085, 3049.668, 3239.306)
  # age <- c(37, 38, 39, 37, 38, 39)
  # gender <- c('M','M','M','F','F','F')
  # gender <- c('F','F','F','M','M','M')
  # gender <- 'F'
  #
  # x_to_z(x, age, gender, chart = 'Fenton2013', measure = 'Wt')
  # x_to_z(x, age, gender, chart = 'Fenton2013', measure = 'Wt') %>% pnorm() %>% round(3)

  x      <- c(2946.661, 3155.875, 3360.478, 2835.085, 3049.668, 3239.306) # 50%ile
  age    <- c(      37,       38,       39,       37,       38,       39)
  gender <- c(     'M',      'M',      'M',      'F',      'F',      'F')
  expect_equal(round(x_to_z(x, age, gender, chart = 'Fenton2013', measure = 'Wt'), 4), rep(0, length(x)))

  x      <- c(2391.340352, 2595.734374, 2793.553717, 2287.54664, 2486.917178, 2661.786987) # 10%ile
  expect_equal(round(pnorm(x_to_z(x, age, gender, chart = 'Fenton2013', measure = 'Wt')), 4), rep(0.1, length(x)))
})
