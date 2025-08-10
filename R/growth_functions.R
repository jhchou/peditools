########## LMS growth chart functions ##########
#
# lmsdata (in R/sysdata.rda) is R an object of LMS parameters for a collection of charts
#
# To do:
# [ ] consider not exporting all functions, like get_lms, x_lms_to_z
# [x] Should include table of charts and their measures in help for x_to_z
# [x] rename functions
# [ ] unit tests



# Open code outside of a function -- apparently this is run once when the package is first loaded?
# - lmsdata from sysdata.rda does not appear to be accessible outside of functions
# - so instead, will initialize it to NULL here, and then populate / retrieve it from a getter function
data_env <- new.env(parent = emptyenv())
data_env$lmsdata <- NULL


#' Get lmsdata
#' 
#' Function to return the dataframe containing all LMS values, measures, and units of measure, including any added with add_lmsdata()
#' @export
#' @examples
#' head(get_lmsdata())
get_lmsdata <- function() {
  if (is.null(data_env$lmsdata)) {
    # print('loading for first time')
    data_env$lmsdata <- lmsdata
  }
  return(data_env$lmsdata)
}


#' Add lmsdata
#' 
#' Function to add to lmsdata in data_env environment
#' @param df Dataframe of new LMS data to add
#' @export
add_lmsdata <- function(df) {
  # Should make this much safer, by handling:
  # [ ] chart to add already exists
  # [ ] missing or additional columns (should make rbind fail, as opposed to bind_rows in tidyverse?)
  lmsdata <- get_lmsdata() # need to run this once first, or built-in lmsdata will never get loaded
  data_env$lmsdata <- unique(rbind(data_env$lmsdata, df)) # unique to prevent duplicates if add more than once
}


#' Get chart metadata
#' 
#' Function to return the dataframe containing chart metadata, including chart name, anthropometric measure and units,
#' age label and units with min, max, and range, number of parameters, source text and URL, and notes
#' @export
#' @examples
#' head(get_chartmetadata())
get_chartmetadata <- function() {
  if (is.null(data_env$chart_metadata)) {
    # print('loading for first time')
    data_env$chart_metadata <- chart_metadata
  }
  return(data_env$chart_metadata)
}



#' Get LMS parameters function
#'
#' Function to interpolate LMS parameters, given age, gender, chart, and measure
#' @param age Vector of ages
#' @param gender Vector of gender, either 'm' or 'f'
#' @param chart Which chart to obtain LMS parameters for
#' @param measure Which measure for the chart. Defaults to 'weight'
#' @export
#' @examples
#' get_lms(c(11.5, 11.5), c('m', 'f'), chart = 'cdc_2000_infant', measure = 'weight')
get_lms <- function( age, gender, chart, measure = 'weight' ) {
  # function to interpolate LMS parameters, given vector of age, vector of gender, unique chart, and unique measure
  # - approxfun does NOT extrapolate, so will automatically remain within range of chart, or else return NA
  
  lmsdata <- get_lmsdata() # load lmsdata from the created environment

  if ((length(chart) != 1) | (length(measure) != 1)) {
    stop('Not vectorized over charts or measures -- needs a unique chart and measure', call. = FALSE)
  }
  
  # To allow vectorized function over gender, will actually calculate for both genders, and then select the appropriate number at end

  # MALE
  lms <- lmsdata[ (lmsdata$chart == chart) & (lmsdata$measure == measure) & (lmsdata$gender) == 'm', ]
  if (nrow(lms) == 0) { # No chart matched
    stop('No chart / measure matched', call. = FALSE)
  }
  # generate functions to interpolate LMS parameters
  lms <- lms[ order(lms$age), ] # for safety and robustness, ensure dataframe sorted by Age
  fxn_l_male <- stats::approxfun( lms$age, lms$L, ties = "ordered" ) # stats::approxfun: linear interpolation; returns NA if out of range
  fxn_m_male <- stats::approxfun( lms$age, lms$M, ties = "ordered" )
  fxn_s_male <- stats::approxfun( lms$age, lms$S, ties = "ordered" )
  lms_male <- list(L_male = fxn_l_male(age), M_male = fxn_m_male(age), S_male = fxn_s_male(age))

  # FEMALE
  lms <- lmsdata[ (lmsdata$chart == chart) & (lmsdata$measure == measure) & (lmsdata$gender) == 'f', ]
  if (nrow(lms) == 0) { # No chart matched
    stop('No chart / measure matched', call. = FALSE)
  }
  # generate functions to interpolate LMS parameters
  lms <- lms[ order(lms$age), ] # for safety and robustness, ensure dataframe sorted by Age
  fxn_l_female <- stats::approxfun( lms$age, lms$L, ties = "ordered" ) # stats::approxfun: linear interpolation; returns NA if out of range
  fxn_m_female <- stats::approxfun( lms$age, lms$M, ties = "ordered" )
  fxn_s_female <- stats::approxfun( lms$age, lms$S, ties = "ordered" )
  lms_female <- list(L_female = fxn_l_female(age), M_female = fxn_m_female(age), S_female = fxn_s_female(age))

  gender <- tolower(substr(gender, 1, 1))

  df <- data.frame(age, gender, lms_male, lms_female) # if gender is a vector of length 1, will be expanded to fill dataframe
  
  # Select the correct male versus female LMS parameter, depending on df$gender
  # - if neither 'm' or 'f', fill in NA
  df$L <- ifelse(
    df$gender == 'm',
    df$L_male,
    ifelse(
      df$gender == 'f',
      df$L_female,
      NA
    )
  )

  df$M <- ifelse(
    df$gender == 'm',
    df$M_male,
    ifelse(
      df$gender == 'f',
      df$M_female,
      NA
    )
  )

  df$S <- ifelse(
    df$gender == 'm',
    df$S_male,
    ifelse(
      df$gender == 'f',
      df$S_female,
      NA
    )
  )

  return( list( L = df$L, M = df$M, S = df$S,
                Chart        = as.character(lms[1, 'chart']),
                AgeUnits     = as.character(lms[1, 'age_units']),
                Measure      = as.character(lms[1, 'measure']),
                MeasureUnits = as.character(lms[1, 'measure_units'])
  ) )

}


#' X and LMS to Z function
#'
#' function to convert X + LMS parameters to a Z-score
#' @param x Vector of measurements
#' @param lms List of L, M, and S elements, each with length(x) elements
#' @export
#' @examples
#' # x_lms_to_z()
x_lms_to_z <- function( x, lms ) {
    # function to convert x + LMS parameters to Z, works vectorized
    # lms input is a list with L, M, and S elements
    L = lms[['L']]
    M = lms[['M']]
    S = lms[['S']]
    ifelse( L != 0,
            ( (x/M)**L - 1 ) / ( L * S ), # for L != 0
            log(x/M) / S                  # for L == 0, log is natural logarithm
    )
}


#' Z and LMS to X function
#'
#' Vectorized function to convert Z + LMS parameters to a measurement. If Z, L, M, or S are vectors of different length, will recycle the shorter vectors.
#' @param Z Z parameter(s)
#' @param L lambda parameter(s)
#' @param M mu parameter(s)
#' @param S sigma parameter(s)
#' @export
#' @examples
#' # z_lms_to_x()
z_lms_to_x <- function( Z, L, M, S ) { # vectorized function to convert Z + LMS parameters to a measurement
  # If do unit tests, may want something like the following:
  #
  # z_lms_to_x(        -1, 1.41379, 1388.30336, 0.2114) # check individual cases
  # z_lms_to_x(         0, 1.41379, 1388.30336, 0.2114)
  # z_lms_to_x(         1, 1.41379, 1388.30336, 0.2114)
  # z_lms_to_x( c(-1,0,1), 1.41379, 1388.30336, 0.2114) # should recycle properly
  # http://stackoverflow.com/questions/9335099/implementation-of-standard-recycling-rules
  expand.arguments <- function(...){
    # usage: expand.arguments(a = 1, b = 2, c = 1:4) --> List of 3: $a 1, 1, 1, 1; $b 2, 2, 2, 2,; $c 1, 2, 3, 4
    # does NOT enforce shorter lists being length multiples of longer vectors
    dotList <- list(...)
    max.length <- max(sapply(dotList, length))
    lapply(dotList, rep, length = max.length)
  }
  temp <- expand.arguments(Z, L, M, S) # expands any of Z, L, M, S to the maximum length of any of the other arguments
  Z = temp[[1]]
  L = temp[[2]]
  M = temp[[3]]
  S = temp[[4]]
  ifelse(
    L != 0,
      M * (1 + L*S*Z) ^ (1/L), # for L != 0
      M * exp(S*Z)             # for L == 0
    )
}


#' X to Z-score function
#'
#' Function to take vectors of measurements, age, and gender, and unique chart and measure, to return Z score
#' 
#' The following charts are available, with their corresponding measures.
#' 
#' \tabular{lll}{
#'   \strong{chart} \tab \strong{age_units} \tab \strong{measures} \cr
#'   abdel-rahman_2017     \tab months \tab arm_circ(cm)                                            \cr
#'   addo_2010_skin        \tab years  \tab subscapular(mm), triceps(mm)                            \cr
#'   addo_2017             \tab months \tab afa(cm2), ama(cm2), arm_circ(cm)                        \cr
#'   brooks_gmfcs_1        \tab years  \tab bmi(kg/m2), height(cm), weight(kg)                      \cr
#'   brooks_gmfcs_2        \tab years  \tab bmi(kg/m2), height(cm), weight(kg)                      \cr
#'   brooks_gmfcs_3        \tab years  \tab bmi(kg/m2), height(cm), weight(kg)                      \cr
#'   brooks_gmfcs_4        \tab years  \tab bmi(kg/m2), height(cm), weight(kg)                      \cr
#'   brooks_gmfcs_5_nt     \tab years  \tab bmi(kg/m2), height(cm), weight(kg)                      \cr
#'   brooks_gmfcs_5_tf     \tab years  \tab bmi(kg/m2), height(cm), weight(kg)                      \cr
#'   cappa_2024            \tab years  \tab height_velocity(cm/year), height(cm), weight(kg)        \cr
#'   cdc_2000_bmi          \tab months \tab bmi(kg/m2)                                              \cr
#'   cdc_2000_infant       \tab months \tab head_circ(cm), length(cm), weight(kg)                   \cr
#'   cdc_2000_pedi         \tab months \tab height(cm), weight(kg)                                  \cr
#'   cdc_2000_wt_for_ht    \tab cm     \tab weight(kg)                                              \cr
#'   cdc_2000_wt_for_len   \tab cm     \tab weight(kg)                                              \cr
#'   fenton_2003           \tab weeks  \tab head_circ(cm), length(cm), weight(kg)                   \cr
#'   mramba_2017           \tab months \tab arm_circ(cm)                                            \cr
#'   olsen_2010            \tab weeks  \tab head_circ(cm), length(cm), weight(g)                    \cr
#'   olsen_2015_bmi        \tab weeks  \tab bmi(g/cm2)                                              \cr
#'   who_2006_infant       \tab months \tab head_circ(cm), length(cm), weight(kg)                   \cr
#'   who_2007_skin_arm     \tab months \tab arm_circ(cm), subscapular(mm), triceps(mm)              \cr
#'   who_expanded          \tab days   \tab bmi(kg/m2), head_circ(cm), length_height(cm), weight(kg)\cr
#'   who_expanded_arm_skin \tab days   \tab arm_circ(cm), subscapular(mm), triceps(mm)              \cr
#'   who_wt_for_ht         \tab cm     \tab weight(kg)                                              \cr
#'   who_wt_for_len        \tab cm     \tab weight(kg)                                              \cr
#'   zemel_2015_infant     \tab months \tab head_circ(cm), length(cm), weight(kg)                   \cr
#'   zemel_2015_pedi       \tab years  \tab bmi(kg/m2), head_circ(cm), height(cm), weight(kg)       
#' }
#'
#' @param x Vector of measurements
#' @param age Vector of age parameters
#' @param gender Vector of genders, either 'm' or 'f'
#' @param chart Uniquely specified chart to obtain LMS parameters for
#' @param measure Uniquely specified measure for the chart. Defaults to 'weight'.
#' @export
#' @examples
#' # 3, 10, 50, 90, and 97%ile weight for 8.5 month old female on CDC 2000 infant chart
#' x_to_z(
#'     c(6.720327734, 7.197413532, 8.314178377, 9.573546299, 10.2153883),
#'     rep(8.5, 5),
#'     rep('f', 5),
#'     chart = 'cdc_2000_infant',
#'     measure = 'weight'
#'   )
#' round( pnorm( x_to_z(
#'     c(6.720327734, 7.197413532, 8.314178377, 9.573546299, 10.2153883),
#'     rep(8.5, 5),
#'     rep('f', 5),
#'     chart = 'cdc_2000_infant',
#'     measure = 'weight'
#'   ) ), 4)
x_to_z <- function(x, age, gender, chart, measure = 'weight') {
    # function to take a measurement x, and specify age, gender, chart, and measure, to return a Z score
    z <- x_lms_to_z( x, get_lms( age, gender, chart, measure ) )
    return( z )
}
