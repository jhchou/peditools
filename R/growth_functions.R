##########################################################################################
# growth chart functions
#
# lmsData is R object of LMS parameters for a collection of charts, in R/sysdata.rda for the package
#
# To get a summary of available charts and their measures:
# lmsData %>% dplyr::select(Chart, Measure) %>% unique() %>% dplyr::group_by(Chart) %>% dplyr::summarise(Measures = paste(sort(Measure), collapse = ', '))
#
# Include age and measure units:
# lmsData %>%
#   filter(Chart != 'Fenton2013') %>%
#   mutate(Measure = paste0(Measure, '(', MeasureUnits,')')) %>%
#   select(Chart, AgeUnits, Measure) %>%
#   unique() %>%
#   group_by(Chart, AgeUnits) %>%
#   summarise(Measures = paste(sort(Measure), collapse = ', ')) %>%
#   arrange(Chart)
#
# To do:
# [ ] z_lms_to_x -- is this ever used, or just available, to perhaps make growth charts?
# [ ] DON'T export functions like get_lms, x_lms_to_z
# [ ] Probably ONLY need to export x_to_z
# [ ] Should include table of charts and their measures in help for x_to_z
# [ ] Need to rename functions
# [ ] Need unit tests


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
  # reference explaining LMS parameters: http://www.cdc.gov/growthcharts/percentile_data_files.htm

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



#' Get LMS parameters function
#'
#' Function to interpolate LMS parameters, given age, gender, chart, and measure
#' @param age Vector of ages
#' @param gender Vector of gender, either 'M' or 'F'
#' @param chart Which chart to obtain LMS parameters for. Defaults to 'Fenton 2013' premature growth chart
#' @param measure Which measure for the chart. Defaults to 'Wt'
#' @export
#' @examples
#' get_lms(c(38, 38), c('M', 'F'), chart = 'Fenton2013', measure = 'Wt')
get_lms <- function( age, gender, chart = 'Fenton2013', measure = 'Wt' ) {
  # function to interpolate LMS parameters, given vector of age, vector of gender, unique chart, and unique measure

  # MALE
  lms <- lmsData[ (lmsData$Chart == chart) & (lmsData$Measure == measure) & (lmsData$Gender) == 'M', ]
  if (nrow(lms) == 0) { return(NA) } # No chart matched
  # generate functions to interpolate LMS parameters
  lms <- lms[ order(lms$Age), ] # for safety and robustness, ensure dataframe sorted by Age
  fxn_l_male <- stats::approxfun( lms$Age, lms$L ) # stats::approxfun: linear interpolation; returns NA if out of range
  fxn_m_male <- stats::approxfun( lms$Age, lms$M )
  fxn_s_male <- stats::approxfun( lms$Age, lms$S )

  lms_male <- list(L_male = fxn_l_male(age), M_male = fxn_m_male(age), S_male = fxn_s_male(age))

  # FEMALE
  lms <- lmsData[ (lmsData$Chart == chart) & (lmsData$Measure == measure) & (lmsData$Gender) == 'F', ]
  if (nrow(lms) == 0) { return(NA) } # No chart matched
  # generate functions to interpolate LMS parameters
  lms <- lms[ order(lms$Age), ] # for safety and robustness, ensure dataframe sorted by Age
  fxn_l_female <- stats::approxfun( lms$Age, lms$L ) # stats::approxfun: linear interpolation; returns NA if out of range
  fxn_m_female <- stats::approxfun( lms$Age, lms$M )
  fxn_s_female <- stats::approxfun( lms$Age, lms$S )
  lms_female <- list(L_female = fxn_l_female(age), M_female = fxn_m_female(age), S_female = fxn_s_female(age))

  gender <- tolower(substr(gender, 1, 1))

  df <- data.frame(age, gender, lms_male, lms_female) # if gender is a vector of length 1, will be expanded to fill dataframe

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
                Chart        = as.character(lms[1, 'Chart']),
                AgeUnits     = as.character(lms[1, 'AgeUnits']),
                Measure      = as.character(lms[1, 'Measure']),
                MeasureUnits = as.character(lms[1, 'MeasureUnits'])
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


#' X to Z-score function
#'
#' Function to take vectors of measurements, age, and gender, and unique chart and measure, to return Z score
#' 
#' The following charts are available, with their corresponding measures.
#' 
#' \tabular{lll}{
#'   \strong{Chart} \tab \strong{Age} \tab \strong{Measures}\cr
#'   CDCbmi \tab months \tab BMI(kg/m2)\cr
#'   CDCinfant \tab months \tab HC(cm), Len(cm), Wt(kg)\cr
#'   CDCpedi \tab months \tab Len(cm), Wt(kg)\cr
#'   CDCskin \tab years \tab subscapular(mm), triceps(mm)\cr
#'   DownInfant \tab months \tab HC(cm), Len(cm), Wt(kg)\cr
#'   DownPedi \tab years \tab BMI(kg/m2), HC(cm), Height(cm), Wt(kg)\cr
#'   Fenton2003 \tab weeks \tab HC(cm), Len(cm), Wt(kg)\cr
#   Fenton2013 \tab weeks \tab HC(cm), Len(cm), Wt(g)\cr
#'   Olsen2010 \tab weeks \tab HC(cm), Len(cm), Wt(g)\cr
#'   OlsenBMI \tab weeks \tab BMI(g/cm2)\cr
#'   WHOinfant \tab months \tab HC(cm), Len(cm), Wt(kg)\cr
#'   WHOskin \tab months \tab arm circ(cm), subscapular(mm), triceps(mm)\cr
#' }
#' 
#' @param x Vector of measurements
#' @param age Vector of age parameters
#' @param gender Vector of genders, either 'M' or 'F'
#' @param chart Uniquely specified chart to obtain LMS parameters for. Defaults to 'Fenton 2013' premature growth chart.
#' @param measure Uniquely specified measure for the chart. Defaults to 'Wt'.
#' @export
#' @examples
#' # 3, 10, 50, 90, and 97%ile for 30 0/7 week M on Fenton2013
#' x_to_z(
#'     c(774.1259148, 986.5784793, 1388.303356, 1746.218584, 1903.278428),
#'     rep(30, 5),
#'     rep('M', 5),
#'     chart = 'Fenton2013',
#'     measure = 'Wt'
#'   )
#' round( pnorm( x_to_z(
#'     c(774.1259148, 986.5784793, 1388.303356, 1746.218584, 1903.278428),
#'     rep(30, 5),
#'     rep('M', 5),
#'     chart = 'Fenton2013',
#'     measure = 'Wt'
#'   ) ), 4)
x_to_z <- function(x, age, gender, chart = 'Fenton2013', measure = 'Wt') {
    # function to take a measurement x, and specify age, gender, chart, and measure, to return a Z score
    z <- x_lms_to_z( x, get_lms( age, gender, chart, measure ) )
    return( z )
}
