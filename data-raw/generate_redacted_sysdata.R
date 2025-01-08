# Generate sysdata for use in package
#
# - starting from source .CSV files with LMS data
# - optionally add restricted datasets (which will NOT be in repo)
# - generate chart metadata dataframe
# 
# To consider:
# - rename gender to sex
# - keep `age` column but in metadata include age_label and age_units?
# - rename output `lmsdata` to `chart_lmsdata` for consistency with `chart_metadata`

library(tidyverse)
library(usethis) # use_data()

include_restricted <- FALSE

lmsdata <- read.csv(file = 'data-raw/charts_long.csv', stringsAsFactors = FALSE)

#### Optionally include restricted files
if (include_restricted) {
  lmsdata <- lmsdata %>% 
    bind_rows(read.csv(file = 'data-raw/charts_long_fenton_2013.csv', stringsAsFactors = FALSE))
}

##### Redact problematic charts
lmsdata <- lmsdata %>% 
  filter(!((chart == "cappa_2024") & (measure == "bmi"))) # problems with published male BMI LMS parameters 2024-12-20

#### Standardize row order
lmsdata <- lmsdata %>%   arrange(chart, measure, gender, age)


##### Consider changes for LMS data
# - would be returned by get_lmsdata()
# - breaking changes, consider for future version of package?
# - rename gender to sex
# - does not include age_units, measure_units, which are now in metadata
# chart_lms <- lmsdata %>% 
#   transmute(
#     chart, measure, sex = gender, age, L, M, S
#     
#   ) %>% 
#   arrange(chart, measure, sex, age)

##### Generate chart metadata
# - not best practice, but want to include all metadata, even if chart not available
# - otherwise, would need to separately store metadata information elsewhere, with source LMS data
# - kludgy, but could filter out any chart metadata that is NOT available after get_lmsdata() and add_lmsdata()
lmsdata2 <- lmsdata %>% 
  bind_rows(read.csv(file = 'data-raw/charts_long_fenton_2013.csv', stringsAsFactors = FALSE)) %>% 
  unique()

chart_metadata <- lmsdata2 %>% 
  group_by(chart, measure) %>%
  mutate(
    age_min = min(age),
    age_max = max(age),
    num_param = n()
  ) %>%
  ungroup() %>%
  transmute(
    chart,
    measure,
    measure_units,
    age_label = ifelse(chart == "who_wt_for_ht", "height", ifelse(chart == "who_wt_for_len", "length", "age")),
    age_units,
    age_min,
    age_max,
    age_range = paste0(round(age_min, 2), '-', round(age_max, 2)),
    num_param
  ) %>% 
  unique() %>% 
  arrange(chart, measure) %>% 
  mutate(
    # - who_2007_skin_arm overlays who_standard expanded
    source = case_when(
      chart == 'abdel-rahman_2017'   ~ '<a href = "http://www.ncbi.nlm.nih.gov/pubmed/27856693" target="_blank">"Construction of Lambda, Mu, Sigma Values for Determining Mid-Upper Arm Circumference z Scores in U.S. Children Aged 2 Months Through 18 Years." Abdel-Rahman et al, Nutr Clin Practice 2017; 32:68-76</a>',
      chart == 'addo_2010_skin'      ~ '<a href = "http://www.ncbi.nlm.nih.gov/pubmed/20053877" target="_blank">"Reference curves for triceps and subscapular skinfold thicknesses in US children and adolescents." Addo OY and Himes JH, Am J Clin Nutr 2010; 91:635-42</a>',
      chart == 'cappa_2024'          ~ '<a href = "http://www.ncbi.nlm.nih.gov/pubmed/38964306" target="_blank">"Noonan Syndrome Growth Charts and Genotypes: 15-Year Longitudinal Single-Centre Study." Cappa M et al, Horm Red Paediatr 2024, 22:1. (BMI omitted due to parameter publication error.)</a>',
      chart == 'cdc_2000_bmi'        ~ '<a href = "http://www.cdc.gov/growthcharts/percentile_data_files.htm" target="_blank">CDC Growth Charts - Data Tables</a> and <a href = "https://www.cdc.gov/growthcharts/extended-bmi.htm" target="_blank">CDC 2022 Extended BMI-for-age</a>',
      chart == 'cdc_2000_infant'     ~ '<a href = "http://www.cdc.gov/growthcharts/percentile_data_files.htm" target="_blank">CDC Growth Charts - Data Tables</a>',
      chart == 'cdc_2000_pedi'       ~ '<a href = "http://www.cdc.gov/growthcharts/percentile_data_files.htm" target="_blank">CDC Growth Charts - Data Tables</a>',
      chart == 'fenton_2003'         ~ '<a href = "http://www.ncbi.nlm.nih.gov/pubmed/17299469" target="_blank">"Using the LMS method to calculate z-scores for the Fenton preterm infant growth chart." Fenton TR and Sauve RS, Eur J Clin Nutrition 2007, 61:1380</a>',
      chart == 'fenton_2013'         ~ '<a href = "http://www.ncbi.nlm.nih.gov/pubmed/23601190" target="_blank">"A systematic review and meta-analysis to revise the Fenton growth chart for preterm infants." Fenton TR and Kim JH, BMC Pediatrics 2013, 13:59</a> <a href = "http://www.biomedcentral.com/1471-2431/13/59" target="_blank">(BioMed Central)</a>',
      chart == 'mramba_2017'         ~ '<a href = "http://www.ncbi.nlm.nih.gov/pubmed/28774873" target="_blank">"A growth reference for mid upper arm circumference for age among school age children and adolescents, and validation for mortality: growth curve construction and longitudinal cohort study." Mramba et al, BMJ 2017; 358:j3423</a>',
      chart == 'olsen_2010'          ~ '<a href = "http://www.ncbi.nlm.nih.gov/pubmed/20100760" target="_blank">"New intrauterine growth curves based on United States data." Olsen IE et al, Pediatrics 2010, 125:e214</a>',
      chart == 'olsen_2015_bmi'      ~ '<a href = "http://www.ncbi.nlm.nih.gov/pubmed/25687149" target="_blank">"BMI curves for preterm infants." Olsen IE et al, Pediatrics 2015, 135:e572</a>',
      chart == 'who_2006_infant'     ~ '<a href = "http://www.cdc.gov/growthcharts/who_charts.htm" target="_blank">WHO Growth Standard - Data Tables</a>',
      chart == 'zemel_2015_infant'   ~ '<a href = "http://www.ncbi.nlm.nih.gov/pubmed/26504127/" target="_blank">"Growth Charts for Children With Down Syndrome in the United States." Zemel BS et al, Pediatrics 2015, 136:e1204</a>',
      chart == 'zemel_2015_pedi'     ~ '<a href = "http://www.ncbi.nlm.nih.gov/pubmed/26504127/" target="_blank">"Growth Charts for Children With Down Syndrome in the United States." Zemel BS et al, Pediatrics 2015, 136:e1204</a>',
      grepl('brooks_gmfcs', chart)   ~ '<a href = "https://pubmed.ncbi.nlm.nih.gov/21768315/" target="_blank">"Low weight, morbidity, and mortality in children with cerebral palsy: new clinical growth charts." Brooks J et al, Pediatrics 2011, 128:e299</a> <a href = "https://www.lifeexpectancy.org/articles/GrowthCharts.shtml" target="_blank">(additional info)</a>',
      grepl('who_wt_for_ht', chart)  ~ '<a href = "https://www.who.int/tools/child-growth-standards" target="_blank">WHO Child Growth Standards. Weight for height, so "age" should be "height"</a>',
      grepl('who_wt_for_len', chart) ~ '<a href = "https://www.who.int/tools/child-growth-standards" target="_blank">WHO Child Growth Standards. Weight for length, so "age" should be "length"</a>',
      # who_2007_skin_arm reference http://www.who.int/childgrowth/standards/second_set/technical_report_2.pdf, broken
      # new link: https://www.who.int/publications/i/item/9789241547185
      chart == 'who_2007_skin_arm'   ~ '<a href = "https://www.who.int/publications/i/item/9789241547185" target="_blank">WHO Child Growth Standards -- Head circumference-for-age, arm circumference-for-age, triceps skinfold-for-age and subscapular skinfold-for-age: Methods and development</a>',
      grepl('who_', chart)           ~ '<a href = "https://www.who.int/tools/child-growth-standards" target="_blank">WHO Child Growth Standards</a>',
      TRUE ~ 'source information not available'
    )
  ) %>% 
  mutate(
    text = str_extract(source, '(?<=\\>)[^<]+'),     # Extract the text inside the tag
    url = str_extract(source, '(?<=href = ")[^"]+'), # Extract the URL
    notes = "" # placeholder for any future needs
  ) %>% 
  select(-source)


##### Save dataframe(s) into R/sys.data in proper format for package
usethis::use_data(lmsdata, chart_metadata, internal = TRUE, overwrite = TRUE)                   #



# Display table of available charts / measures
# lmsdata %>%
#   mutate(
#     measure = paste0(measure, ' (', measure_units,')'),
#     gender = ifelse(chart == 'fenton_2003', 'm', gender)
#   ) %>%
#   # select(chart, measure, gender) %>% unique() %>% nrow # count number of curves
#   unique() %>%
#   group_by(chart, age_units, measure) %>%
#   summarise(
#     n = n(),
#     .groups = "drop_last"
#   ) %>% 
#   summarise(
#     measures = paste(sort(measure), collapse = ', '),
#     num_param = sum(n),
#     .groups = "drop"
#   ) %>% 
#   arrange(chart) %>% 
#   transmute(chart, measures, horiz_axis = age_units, num_param) %>% 
#   # knitr::kable()
#   View
# 
# Display table of charts / measures with age ranges
# lmsdata %>%
#   group_by(chart, age_units) %>%
#   summarise(
#     min = min(age) %>% round(),
#     max = max(age) %>% round(),
#     measures = paste(unique(measure), collapse=', '),
#     .groups = "drop"
#   ) %>%
#   transmute(
#     chart = chart,
#     range = paste0(min, '-', max),
#     age_units,
#     measures
#   ) %>%
#   View

