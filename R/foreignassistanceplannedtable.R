#' Foreign Assistance Planned Data Function
#'
#' This function allows users to query the data based on variables they are interested in to create a table using foreignassistance.gov's planned dataset.
#' @param years Fiscal years included. Defaults to 'all'
#' @param appropriation_type Request, appropriation, or actual appropriaion. Defaults to 'all'. To select one type, put your selection in quotation marks (eg "request"). To select multiple, use c() to create a list (eg c("request", "appropriation", "appropriated_actual"))
#' @param sectors_included Do you want to include the sectors variable? Defaults to FALSE. To see sectors variable, replace with TRUE.
#' @param sectors Which sectors do you want to select? Defaults to 'all'. Possible selections include Family Planning and Reproductive Health, Maternal and Child Health, Other Public Health Threats, Tuberculosis, Water Supply and Sanitation, HIV/AIDS, Malaria, Pandemic Influenza and Other Emerging Threats (PIOET), Nutrition, and Health - General. See appropriation_type for instructions on how to select multiple values.
#' @param agencies_included Do you want to include the agencies variable? Defaults to FALSE. To see agencies variable, replace with TRUE.
#' @param agencies Which agencies do you want to select? Defaults to 'all'. Possible selections include U.S. Department of State and U.S. Agency for International Development, U.S. Department of Health and Human Services, U.S. Department of Defense, and U.S. Department of Agriculture. See appropriation_type for instructions on how to select multiple values.
#' @param accounts_included Do you want to include the accounts variable? Defaults to FALSE. To see accounts variable, replace with TRUE.
#' @param accounts Which accounts do you want to select? Defaults to 'all'. See appropriation_type for instructions on how to select multiple values. Note: Not all observations include an account value.
#' @param locations_included Do you want to include recipient location variable? Defaults to FALSE. To see locations variable, replace with TRUE.
#' @param locations Which locations do you want to select? Defaults to 'all'. Country names are based off U.S. naming conventions (eg Burma, not Myanmar). Locations includes country names, regions, and worldwide for global funding.
#' @param location_types_included Do you want to include location type variable? Location type classifies locations as a country, region, or global program. Defaults to FALSE. To see location type variable, replace with TRUE.
#' @param location_types Which location types do you want to select? Defaults to 'all'. Possible selections include Country, Region, or Worldwide. See appropriation_type for instructions on how to select multiple values.
#' @param regions_included Do you want to include the regions variable? This variable classifies countries into either USAID or WHO regions. Defaults to FALSE. To see regions variable, replace with TRUE.
#' @param region_classifications Which regional classification system do you want to use? Possible selections included USAID or WHO. Does not allow for both classifications to be used simultaneously. Defaults to USAID.
#' @param regions Which regions do you want to select? Defaults to 'all'. Possible selections vary based on USAID or WHO selection. See appropriation_type for instructions on how to select multiple values. Note: Not all countries have a regional classification, based on WHO and USAID coding
#' @param incomes_included Do you want to include the incomes variable? This variable classifies countries' income level using World Bank data. Defaults to FALSE. To see incomes variable, replace with TRUE.
#' @param incomes Which income levels do you want to select? Defaults to 'all'. Possible values include Low-income, Lower-middle income, Upper-middle income, and High-income.
#' @param group_by How do you want to group the data? This parameter is very important to remember if you want to group the data by certain variables. Selecting the variable will only include it in the table view, but will not necessarily group by that variable. Table is automatically grouped by fiscal year and appropriation type.
#'
#'
#' @keywords foreignassistance
#'
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import readr
#' @import janitor


foreign_assistance_planned_table <- function(years = 'all',
                                             appropriation_type = 'all',
                                             sectors_included = FALSE,
                                             sectors = 'all',
                                             agencies_included = FALSE,
                                             agencies = 'all',
                                             accounts_included = FALSE,
                                             accounts = 'all',
                                             locations_included = FALSE,
                                             locations = 'all',
                                             location_types_included = FALSE,
                                             location_types = 'all',
                                             regions_included = FALSE,
                                             region_classifications = 'USAID',
                                             regions = 'all',
                                             incomes_included = FALSE,
                                             incomes = 'all',
                                             group_by = 'year') ##possible inputs: year, appropriation_type, sectors, agencies, accounts, locations, location_types, regions, incomes
{

  ##LOAD DATA ----------------------------------------------

  budget_data <- read.csv("https://www.foreignassistance.gov/downloads/BudgetData/Budget Planning Data - All Reporting Agencies.csv") %>%
    clean_names() %>%
    filter(category == "Health") %>%
    pivot_longer(cols = c(request, appropriation, appropriated_actual), names_to = "appropriation_phase", values_to = "value") %>%
    mutate(value = ifelse(is.na(value), 0, value))

  ##ADDING MISSING VARIABLES --------------------------------------------

  ##Location type
  budget_data_regions <- sort(unique(budget_data$location))[which(str_detect(sort(unique(budget_data$location)), "Asia|America|Europe|Oceania"))]

  budget_data <- budget_data %>%
    mutate(location_type = ifelse(location %in% budget_data_regions, "Region",
                                  ifelse(location == "Worldwide", "Worldwide",
                                         "Country")))

  rm(budget_data_regions)

  ##Region classification

  ##WHO -- Data retrieved from: https://www.who.int/countries

  who_eastern_mediterranean_region <- c("Afghanistan", "Bahrain", "Djibouti", "Egypt", "Iran", "Iraq", "Jordan", "Kuwait", "Lebanon", "Libya", "Morocco", "Oman", "Pakistan", "Qatar", "Saudi Arabia", "Somalia", "Sudan", "Syria", "Tunisia", "United Arab Emirates", "Yemen")

  who_african_region <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo (Brazzaville)", "Cote d'Ivoire", "Congo (Kinshasa)", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "South Africa", "South Sudan", "Togo", "Uganda", "Tanzania", "Zambia", "Zimbabwe")

  who_americas_region <- c("Antigua and Barbuda", "Argentina", "Bahamas", "Barbados", "Belize", "Bolivia", "Brazil", "Canada", "Chile", "Colombia", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "Ecuador", "El Salvador", "Grenada", "Guatemala", "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Trinidad and Tobago", "Uruguay", "Venezuela")

  who_south_east_asia_region <- c("Bangladesh", "Bhutan", "North Korea", "India", "Indonesia", "Maldives", "Burma", "Nepal", "Sri Lanka", "Thailand", "Timor-Leste")

  who_western_pacific_region <- c("Australia", "Brunei Darussalam", "Cambodia", "China", "Cook Islands", "Fiji", "Japan", "Kiribati", "Laos", "Malaysia", "Marshall Islands", "Micronesia, Federated States of", "Mongolia", "Nauru", "New Zealand", "Niue", "Palau", "Papua New Guinea", "Philippines", "South Korea", "Samoa", "Singapore", "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu", "Vietnam")

  who_european_region <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", 'Estonia', "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", 'Kazakhstan', "Kyrgyzstan", "Latvia", "Lithuania", "Luxembourg", "Malta", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Moldova", 'Romania', "Russia", 'San Marino', "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Tajikistan", "Turkey", "Turkmenistan", "Ukraine", "United Kingdom", "Uzbekistan")


  ##USAID -- Data retrieved from: https://www.usaid.gov/where-we-work

  usaid_africa_region <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Cote d'Ivoire", "Congo (Kinshasa)", "Congo (Brazzaville)", "Djibouti", "Eswatini", "Ethiopia", 'Ghana', "Guinea", "Kenya", 'Lesotho', "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Gambia", "Uganda", 'Zambia', "Zimbabwe")

  usaid_asia_region <- c("Afghanistan", "Bangladesh", "Burma", "Cambodia", "China", "India", "Indonesia", "Kazakhstan", "Kyrgyzstan", "Laos", "Maldives", "Mongolia", "Nepal", "Pacific Islands", "Pakistan", "Philippines", "Sri Lanka", "Tajikistan", "Thailand", "Timor-Leste", "Turkmenistan", "Uzbekistan", "Vietnam")

  usaid_europe_and_eurasia_region <- c("Albania", "Armenia", "Azerbaijan", "Belarus", "Bosnia and Herzegovina", "Cyprus", "Georgia", "Kosovo", "Moldova", "Montenegro", "North Macedonia", "Russia", "Serbia", "Ukraine")

  usaid_latin_america_and_the_caribbean_region <- c("Bolivia", "Brazil", "Colombia", "Cuba", "Dominican Republic", "Ecuador", "El Salvador", 'Guatemala', "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Venezuela")

  usaid_middle_east_region <- c("Egypt", 'Iraq', "Jordan", "Lebanon", "Libya", "Morocco", "Syria", "Tunisia", "West Bank and Gaza", "Yemen")

  ##Income -- Data retrieved from: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519

  wb_low_income <- c("Afghanistan", "Burkina Faso", "Burundi", "Central African Republic", "Chad", "Congo (Kinshasa)", "Eritrea", "Ethiopia", "Gambia", "Guinea", "Guinea-Bissau", "Haiti", "North Korea", "Liberia", "Madagascar", "Malawi", "Mali", 'Mozambique', "Niger", "Rwanda", "Sierra Leone", "Somalia", "South Sudan", "Sudan", "Syria", "Tajikistan", "Togo", "Uganda", "Yemen")

  wb_lower_middle_income <- c("Angola", "Algeria", "Bangladesh", 'Benin', 'Bhutan', "Bolivia", "Burma", "Cabo Verde", "Cambodia", "Cameroon", "Congo (Brazzaville)", "Comoros", "Cote d'Ivoire", 'Djibouti', 'Egypt', "El Salvador", "Eswatini", "Ghana", "Honduras", "India", "Kenya", "Kiribati", "Kyrgyzstan", "Laos", "Lesotho", "Mauritania", "Micronesia, Federated States of", "Moldova", "Mongolia", "Morocco", "Nepal", "Nicaragua", "Nigeria", "Pakistan", "Papua New Guinea", "Philippines", "Sao Tome and Principe", "Senegal", "Solomon Islands", "Sri Lanka", "Tanzania", "Timor-Leste", "Tunisia", "Ukraine", "Uzbekistan", "Vanuatu", "Vietnam", "West Bank and Gaza", "Zambia", "Zimbabwe")

  wb_upper_middle_income <- c("Albania", "Argentina", "Armenia", "Azerbaijan", "Belarus", "Belize", "Bosnia and Herzegovina", "Botswana", "Brazil", "Bulgaria", "China", "Colombia", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "Equatorial Guinea", "Ecuador", "Fiji", "Gabon", "Georgia", "Grenada", 'Guatemala', "Guyana", "Indonesia", "Iran", "Iraq", "Jamaica", "Jordan", "Kazakhstan", "Kosovo", "Lebanon", "Libya", "Malaysia", "Maldives", "Marshall Islands", "Mexico", "Montenegro", "Namibia", "North Macedonia", "Paraguay", "Peru", "Russia", "Samoa", "Serbia", "South Africa", "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Thailand", "Tonga", "Turkey", "Turkmenistan", "Tuvalu", "Venezuela")

  wb_high_income <- c("Andorra", "Antigua and Barbuda", "Aruba", "Australia", "Austria", "Bahamas", "Bahrain", "Barbados", "Belgium", "Bermuda", "British Virgin Islands", "Brunei Darussalam", "Canada", "Cayman Islands", "Channel Islands", "Chile", "Croatia", "Curaçao", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Faroe Islands", "Finland", "France", "French Polynesia", "Germany", "Gibraltar", "Greece", "Greenland", "Guam", "Hong Kong SAR, China", "Hungary", "Iceland", "Ireland", "Isle of Man", "Israel", "Italy", "Japan", "South Korea", "Kuwait", "Latvia", "Liechtenstein", "Luxembourg", "Macao SAR, China", "Malta", "Mauritius", "Monaco", "Nauru", "Netherlands", "New Caledonia", "New Zealand", "Northern Mariana Islands", "Norway", "Oman", "Palau", "Panama", "Poland", "Portugal", "Romania", "Qatar", "San Marino", "Saudi Arabia", "Seychelles", "Singapore", "Slovakia", "Slovenia", "Spain", "Saint Kitts and Nevis", "Sweden", "Switzerland", "Taiwan, China", "Trinidad and Tobago", "Turks and Caicos Islands", "United Arab Emirates", "Uruguay")


  ##Adding to budget_data df

  budget_data <- budget_data %>%
    mutate(location = ifelse(str_detect(location, "Ivoire"), "Cote d'Ivoire",
                             ifelse(str_detect(location, "Bahamas"), "Bahamas", location))) %>%
    mutate(who_region = ifelse(location %in% who_eastern_mediterranean_region, "WHO Eastern Mediterranean Region",
                               ifelse(location %in% who_african_region, "WHO African Region",
                                      ifelse(location %in% who_americas_region, "WHO Region of the Americas",
                                             ifelse(location %in% who_south_east_asia_region, "WHO South-East Asia Region",
                                                    ifelse(location %in% who_western_pacific_region, "WHO Western Pacific Region",
                                                           ifelse(location %in% who_european_region, "WHO European Region", NA))))))) %>% ##Only country without a WHO region: Kosovo and West Bank and Gaza
    mutate(usaid_region = ifelse(location %in% usaid_africa_region, "USAID Africa Region",
                                 ifelse(location %in% usaid_asia_region, "USAID Asia Region",
                                        ifelse(location %in% usaid_europe_and_eurasia_region, "USAID Europe and Eurasia Region",
                                               ifelse(location %in% usaid_latin_america_and_the_caribbean_region, "USAID Latin America and Caribbean Region",
                                                      ifelse(location %in% usaid_middle_east_region, "USAID Middle East Region", NA)))))) %>%
    mutate(income = ifelse(location %in% wb_low_income, "Low-income",
                           ifelse(location %in% wb_lower_middle_income, "Lower-middle income",
                                  ifelse(location %in% wb_upper_middle_income, "Upper-middle income",
                                         ifelse(location %in% wb_high_income, "High-income", NA)))))

  rm(who_african_region, who_americas_region, who_eastern_mediterranean_region, who_european_region, who_south_east_asia_region, who_western_pacific_region, usaid_africa_region, usaid_asia_region, usaid_europe_and_eurasia_region, usaid_latin_america_and_the_caribbean_region, usaid_middle_east_region, wb_high_income, wb_low_income, wb_lower_middle_income, wb_upper_middle_income)


  ##FILTERS -------------------------------------------------------------

  ##year
  suppressWarnings(if(years != 'all') {
    budget_data <- budget_data %>%
      filter(i_fiscal_year %in% years)
  })

  ##appropriation_type
  suppressWarnings(if(appropriation_type != 'all') {
    budget_data <- budget_data %>%
      filter(appropriation_phase %in% appropriation_type)
  })

  ##sector
  suppressWarnings(if(sectors != 'all') {
    budget_data <- budget_data %>%
      filter(sector %in% sectors)
  })

  ##agency
  suppressWarnings(if(agencies != 'all') {
    budget_data <- budget_data %>%
      filter(agency %in% agencies)
  })

  ##account
  suppressWarnings(if(accounts != 'all') {
    budget_data <- budget_data %>%
      filter(account %in% accounts)
  })

  ##location
  suppressWarnings(if(locations != 'all') {
    budget_data <- budget_data %>%
      filter(location %in% locations)
  })

  ##location_types
  suppressWarnings(if(location_types != 'all') {
    budget_data <- budget_data %>%
      filter(location_type %in% location_types)
  })

  ##regions
  suppressWarnings(if(regions != 'all' & region_classifications == "WHO") {
    budget_data <- budget_data %>%
      filter(who_region %in% regions)
  } else {
    if(regions != 'all' & region_classifications == "USAID") {
      budget_data <- budget_data %>%
        filter(usaid_region %in% regions)
    }
  })

  ##incomes
  suppressWarnings(if(incomes != 'all') {
    budget_data <- budget_data %>%
      filter(income %in% incomes)
  })

  ##SELECTED COLUMNS ---------------------------------------------------

  selected_columns <- c('i_fiscal_year', 'appropriation_phase', 'value')

  if(sectors_included == TRUE) {
    selected_columns <- c(selected_columns, 'sector')
  }
  if(agencies_included == TRUE) {
    selected_columns <- c(selected_columns, 'agency')
  }
  if(accounts_included == TRUE) {
    selected_columns <- c(selected_columns, 'account')
  }
  if(locations_included == TRUE) {
    selected_columns <- c(selected_columns, 'location')
  }
  if(location_types_included == TRUE) {
    selected_columns <- c(selected_columns, 'location_type')
  }
  if(regions_included == TRUE & region_classifications == "USAID") {
    selected_columns <- c(selected_columns, 'usaid_region')
  }
  if(regions_included == TRUE & region_classifications == "WHO") {
    selected_columns <- c(selected_columns, 'who_region')
  }
  if(incomes_included == TRUE) {
    selected_columns <- c(selected_columns, 'income')
  }

  budget_data <- budget_data %>%
    select(selected_columns)

  ##GROUPING VARIABLES ----------------------------------------------

  '%!in%' <- function(x,y)!('%in%'(x,y))

  budget_data <- budget_data %>%
    group_by(i_fiscal_year, appropriation_phase)

  if('sectors' %!in% group_by) {
    stop } else {
      budget_data <- budget_data %>%
        group_by(sector, .add = TRUE)
    }
  if('agencies' %!in% group_by) {
    stop } else {
      budget_data <- budget_data %>%
        group_by(agency, .add = TRUE)
    }
  if('accounts' %!in% group_by) {
    stop } else {
      budget_data <- budget_data %>%
        group_by(account, .add = TRUE)
    }
  if('locations' %!in% group_by) {
    stop } else {
      budget_data <- budget_data %>%
        group_by(location, .add = TRUE)
    }
  if('location_types' %!in% group_by) {
    stop } else {
      budget_data <- budget_data %>%
        group_by(location_type, .add = TRUE)
    }
  if('regions' %in% group_by & region_classifications == 'USAID') {
    budget_data <- budget_data %>%
      group_by(usaid_region, .add = TRUE)
  }
  if('regions' %in% group_by & region_classifications == "WHO") {
    budget_data <- budget_data %>%
      group_by(who_region, .add = TRUE)
  }
  if('incomes' %!in% group_by) {
    stop } else {
      budget_data <- budget_data %>%
        group_by(income, .add = TRUE)
    }


  ##CREATE TABLE ---------------------------------------------

  table <- budget_data %>%
    mutate(value = sum(value, na.rm = T)) %>%
    unique() %>%
    pivot_wider(names_from = i_fiscal_year, values_from = value)

}
