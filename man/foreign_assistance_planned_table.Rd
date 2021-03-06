% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/foreignassistanceplannedtable.R
\name{foreign_assistance_planned_table}
\alias{foreign_assistance_planned_table}
\title{Foreign Assistance Planned Data Function}
\usage{
foreign_assistance_planned_table(
  years = "all",
  appropriation_type = "all",
  sectors_included = FALSE,
  sectors = "all",
  agencies_included = FALSE,
  agencies = "all",
  accounts_included = FALSE,
  accounts = "all",
  locations_included = FALSE,
  locations = "all",
  location_types_included = FALSE,
  location_types = "all",
  regions_included = FALSE,
  region_classifications = "USAID",
  regions = "all",
  incomes_included = FALSE,
  incomes = "all",
  group_by = "year"
)
}
\arguments{
\item{years}{Fiscal years included. Defaults to 'all'}

\item{appropriation_type}{Request, appropriation, or actual appropriaion. Defaults to 'all'. To select one type, put your selection in quotation marks (eg "request"). To select multiple, use c() to create a list (eg c("request", "appropriation", "appropriated_actual"))}

\item{sectors_included}{Do you want to include the sectors variable? Defaults to FALSE. To see sectors variable, replace with TRUE.}

\item{sectors}{Which sectors do you want to select? Defaults to 'all'. Possible selections include Family Planning and Reproductive Health, Maternal and Child Health, Other Public Health Threats, Tuberculosis, Water Supply and Sanitation, HIV/AIDS, Malaria, Pandemic Influenza and Other Emerging Threats (PIOET), Nutrition, and Health - General. See appropriation_type for instructions on how to select multiple values.}

\item{agencies_included}{Do you want to include the agencies variable? Defaults to FALSE. To see agencies variable, replace with TRUE.}

\item{agencies}{Which agencies do you want to select? Defaults to 'all'. Possible selections include U.S. Department of State and U.S. Agency for International Development, U.S. Department of Health and Human Services, U.S. Department of Defense, and U.S. Department of Agriculture. See appropriation_type for instructions on how to select multiple values.}

\item{accounts_included}{Do you want to include the accounts variable? Defaults to FALSE. To see accounts variable, replace with TRUE.}

\item{accounts}{Which accounts do you want to select? Defaults to 'all'. See appropriation_type for instructions on how to select multiple values. Note: Not all observations include an account value.}

\item{locations_included}{Do you want to include recipient location variable? Defaults to FALSE. To see locations variable, replace with TRUE.}

\item{locations}{Which locations do you want to select? Defaults to 'all'. Country names are based off U.S. naming conventions (eg Burma, not Myanmar). Locations includes country names, regions, and worldwide for global funding.}

\item{location_types_included}{Do you want to include location type variable? Location type classifies locations as a country, region, or global program. Defaults to FALSE. To see location type variable, replace with TRUE.}

\item{location_types}{Which location types do you want to select? Defaults to 'all'. Possible selections include Country, Region, or Worldwide. See appropriation_type for instructions on how to select multiple values.}

\item{regions_included}{Do you want to include the regions variable? This variable classifies countries into either USAID or WHO regions. Defaults to FALSE. To see regions variable, replace with TRUE.}

\item{region_classifications}{Which regional classification system do you want to use? Possible selections included USAID or WHO. Does not allow for both classifications to be used simultaneously. Defaults to USAID.}

\item{regions}{Which regions do you want to select? Defaults to 'all'. Possible selections vary based on USAID or WHO selection. See appropriation_type for instructions on how to select multiple values. Note: Not all countries have a regional classification, based on WHO and USAID coding}

\item{incomes_included}{Do you want to include the incomes variable? This variable classifies countries' income level using World Bank data. Defaults to FALSE. To see incomes variable, replace with TRUE.}

\item{incomes}{Which income levels do you want to select? Defaults to 'all'. Possible values include Low-income, Lower-middle income, Upper-middle income, and High-income.}

\item{group_by}{How do you want to group the data? This parameter is very important to remember if you want to group the data by certain variables. Selecting the variable will only include it in the table view, but will not necessarily group by that variable. Table is automatically grouped by fiscal year and appropriation type.}
}
\description{
This function allows users to query the data based on variables they are interested in to create a table using foreignassistance.gov's planned dataset.
}
\examples{

}
\keyword{foreignassistance}
