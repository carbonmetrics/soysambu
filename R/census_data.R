#' Animal census data for Soysambu Wildlife Conservancy.
#'
#' There were 50 census exercises done over the period 1990-02-01 to 2018-05-01.
#' These are stored in individual tabs in the spreadsheet
#' ./inst/ext/censusdata.xlsx. The original name of this file was 201805_SoyWildlifeCensusAccumulated  (version 1).xlsx.
#' The R code to read and clean the data can be found in ./data-raw/census_read_clean.
#'
#' Each tab was read and processed. Only total counts per animal per census were kept.
#'
#' @name: census_data
#' @format A data.table with 2420 rows and 3 variables:
#' \describe{
#'     \item{CensusDate}{date of census: the month-date format of the tab was used. The date is therefore not the exact date of the census.}
#'     \item{Species}{Name of the species. This has been cleaned: e.g. Species=="Giraffe"
#'     was set to "Rothschild Giraffe given the fact that no other giraffe species were in the conservancy
#'     at the time of any of the censuses carried out.}
#'     \item{Count}{Total count for the animals, this is the sum of the census counts of the individual blocks.}
#' }
#' @source Soysambu Wildlife Conservancy Field Studies and Community Office, ./data-raw/census_read_clean.R.
#' @keywords census
"census_data"
