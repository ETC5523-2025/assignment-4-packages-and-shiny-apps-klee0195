#' Quarterly Household Disposable Income per Capita (Australia)
#'
#' A quarterly time series of real household disposable income per capita.
#' Data covers 1985–2024 and is adjusted to real AUD.
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{Date}{Quarterly date in YYYY-MM-DD format}
#'   \item{Year}{Calendar year}
#'   \item{Quarter}{Quarter number (1–4)}
#'   \item{Income_per_capita}{Real household disposable income per capita (AUD)}
#' }
#' @source Simulated and compiled from ABS and ATO public datasets.
"income_pc"

#' Detached House Price Index by State (Australia)
#'
#' Quarterly detached house price indices for eight Australian states and
#' an Australia-wide aggregate. Data includes simulated pre-2002 values.
#'
#' @format A data frame with 12 variables:
#' \describe{
#'   \item{Date}{Quarterly date in YYYY-MM-DD format}
#'   \item{Year}{Calendar year}
#'   \item{Quarter}{Quarter number (1–4)}
#'   \item{Dwelling_index}{Residential property price index (ABS)}
#'   \item{Sydney, Melbourne, Brisbane, Adelaide, Perth, Hobart, Darwin, Canberra}{State-level detached house indices}
#'   \item{Australia}{Weighted average of 8 capitals}
#' }
#' @source Simulated and compiled from ABS and ATO public datasets.
"detached_full"

#' Average Capital Gains (1985–2023)
#'
#' Estimated average annual capital gains for taxable individuals in Australia.
#' Values are expressed in thousands of AUD and include simulated early years.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{Year}{Financial year ending (numeric)}
#'   \item{FinYear}{Label in "YYYY–YYYY" format}
#'   \item{Total_gain_adj}{Average capital gains (AUD '000)}
#' }
#' @source Simulated and compiled from ABS and ATO public datasets.
"cgt_full"
