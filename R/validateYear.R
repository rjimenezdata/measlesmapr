#' Validates if a Year or Pair of Years Can Be Used (Found in Data)
#'
#' @param year1 Year to be tested. If in a pair, this in the minimum year to be tested.
#'
#' @param year2 Max year to be tested.
#'
#' @returns Invisible TRUE if all tests are passed.
#' @export
#'
#' @examples
#' validateYear(2015)

validateYear <- function(year1, year2 = NULL){

  # check if pair
  if (!is.null(year2)) {

      # check year1 data type
      if (!is.numeric(year1) ||
          length(year1) != 1 ||
          year1 %% 1 != 0) {
      stop("min_year must be a single integer year")}

      # check year2 data type
      if (!is.numeric(year2) ||
        length(year2) != 1 ||
        year2 %% 1 != 0) {
      stop("max_year must be a single integer year")}

      # check if chronological order
      if (year1 > year2) {
        stop("min_year must be less than or equal to max_year")}

      # set warning if min_year is same as max_year
      if (year1 == year2) {
        warning("min_year is equal to max_year resulting in at most one data point.")}

      # check if min_year out of range
      if (year1 < 2012 || year1 > 2025) {
        stop("min_year out of range")}

      # check if max_year out of range
      if (year2 < 2012 || year2 > 2025) {
        stop("max_year out of range")}}

  # single value (not pair)
  else{
      # check year1 data type
      if (!is.numeric(year1) ||
          length(year1) != 1 ||
          year1 %% 1 != 0) {
        stop("year must be a single integer year")}

      # check if out of range
      if (year1 < 2012 || year1 > 2025) {
        stop("Year out of range")}}

  # if all checks pass, return invisible TRUE
  invisible(TRUE)}
