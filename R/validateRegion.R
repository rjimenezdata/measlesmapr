#' Validates a region
#'
#' @param region Region to be checked for.
#' @param data Data frame which may include the region.
#'
#' @returns Invisible TRUE if test is passed.
#' @export
#'

validateRegion <- function(region, data){

 # check if all countries in vector of region names are valid

  if (!all(region %in% data$Region)) {

    stop("Please enter valid region name(s)")

  }

  else {

    invisible(TRUE)

  }

}
