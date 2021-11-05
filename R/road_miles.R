#' Calculate total road miles in a county
#'
#' @param county_geoid 5 character county GEOID
#' @param year Year of the data to download
#' @param output_unit Units for the output
#'
#' @return A numeric vector representing total roadmiles in the county
#' @export
#'
#' @importFrom tigris roads
#' @importFrom sf st_length
#'
road_miles <- function(county_geoid, year = 2019, output_unit = c("miles", "km", "meters")){

  output_unit <- match.arg(output_unit, c("miles", "km", "meters"))

  conversion <- switch (output_unit,
                        miles = 1609.34,
                        km = 1000,
                        meters = 1
  )

  if (any(nchar(county_geoid) != 5)){
    stop("`road_miles` requires valid county IDs. One or more provided IDs were not 5 characters long.")
  }

  state <- substr(unlist(county_geoid), 1, 2)
  cty   <- substr(unlist(county_geoid), 3, 5)

  rds <- vroads(state, cty, year = year)

  road_length(rds, conversion)
}

vroads <- Vectorize(tigris::roads, vectorize.args = c('state', 'county'), SIMPLIFY = FALSE)

road_length <- function(sf, conversion){
  sapply(sf, function(x) sum(sf::st_length(x)) / conversion)
}
