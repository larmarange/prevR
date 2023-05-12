#' Provide national boundaries of a country.
#'
#'
#' This function uses the data set [TMWorldBorders]. One or several countries
#' can be selected and will be returned as an object of class
#' [sp::SpatialPolygons-class].
#'
#' @param countries a vector of character string corresponding to the name of
#' the countries you want to extract from the dataset. If \code{NULL},
#' a dialogue box will be appear in order to select the desired country.
#' @param multiple should the dialog box allow multiple selection
#' (unused if \code{countries} is specified)?
#' @param proj projection of clusters coordinates used in \code{data}
#'   (longitude and latitude in decimal degrees by default). One of
#'   (i) character: a string accepted by GDAL, (ii) integer, a valid EPSG value
#'   (numeric), or (iii) an object of class `crs`, see [sf::st_crs()].
#'
#' @return Object of class [sp::SpatialPolygons-class].
#'
#' @note The result will be automatically plotted.
#'
#' @seealso [TMWorldBorders].
#'
#' @examples
#' \dontrun{
#' boundary <- create.boundary()
#' }
#' \dontshow{
#' par(ask = TRUE)
#' }
#' boundary <- create.boundary("Burkina Faso")
#' boundary <- create.boundary("Burkina Faso",
#'   proj = "+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#' )
#' boundary <- create.boundary(countries = c("Burkina Faso", "Ghana", "Benin"))
#' \dontshow{
#' par(ask = FALSE)
#' }
#'
#' @export
#' @keywords manip spatial

create.boundary <- function(
    countries = NULL,
    multiple = FALSE,
    proj = "+proj=longlat +datum=WGS84"
  ) {
  if (is.null(countries)) {
    title <- c(
      gettext("Select one country:", domain = "R-prevR"),
      gettext("Select countries (use CTRL):", domain = "R-prevR")
    )[c(!multiple, multiple)]
    countries <- select.list(
      sort(TMWorldBorders$NAME),
      multiple = multiple,
      title = title
    )
  }
  if (length(countries) == 0) {
    return(NULL)
  }
  ind <- match(countries, as.character(TMWorldBorders$NAME), nomatch = 0)
  if (any(ind == 0)) {
    invalid.names <- paste(countries[ind == 0], collapse = ", ")
    n.invalid <- length(countries[ind == 0])
    stop.mess <- sprintf(
      ngettext(
        n.invalid,
        "%s is not a valid country name.",
        "%s are not valid country names.",
        domain = "R-prevR"
      ),
      invalid.names
    )
    stop(stop.mess, call. = FALSE)
  }

  boundary <- TMWorldBorders[ind, ]
  if (!is.null(proj)) {
    boundary <- sf::st_transform(boundary, proj)
  }
  plot(boundary["NAME"], axes = TRUE)
  if (length(countries) == 1) title(main = countries)
  message(
    paste0(
      "Source: World Borders Dataset 0.3 (2008)\nProvided by Bjorn Sandvik, ",
      "http://thematicmapping.org/downloads/world_borders.php\n",
      "The dataset was derived by Schuyler Erle from public domain sources.\n",
      "Sean Gilles did some clean up and made some enhancements.\n",
      "The dataset is available under a Creative Commons Attribution-Share ",
      "Alike License.\n",
      "https://creativecommons.org/licenses/by-sa/3.0/\n",
      "The boundaries, names designations used do not imply official ",
      "endorsement or acceptance by the authors."
    ),
    domain = "R-prevR"
  )
  boundary
}
