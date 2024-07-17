#' @title Get Latitude and Longitude for a List of Locations
#' @description This function retrieves the latitude and longitude for a list of locations
#' using the Nominatim API.
#' @param locations A character vector of locations.
#' @return A data frame with columns `location`, `latitude`, and `longitude`.
#' @details This function queries the Nominatim API for each location provided in the input vector.
#' It includes error handling to manage failed API requests and respects rate limits by adding a delay
#' between requests.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  locations <- c("New York", "Los Angeles", "Chicago")
#'  lat_long_df <- get_lat_long(locations)
#'  print(lat_long_df)
#'  }
#' }
#' @seealso
#'  \code{\link[httr]{GET}}, \code{\link[httr]{status_code}}, \code{\link[httr]{content}}
#' @rdname get_lat_long
#' @export
#' @importFrom httr GET status_code content
#' @importFrom dplyr bind_rows
#' @importFrom jsonlite fromJSON
get_lat_long <- function(locations) {
  # Helper function to get latitude and longitude using Nominatim API
  get_lat_long_single <- function(location) {
    url <- paste0("https://nominatim.openstreetmap.org/search?q=", URLencode(location), "&format=json&limit=1")
    response <- tryCatch(
      {
        httr::GET(url, user_agent("RUtilHub/1.0"))
      },
      error = function(e) {
        message("Error: ", e)
        return(NULL)
      }
    )

    if (is.null(response) || httr::status_code(response) != 200) {
      return(data.frame(location = toupper(location), latitude = NA, longitude = NA))
    }

    data <- fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)

    if (length(data) == 0) {
      return(data.frame(location = toupper(location), latitude = NA, longitude = NA))
    }

    return(data.frame(location = toupper(location), latitude = as.numeric(data$lat), longitude = as.numeric(data$lon)))
  }

  # Get latitude and longitude for each location
  results <- dplyr::bind_rows(lapply(locations, function(loc) {
    Sys.sleep(1)  # Adding a delay to respect the API rate limit
    get_lat_long_single(loc)
  }))

  # Return the results
  return(results)
}

# Example usage:
locations <- c("New York", "Los Angeles", "Chicago")
lat_long_df <- get_lat_long(locations)
print(lat_long_df)
