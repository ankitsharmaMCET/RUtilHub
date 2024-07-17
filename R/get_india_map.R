#' Get India Map Data or Plot
#'
#' Retrieves map data or plots for Indian administrative boundaries.
#'
#' @param shapefile_folder Path to the folder containing shapefiles. If `NULL`, an error is thrown.
#' @param fill Type of administrative boundary to retrieve ("state", "district", "subdistrict", etc.).
#' @param location_category Category of administrative division ("STATE", "DISTRICT", "SUBDISTRICT", etc.).
#' @param location_name Name of the administrative division to retrieve (Default: "INDIA").
#' @param action Action to perform: "plot_map" (default) or "get_map_data".
#' @return Depending on action, returns spatial data or plots it.
#' @details This function retrieves Indian administrative boundary data from shapefiles included in the specified folder.
#' @examples
#' \dontrun{
#' # Example usage to plot the map for a state
#' get_india_map(shapefile_folder = "path/to/shapefiles", fill = "state", location_category = "STATE", location_name = "karnataka", action = "plot_map")
#'
#' # Example usage to return the filtered data for a subdistrict
#' map_data <- get_india_map(shapefile_folder = "path/to/shapefiles", fill = "subdistrict", location_category = "STATE", location_name = "UTTARAKHAND", action = "get_map_data")
#' print(map_data)
#'
#' # Example usage to plot the map for the entire country
#' get_india_map(shapefile_folder = "path/to/shapefiles", fill = "STATE", location_category = "COUNTRY", location_name = "INDIA", action = "plot_map")
#' }
#' @seealso \code{\link[sf]{st_read}}, \code{\link[sf]{st_union}}
#' @importFrom sf st_read st_union
#' @import ggplot2
#' @export
get_india_map <- function(shapefile_folder = NULL, fill, location_category, location_name = "INDIA",
                          action = c("plot_map", "get_map_data")) {
  # Check if shapefile_folder is NULL
  if (is.null(shapefile_folder)) {
    stop("shapefile_folder cannot be NULL. Please provide a valid folder path containing shapefiles.")
  }

  # Match the action argument to ensure it's valid
  action <- match.arg(action)

  # Construct the full shapefile path
  shapefile_path <- paste0(shapefile_folder, "/", toupper(fill), "_BOUNDARY.shp")

  # Read the shapefile
  shapefile_data <- sf::st_read(shapefile_path)

  # Function to replace special characters in character columns
  replace_special_characters <- function(df) {
    df[] <- lapply(df, function(x) {
      if (is.character(x)) {
        x <- gsub(">", "A", x)
        x <- gsub("@", "U", x)
        x <- gsub("\\|", "I", x)  # Need to escape '|' with '\\'
        x <- toupper(x)
        x
      } else {
        x
      }
    })
    return(df)
  }

  # Replace special characters in the shapefile data
  shapefile_data <- replace_special_characters(shapefile_data)

  # Add a new column named 'COUNTRY' and set its value to 'INDIA'
  shapefile_data$COUNTRY <- "INDIA"

  # Rename district column if fill is "DISTRICT"
  if (toupper(fill) == "DISTRICT") {
    colnames(shapefile_data)[colnames(shapefile_data) == "District"] <- "DISTRICT"
  }

  # Ensure the location_category column exists in the data
  if (!(location_category %in% names(shapefile_data))) {
    stop("The specified location category does not exist in the shapefile data.")
  }

  # Filter the shapefile data based on the location category and location name
  if (toupper(fill) == "COUNTRY" && toupper(location_category) == "COUNTRY") {
    # Aggregate all state polygons into a single polygon
    PLOT_DATA <- sf::st_union(shapefile_data)
  } else {
    PLOT_DATA <- shapefile_data[shapefile_data[[location_category]] == toupper(location_name), ]
  }

  # Check if any data is available after filtering
  if (nrow(PLOT_DATA) == 0 && !is.null(location_name)) {
    stop("No data found for the specified location name in the given category.")
  }

  # Return the plot data if action is "get_map_data"
  if (action == "get_map_data") {
    return(PLOT_DATA)
  }

  # Plot the filtered shapefile data if action is "plot_map"
  if (action == "plot_map") {
    ggplot(data = PLOT_DATA) +
      geom_sf(fill = "lightblue", color = ifelse(toupper(fill) == "COUNTRY", "NA", "darkblue")) +
      theme_minimal() +
      labs(title = paste("Map of", ifelse(toupper(fill) == "COUNTRY", "INDIA", location_name), "(", fill, ")"),
           caption = "Source: Administrative Boundary Database") +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            plot.caption = element_text(hjust = 0.5, size = 10),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
  }
}

# # Example usage to plot the map for a state
# get_india_map( shapefile_folder =shape,fill = "state", location_category = "STATE", location_name = "karnataka", action = "plot_map")
#
# # Example usage to return the filtered data for a subdistrict
# map_data <- get_india_map(shapefile_folder = "path/to/shapefiles", fill = "subdistrict", location_category = "STATE", location_name = "UTTARAKHAND", action = "get_map_data")
# print(map_data)
#
# # Example usage to plot the map for the entire country
# get_india_map(shapefile_folder = shapefile_folder, fill = "STATE", location_category = "COUNTRY", location_name = "INDIA", action = "plot_map")
