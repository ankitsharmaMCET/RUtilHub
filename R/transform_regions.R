#' @title Transform Regional Data
#' @description This function transforms a column in a spatial data frame based on specified groupings and replacements. It allows you to recategorize regions according to predefined groups and visualize the transformed map if desired.
#' @param map A spatial data frame (e.g., an `sf` object) containing regional information.
#' @param existing_column_name The name of the column in `map` that contains the regions to be transformed.
#' @param groups A list where each element is a vector of regions that should be replaced with a new category.
#' @param replacements A vector of replacement values corresponding to each group in `groups`. Each element in `replacements` specifies the new category for the regions listed in `groups`.
#' @param default A string specifying the default category for regions not included in any of the `groups`. Default is `"OTHER"`.
#' @param view_transformed_map A logical value indicating whether to visualize the transformed map using `ggplot2`. If `TRUE`, the function returns a list containing the transformed map and the plot. Default is `FALSE`.
#' @return If `view_transformed_map` is `FALSE`, returns the modified spatial data frame with a new column containing the transformed region categories. If `view_transformed_map` is `TRUE`, returns a list with two elements: the transformed spatial data frame and a `ggplot` object visualizing the transformed map.
#' @details The function creates a new column in the spatial data frame where each region is categorized according to the specified `groups` and `replacements`. Regions not included in any of the `groups` are assigned the `default` category. If `view_transformed_map` is `TRUE`, it uses `ggplot2` to plot the transformed data with a color scale based on the new categories.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#' @rdname transform_regions
#' @export
#' @importFrom glue glue
#' @import ggplot2
transform_regions <- function(map, existing_column_name, groups, replacements, default = "OTHER", view_transformed_map = FALSE) {

  # Initialize a vector to store results
  new_column_values <- rep(default, nrow(map))
  nreg <- length(replacements)

  # Loop through each group and replacement
  for (i in seq_along(groups)) {
    new_column_values[map[[existing_column_name]] %in% groups[[i]]] <- replacements[[i]]
  }

  # Add the new column to the data frame
  new_column_name <- glue::glue("reg{nreg}")
  map[[new_column_name]] <- new_column_values

  # View transformed map if requested
  if (view_transformed_map) {
    view_map <- ggplot(map) +
      geom_sf(aes_string(fill = new_column_name)) +
      #scale_fill_viridis_d(option = "H", na.value = "grey") +
      scale_fill_brewer(palette = "Set3", na.value = "grey")+
      theme_bw()

    return(list(map = map, view_map = view_map))
  }

  return(map)
}
