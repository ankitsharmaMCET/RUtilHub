% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_regions.R
\name{transform_regions}
\alias{transform_regions}
\title{Transform Regional Data}
\usage{
transform_regions(
  map,
  existing_column_name,
  groups,
  replacements,
  default = "OTHER",
  view_transformed_map = FALSE
)
}
\arguments{
\item{map}{A spatial data frame (e.g., an `sf` object) containing regional information.}

\item{existing_column_name}{The name of the column in `map` that contains the regions to be transformed.}

\item{groups}{A list where each element is a vector of regions that should be replaced with a new category.}

\item{replacements}{A vector of replacement values corresponding to each group in `groups`. Each element in `replacements` specifies the new category for the regions listed in `groups`.}

\item{default}{A string specifying the default category for regions not included in any of the `groups`. Default is `"OTHER"`.}

\item{view_transformed_map}{A logical value indicating whether to visualize the transformed map using `ggplot2`. If `TRUE`, the function returns a list containing the transformed map and the plot. Default is `FALSE`.}
}
\value{
If `view_transformed_map` is `FALSE`, returns the modified spatial data frame with a new column containing the transformed region categories. If `view_transformed_map` is `TRUE`, returns a list with two elements: the transformed spatial data frame and a `ggplot` object visualizing the transformed map.
}
\description{
This function transforms a column in a spatial data frame based on specified groupings and replacements. It allows you to recategorize regions according to predefined groups and visualize the transformed map if desired.
}
\details{
The function creates a new column in the spatial data frame where each region is categorized according to the specified `groups` and `replacements`. Regions not included in any of the `groups` are assigned the `default` category. If `view_transformed_map` is `TRUE`, it uses `ggplot2` to plot the transformed data with a color scale based on the new categories.
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\seealso{
\code{\link[glue]{glue}}
}
